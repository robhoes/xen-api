(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
(* A very very simple HTTP server! *)

(*
 * Notes:
 *
 * HTTP CONNECT requests are not handled in the standard way! Normally, one
 * would issue a connect request like this:
 *
 *    CONNECT host.domain:port HTTP/1.0
 *
 * But we've got different proxies for different things, so we use the syntax
 *
 *    CONNECT /console?session_id=... HTTP/1.0
 *
 * So we're not exactly standards compliant :)
 *
 *)

open Http
module Unixext = Xapi_stdext_unix.Unixext

(* This resolves the lowercase deprecation for all compiler versions *)
let lowercase = Astring.String.Ascii.lowercase

module D = Debug.Make (struct let name = "http" end)

open D

module E = Debug.Make (struct let name = "http_internal_errors" end)

let ( let* ) = Option.bind

let ( let@ ) f x = f x

type uri_path = string

module Stats = struct
  (** Record of statistics per-handler *)

  type t = {
      mutable n_requests: int  (** successful requests *)
    ; mutable n_connections: int  (** closed connections *)
    ; mutable n_framed: int  (** using the more efficient framed protocol *)
  }

  let empty () = {n_requests= 0; n_connections= 0; n_framed= 0}

  let update (x : t) (m : Mutex.t) req =
    Xapi_stdext_threads.Threadext.Mutex.execute m (fun () ->
        x.n_requests <- x.n_requests + 1 ;
        if req.Http.Request.close then x.n_connections <- x.n_connections + 1 ;
        if req.Http.Request.frame then x.n_framed <- x.n_framed + 1
    )
end

type reqd = H1_reqd of Httpun.Reqd.t | H2_reqd of H2.Reqd.t | No_reqd

(** Type of a function which can handle a Request.t *)
type handler = Http.Request.t -> Unix.file_descr -> reqd -> unit

(* try and do f (unit -> unit), ignore exceptions *)
let best_effort f = try f () with _ -> ()

let headers s headers =
  List.iter (fun h -> debug "Response header: %s" h) headers ;
  output_http s headers ;
  output_http s [""]

(* let response s hdrs length f =
   output_http s hdrs;
   output_http s [ Printf.sprintf "Content-Length: %Ld" length ];
   output_http s [ "" ];
   f s *)

(* If http/1.0 was requested, return that, else return http/1.1 *)
let get_return_version req =
  try
    let maj, min =
      Scanf.sscanf (Request.get_version req) "%d.%d" (fun a b -> (a, b))
    in
    match (maj, min) with 1, 0 -> "1.0" | _ -> "1.1"
  with _ -> "1.1"

let response_of_request req hdrs =
  let connection =
    ( Http.Hdr.connection
    , if req.Request.close then
        "close"
      else
        "keep-alive"
    )
  in
  let cache = (Http.Hdr.cache_control, "no-cache, no-store") in
  Http.Response.make ~version:(get_return_version req)
    ~frame:req.Http.Request.frame
    ~headers:(connection :: cache :: hdrs)
    "200" "OK"

module Helper = struct
  include Tracing.Propagator.Make (struct
    include Tracing_propagator.Propagator.Http

    let name_span req = req.Http.Request.path
  end)
end

let response_fct req ?(hdrs = []) s (response_length : int64)
    (write_response_to_fd_fn : Unix.file_descr -> unit) =
  let@ req = Helper.with_tracing ~name:__FUNCTION__ req in
  let res =
    {
      (response_of_request req hdrs) with
      Http.Response.content_length= Some response_length
    }
  in
  D.debug "Response %s" (Http.Response.to_string res) ;
  Unixext.really_write_string s (Http.Response.to_wire_string res) ;
  write_response_to_fd_fn s

let response_str req ?hdrs s body =
  let length = String.length body in
  response_fct req ?hdrs s (Int64.of_int length) (fun s ->
      Unixext.really_write_string s body
  )

type status_code = [`OK | `Not_found | `Unauthorized]

type send_headers = (string * string) list -> unit

let response2_h1 reqd status headers s =
  let open Httpun in
  let headers =
    headers |> Headers.of_list |> fun h ->
    Headers.add_unless_exists h Http.Hdr.content_type "text/xml" |> fun h ->
    if s <> "" then
      Headers.add_unless_exists h Http.Hdr.content_length
        (String.length s |> string_of_int)
    else
      h
  in
  let response = Response.create ~headers status in
  D.debug "Response %s" (Format.asprintf "%a" Response.pp_hum response) ;
  Reqd.respond_with_string reqd response s

let response2_h2 reqd status headers s =
  let open H2 in
  let headers =
    headers |> Headers.of_list |> fun h ->
    Headers.add_unless_exists h Http.Hdr.content_type "text/xml" |> fun h ->
    if s <> "" then
      Headers.add_unless_exists h Http.Hdr.content_length
        (String.length s |> string_of_int)
    else
      h
  in
  let response = Response.create ~headers status in
  D.debug "Response %s" (Format.asprintf "%a" Response.pp_hum response) ;
  Reqd.respond_with_string reqd response s

let response2 reqd (status : status_code) headers s =
  match reqd with
  | H1_reqd r ->
      response2_h1 r (status : status_code :> Httpun.Status.t) headers s
  | H2_reqd r ->
      response2_h2 r (status : status_code :> H2.Status.t) headers s
  | No_reqd ->
      ()

let response_missing2 ?(hdrs = []) reqd body =
  let connection = (Http.Hdr.connection, "close") in
  let cache = (Http.Hdr.cache_control, "no-cache, no-store") in
  let content_type = (Http.Hdr.content_type, "text/plain") in
  let headers = connection :: cache :: content_type :: hdrs in
  response2 reqd `Not_found headers body

let response_error_html2 reqd (status : [< status_code]) message hdrs body =
  let connection = (Http.Hdr.connection, "close") in
  let cache = (Http.Hdr.cache_control, "no-cache, no-store") in
  let content_type = (Http.Hdr.content_type, "text/html") in
  let headers = connection :: cache :: content_type :: hdrs in
  response2 reqd status headers body

let response_unauthorised2 reqd realm =
  let body =
    "<html><body><h1>HTTP 401 unauthorised</h1>Please check your credentials \
     and retry.</body></html>"
  in
  let realm = ("WWW-Authenticate", Printf.sprintf "Basic realm=\"%s\"" realm) in
  response_error_html2 reqd `Unauthorized "Unauthorised" [realm] body

let response_missing ?(hdrs = []) s body =
  let connection = (Http.Hdr.connection, "close") in
  let cache = (Http.Hdr.cache_control, "no-cache, no-store") in
  let res =
    Http.Response.make ~version:"1.1"
      ~headers:(connection :: cache :: hdrs)
      ~body "404" "Not Found"
  in
  D.debug "Response %s" (Http.Response.to_string res) ;
  Unixext.really_write_string s (Http.Response.to_wire_string res)

let response_error_html ?(version = "1.1") s code message hdrs body =
  let connection = (Http.Hdr.connection, "close") in
  let cache = (Http.Hdr.cache_control, "no-cache, no-store") in
  let content_type = (Http.Hdr.content_type, "text/html") in
  let res =
    Http.Response.make ~version
      ~headers:(content_type :: connection :: cache :: hdrs)
      ~body code message
  in
  D.debug "Response %s" (Http.Response.to_string res) ;
  Unixext.really_write_string s (Http.Response.to_wire_string res)

let response_custom_error ?req s error_code reason body =
  let version = Option.map get_return_version req in
  response_error_html ?version s error_code reason [] body

let response_unauthorised ?req label s =
  let version = Option.map get_return_version req in
  let body =
    "<html><body><h1>HTTP 401 unauthorised</h1>Please check your credentials \
     and retry.</body></html>"
  in
  let realm = ("WWW-Authenticate", Printf.sprintf "Basic realm=\"%s\"" label) in
  response_error_html ?version s "401" "Unauthorised" [realm] body

let response_forbidden ?req s =
  let version = Option.map get_return_version req in
  let body =
    "<html><body><h1>HTTP 403 forbidden</h1>Access to the requested resource \
     is forbidden.</body></html>"
  in
  response_error_html ?version s "403" "Forbidden" [] body

let response_badrequest ?req s =
  let version = Option.map get_return_version req in
  let body =
    "<html><body><h1>HTTP 400 bad request</h1>The HTTP request was malformed. \
     Please correct and retry.</body></html>"
  in
  response_error_html ?version s "400" "Bad Request" [] body

let response_request_timeout s =
  let body =
    "<html><body><h1>HTTP 408 request timeout</h1>Timed out waiting for the \
     request.</body></html>"
  in
  response_error_html s "408" "Request Timeout" [] body

let response_request_header_fields_too_large s =
  let body =
    "<html><body><h1>HTTP 431 request header fields too large</h1>Exceeded the \
     maximum header size.</body></html>"
  in
  response_error_html s "431" "Request Header Fields Too Large" [] body

let response_internal_error ?req ?extra exc s =
  Backtrace.is_important exc ;
  E.error "Responding with 500 Internal Error due to %s" (Printexc.to_string exc) ;
  E.log_backtrace exc ;
  let version = Option.map get_return_version req in
  let extra =
    Option.fold ~none:""
      ~some:(fun x -> "<h1> Additional information </h1>" ^ x)
      extra
  in
  let body =
    "<html><body><h1>HTTP 500 internal server error</h1>An unexpected error \
     occurred; please wait a while and try again. If the problem persists, \
     please contact your support representative."
    ^ extra
    ^ "</body></html>"
  in
  response_error_html ?version s "500" "Internal Error" [] body

let response_method_not_implemented ?req s =
  let version = Option.map get_return_version req in
  let extra =
    Option.fold ~none:""
      ~some:(fun req ->
        Printf.sprintf "<p>%s not supported.<br /></p>"
          (Http.string_of_method_t req.Http.Request.m)
      )
      req
  in
  let body =
    "<html><body><h1>HTTP 501 Method Not Implemented</h1>"
    ^ extra
    ^ "</body></html>"
  in
  response_error_html ?version s "501" "Method not implemented" [] body

let response_redirect ?req s dest =
  let version = Option.fold ~none:"1.1" ~some:get_return_version req in
  let location = (Http.Hdr.location, dest) in
  let res =
    Http.Response.make ~version ~headers:[location] ~body:"" "301"
      "Moved Permanently"
  in
  Unixext.really_write_string s (Http.Response.to_wire_string res)

let response_file ?mime_content_type ?download_name ~hsts_time s send_headers
    file =
  let size = (Unix.LargeFile.stat file).Unix.LargeFile.st_size in
  let keep_alive = [(Http.Hdr.connection, "keep-alive")] in
  let hsts_header =
    if hsts_time < 0 then
      []
    else
      let max_age = "max-age=" ^ string_of_int hsts_time in
      [(Http.Hdr.hsts, max_age)]
  in
  let mime_header =
    Option.fold ~none:[]
      ~some:(fun ty -> [(Hdr.content_type, ty)])
      mime_content_type
  in
  let content_disposition =
    let hdr = Hdr.content_disposition in
    let typ = "attachment" in
    Option.fold ~none:[]
      ~some:(fun name -> [(hdr, Printf.sprintf {|%s; filename="%s"|} typ name)])
      download_name
  in
  let content_length = [(Http.Hdr.content_length, Printf.sprintf "%Ld" size)] in
  send_headers
    (List.concat
       [
         keep_alive
       ; hsts_header
       ; mime_header
       ; content_disposition
       ; content_length
       ]
    ) ;
  Unixext.with_file file [Unix.O_RDONLY] 0 (fun f ->
      let (_ : int64) = Unixext.copy_file f s in
      ()
  )

let respond_to_options req s =
  let access_control_allow_headers =
    try
      let acrh = List.assoc Hdr.acrh req.Request.additional_headers in
      Printf.sprintf "%s, X-Requested-With" acrh
    with Not_found -> "X-Requested-With"
  in
  response_fct req
    ~hdrs:
      [
        ("Access-Control-Allow-Origin", "*")
      ; ("Access-Control-Allow-Headers", access_control_allow_headers)
      ; ("Access-Control-Allow-Methods", "PUT")
      ]
    s 0L
    (fun _ -> ())

(** If no handler matches the request then call this callback *)
let default_callback req fd _ =
  response_forbidden fd ;
  req.Request.close <- true

module TE = struct
  type t = {stats: Stats.t; stats_m: Mutex.t; handler: handler}

  let empty () =
    {stats= Stats.empty (); stats_m= Mutex.create (); handler= default_callback}
end

module MethodMap = Map.Make (struct
  type t = Http.method_t

  let compare = compare
end)

module Server = struct
  type t = {mutable handlers: TE.t Radix_tree.t MethodMap.t}

  let empty () = {handlers= MethodMap.empty}

  let add_handler x ty path handler =
    let existing =
      Option.value (MethodMap.find_opt ty x.handlers) ~default:Radix_tree.empty
    in
    x.handlers <-
      MethodMap.add ty
        (Radix_tree.insert path {(TE.empty ()) with TE.handler} existing)
        x.handlers

  let find_stats x m uri =
    let* rt = MethodMap.find_opt m x.handlers in
    let* te = Radix_tree.longest_prefix uri rt in
    Some te.TE.stats

  let all_stats x =
    let open Radix_tree in
    MethodMap.fold
      (fun m rt acc -> fold (fun k te acc -> (m, k, te.TE.stats) :: acc) acc rt)
      x.handlers []
end

let escape str =
  (* from xapi-stdext-std xstringext *)
  let escaped ~rules string =
    let aux h t =
      ( if List.mem_assoc h rules then
          List.assoc h rules
        else
          Astring.String.of_char h
      )
      :: t
    in
    String.concat "" (Astring.String.fold_right aux string [])
  in
  escaped
    ~rules:
      [
        ('<', "&lt;")
      ; ('>', "&gt;")
      ; ('\'', "&apos;")
      ; ('"', "&quot;")
      ; ('&', "&amp;")
      ]
    str

exception Generic_error of string

(** [read_request_exn fd] reads a single Http.req from [fd] and returns it. On error
    	it simply throws an exception and doesn't touch the output stream. *)
let read_request_exn ~proxy_seen ~read_timeout ~total_timeout ~max_length fd =
  let frame, headers, proxy' =
    Http.read_http_request_header ~read_timeout ~total_timeout ~max_length fd
  in
  let proxy = match proxy' with None -> proxy_seen | x -> x in
  let additional_headers =
    proxy |> Option.fold ~none:[] ~some:(fun p -> [("STUNNEL_PROXY", p)])
  in
  let open Http.Request in
  let kvlist_flatten ls =
    (* Uri.query splits the value string into several if they are separated
       with commas. Like this: "?k=v1,v2,v3" -> [("k", ["v1";"v2";"v3"])]
       This function concatenates these back. It will not concatenate values
       entered for duplicate keys, as these will be separate tuples:
       "?k=v1,v2,v3&k=v4" ->  [("k", ["v1"; "v2"; "v3"]); ("k", ["v4"])] *)
    List.map (fun (k, vs) -> (k, Astring.String.concat ~sep:"," vs)) ls
  in
  let request =
    Astring.String.cuts ~sep:"\n" headers
    |> List.fold_left
         (fun (status, req) header ->
           if not status then
             match Astring.String.fields ~empty:false header with
             | [meth; uri; version] ->
                 (* Request-Line   = Method SP Request-URI SP HTTP-Version CRLF *)
                 let uri_t = Uri.of_string uri in
                 if uri_t = Uri.empty then raise Http_parse_failure ;
                 let path = Uri.path_unencoded uri_t in
                 let query = Uri.query uri_t |> kvlist_flatten in
                 let m = Http.method_t_of_string meth in
                 let version =
                   let x = String.trim version in
                   let prefix = "HTTP/" in
                   String.sub x (String.length prefix)
                     (String.length x - String.length prefix)
                 in
                 let close = version = "1.0" in
                 (true, {req with m; path; query; version; close})
             | _ ->
                 raise Http_parse_failure
           else
             match Astring.String.cut ~sep:":" header with
             | Some (k, v) -> (
                 let k = lowercase k in
                 let v = String.trim v in
                 ( true
                 , match k with
                   | k when k = Http.Hdr.content_length ->
                       {req with content_length= Some (Int64.of_string v)}
                   | k when k = Http.Hdr.cookie ->
                       {req with cookie= Http.parse_cookies v}
                   | k when k = Http.Hdr.transfer_encoding ->
                       {req with transfer_encoding= Some v}
                   | k when k = Http.Hdr.accept ->
                       {req with accept= Some v}
                   | k when k = Http.Hdr.authorization ->
                       {req with auth= Some (authorization_of_string v)}
                   | k when k = Http.Hdr.task_id ->
                       {req with task= Some v}
                   | k when k = Http.Hdr.subtask_of ->
                       {req with subtask_of= Some v}
                   | k when k = Http.Hdr.content_type ->
                       {req with content_type= Some v}
                   | k when k = Http.Hdr.host ->
                       {req with host= Some v}
                   | k when k = Http.Hdr.user_agent ->
                       {req with user_agent= Some v}
                   | k when k = Http.Hdr.connection && lowercase v = "close" ->
                       {req with close= true}
                   | k
                     when k = Http.Hdr.connection && lowercase v = "keep-alive"
                     ->
                       {req with close= false}
                   | _ ->
                       {
                         req with
                         additional_headers= (k, v) :: req.additional_headers
                       }
                 )
               )
             | None ->
                 (true, req)
           (* end of headers *)
         )
         (false, {empty with Http.Request.frame; additional_headers})
    |> snd
  in
  (request, proxy)

(** [read_request fd] returns [Some req] read from [fd], or [None]. If [None] it will have
    	already sent back a suitable error code and response to the client. *)
let read_request ?proxy_seen ~read_timeout ~total_timeout ~max_length fd =
  try
    (* TODO: Restore functionality of tracing this function. We rely on the request
       to contain information we want spans to inherit. However, it is the reading of the
       request that we intend to trace. *)
    let r, proxy =
      read_request_exn ~proxy_seen ~read_timeout ~total_timeout ~max_length fd
    in
    let trace_context = Tracing_propagator.Propagator.Http.extract_from r in
    let tracer = Tracing.Tracer.get_tracer ~name:"http_tracer" in
    let loop_span =
      match
        Tracing.Tracer.start ~tracer ~trace_context ~name:__FUNCTION__
          ~parent:None ()
      with
      | Ok span ->
          span
      | Error _ ->
          None
    in
    let parent_span = Helper.traceparent_of r in
    let loop_span =
      Option.fold ~none:None
        ~some:(fun span ->
          Tracing.Tracer.update_span_with_parent span parent_span
        )
        loop_span
    in
    let _ : (Tracing.Span.t option, exn) result =
      Tracing.Tracer.finish loop_span
    in
    (Some r, proxy)
  with e ->
    Backtrace.is_important e ;
    D.warn "%s (%s)" (Printexc.to_string e) __LOC__ ;
    best_effort (fun () ->
        match e with
        (* Specific errors thrown during parsing *)
        | Http.Http_parse_failure ->
            response_internal_error e fd
              ~extra:"The HTTP headers could not be parsed." ;
            debug "Error parsing HTTP headers"
        (* Connection terminated *)
        (* Generic errors thrown during parsing *)
        | End_of_file ->
            ()
        | Unix.Unix_error (Unix.EAGAIN, _, _) | Http.Timeout ->
            response_request_timeout fd
        | Http.Too_large ->
            response_request_header_fields_too_large fd
        (* Premature termination of connection! *)
        | Unix.Unix_error (a, b, c) ->
            response_internal_error e fd
              ~extra:
                (Printf.sprintf "Got UNIX error: %s %s %s"
                   (Unix.error_message a) b c
                )
        | exc ->
            response_internal_error exc fd
              ~extra:(escape (Printexc.to_string exc))
    ) ;
    (None, None)

let handle_one (x : Server.t) ss req =
  let@ req = Helper.with_tracing ~name:__FUNCTION__ req in
  let span = Helper.traceparent_of req in
  let finished = ref false in
  try
    D.debug "Request %s" (Http.Request.to_string req) ;
    let method_map =
      try MethodMap.find req.Request.m x.Server.handlers
      with Not_found -> raise Method_not_implemented
    in
    let empty = TE.empty () in
    let te =
      Option.value ~default:empty
        (Radix_tree.longest_prefix req.Request.path method_map)
    in
    let@ _ = Tracing.with_child_trace span ~name:"handler" in
    te.TE.handler req ss No_reqd ;
    finished := req.Request.close ;
    Stats.update te.TE.stats te.TE.stats_m req ;
    !finished
  with e ->
    finished := true ;
    best_effort (fun () ->
        match e with
        (* Specific errors thrown by handlers *)
        | Generic_error s ->
            response_internal_error e ~req ss ~extra:s
        | Http.Unauthorised realm ->
            response_unauthorised ~req realm ss
        | Http.Forbidden ->
            response_forbidden ~req ss
        (* Generic errors thrown by handlers *)
        | Http.Method_not_implemented ->
            response_method_not_implemented ~req ss
        | End_of_file ->
            ()
        (* Premature termination of connection! *)
        | Unix.Unix_error (a, b, c) ->
            response_internal_error ~req e ss
              ~extra:
                (Printf.sprintf "Got UNIX error: %s %s %s"
                   (Unix.error_message a) b c
                )
        | exc ->
            response_internal_error ~req exc ss
              ~extra:(escape (Printexc.to_string exc))
    ) ;
    !finished

let handle_connection ~header_read_timeout ~header_total_timeout
    ~max_header_length (x : Server.t) caller ss =
  ( match caller with
  | Unix.ADDR_UNIX _ ->
      debug "Accepted unix connection"
  | Unix.ADDR_INET (addr, port) ->
      debug "Accepted inet connection from %s:%d"
        (Unix.string_of_inet_addr addr)
        port
  ) ;
  (* For HTTPS requests, a PROXY header is sent by stunnel right at the beginning of
     of its connection to the server, before HTTP requests are transferred, and
     just once per connection. To allow for the PROXY metadata (including e.g. the
     client IP) to be added to all request records on a connection, it must be passed
     along in the loop below. *)
  let rec loop ~read_timeout ~total_timeout proxy_seen =
    (* 1. we must successfully parse a request *)
    let req, proxy =
      read_request ?proxy_seen ~read_timeout ~total_timeout
        ~max_length:max_header_length ss
    in

    Http.Request.with_originator_of req Tgroup.of_req_originator ;

    (* 2. now we attempt to process the request *)
    let finished = Option.fold ~none:true ~some:(handle_one x ss) req in
    (* 3. do it again if the connection is kept open, but without timeouts *)
    if not finished then loop ~read_timeout:None ~total_timeout:None proxy
  in
  loop ~read_timeout:header_read_timeout ~total_timeout:header_total_timeout
    None ;
  debug "Closing connection" ;
  Unix.close ss

let req_of_r version meth target headers_fold =
  let m =
    match meth with
    | `POST ->
        Http.Post
    | `GET ->
        Http.Get
    | `PUT ->
        Http.Put
    | `CONNECT ->
        Http.Connect
    | `OPTIONS ->
        Http.Options
    | `DELETE ->
        Http.Unknown "DELETE"
    | `HEAD ->
        Http.Unknown "HEAD"
    | `TRACE ->
        Http.Unknown "TRACE"
    | `Other m ->
        Http.Unknown m
  in
  let kvlist_flatten ls =
    (* Uri.query splits the value string into several if they are separated
       with commas. Like this: "?k=v1,v2,v3" -> [("k", ["v1";"v2";"v3"])]
       This function concatenates these back. It will not concatenate values
       entered for duplicate keys, as these will be separate tuples:
       "?k=v1,v2,v3&k=v4" ->  [("k", ["v1"; "v2"; "v3"]); ("k", ["v4"])] *)
    List.map (fun (k, vs) -> (k, Astring.String.concat ~sep:"," vs)) ls
  in
  let uri_t = Uri.of_string target in
  if uri_t = Uri.empty then raise Http_parse_failure ;
  let path = Uri.path_unencoded uri_t in
  let query = Uri.query uri_t |> kvlist_flatten in
  let close = false in
  let req = {Http.Request.empty with m; path; query; version; close} in
  headers_fold
    ~f:(fun k v (req : Http.Request.t) ->
      let k = lowercase k in
      let v = String.trim v in
      debug "HEADER: %s: %s" k v ;
      match k with
      | k when k = Http.Hdr.content_length ->
          {req with content_length= Some (Int64.of_string v)}
      | k when k = Http.Hdr.cookie ->
          {req with cookie= Http.parse_cookies v}
      | k when k = Http.Hdr.transfer_encoding ->
          {req with transfer_encoding= Some v}
      | k when k = Http.Hdr.accept ->
          {req with accept= Some v}
      | k when k = Http.Hdr.authorization ->
          {req with auth= Some (authorization_of_string v)}
      | k when k = Http.Hdr.task_id ->
          {req with task= Some v}
      | k when k = Http.Hdr.subtask_of ->
          {req with subtask_of= Some v}
      | k when k = Http.Hdr.content_type ->
          {req with content_type= Some v}
      | k when k = Http.Hdr.host ->
          {req with host= Some v}
      | k when k = Http.Hdr.user_agent ->
          {req with user_agent= Some v}
      | k when k = Http.Hdr.connection && lowercase v = "close" ->
          {req with close= true}
      | k when k = Http.Hdr.connection && lowercase v = "keep-alive" ->
          {req with close= false}
      | _ ->
          {req with additional_headers= (k, v) :: req.additional_headers}
    )
    ~init:req

let route x req ss rd =
  let method_map =
    try MethodMap.find req.Http.Request.m x.Server.handlers
    with Not_found -> raise Method_not_implemented
  in
  let empty = TE.empty () in
  let te =
    Option.value ~default:empty
      (Radix_tree.longest_prefix req.Http.Request.path method_map)
  in
  Stats.update te.TE.stats te.TE.stats_m req ;
  try te.TE.handler req ss rd with
  | Http.Unauthorised realm ->
      response_unauthorised2 rd realm
  | _ ->
      ()

module Http2 = struct
  open H2

  let connection_handler :
         Server.t
      -> Unix.file_descr
      -> Httpun.Request.t
      -> Bigstringaf.t H2.IOVec.t list
      -> (Server_connection.t, string) result =
    let error_handler ?request:_ error start_response =
      let response_body = start_response Headers.empty in
      ( match error with
      | `Exn exn ->
          Body.Writer.write_string response_body (Printexc.to_string exn) ;
          Body.Writer.write_string response_body "\n"
      | #Status.standard as error ->
          Body.Writer.write_string response_body
            (Status.default_reason_phrase error)
      ) ;
      Body.Writer.close response_body
    in
    let request_handler x ss : H2.Server_connection.request_handler =
     fun reqd ->
      let open H2 in
      let req =
        let r = Reqd.request reqd in
        req_of_r "2" r.Request.meth r.Request.target
          (Headers.fold r.Request.headers)
      in
      D.debug "Request %s" (Http.Request.to_string req) ;
      route x req ss (H2_reqd reqd)
    in
    fun x ss http_request request_body ->
      let {Httpun.Request.headers; target; meth; _} = http_request in
      H2.Server_connection.create_h2c ?config:None ~headers ~target ~meth
        ~request_body ~error_handler (request_handler x ss)
end

let error_handler (_ : Unix.sockaddr) ?request:_ error start_response =
  let open Httpun in
  let response_body = start_response Headers.empty in
  ( match error with
  | `Exn exn ->
      Body.Writer.write_string response_body (Printexc.to_string exn) ;
      Body.Writer.write_string response_body "\n"
  | #Status.standard as error ->
      Body.Writer.write_string response_body (Status.default_reason_phrase error)
  ) ;
  Body.Writer.close response_body

let upgrade_handler x ss request body upgrade () =
  let connection =
    Stdlib.Result.get_ok (Http2.connection_handler x ss request body)
  in
  upgrade (Gluten.make (module H2.Server_connection) connection)

let request_handler x ss _addr (reqd : Httpun.Reqd.t Gluten.reqd) =
  let open Httpun in
  let {Gluten.reqd; upgrade} = reqd in
  let request = Reqd.request reqd in

  match Headers.get request.Request.headers "Connection" with
  | Some "Upgrade, HTTP2-Settings" ->
      debug "HTTP1 -> 2 upgrade" ;
      let request_body = Reqd.request_body reqd in
      let body = ref [] in
      let rec on_read buffer ~off ~len =
        body := {H2.IOVec.buffer; off; len} :: !body ;
        Body.Reader.schedule_read request_body ~on_eof ~on_read
      and on_eof () =
        let headers =
          Headers.of_list [("Connection", "Upgrade"); ("Upgrade", "h2c")]
        in
        Reqd.respond_with_upgrade reqd headers
          (upgrade_handler x ss request !body upgrade)
      in
      debug "scheduling body ready" ;
      Body.Reader.schedule_read request_body ~on_eof ~on_read
  | _ ->
      debug "HTTP1 (no upgrade)" ;
      let req =
        let r = Reqd.request reqd in
        req_of_r "1.1" r.Request.meth r.Request.target
          (Headers.fold r.Request.headers)
      in
      D.debug "Request %s" (Http.Request.to_string req) ;
      route x req ss (H1_reqd reqd)

let handle_connection2 (x : Server.t) caller ss =
  ( match caller with
  | Unix.ADDR_UNIX _ ->
      debug "Accepted unix connection"
  | Unix.ADDR_INET (addr, port) ->
      debug "Accepted inet connection from %s:%d"
        (Unix.string_of_inet_addr addr)
        port
  ) ;
  Httpun_unix.Server.create_connection_handler
    ~request_handler:(request_handler x ss) ~error_handler caller ss

let bind ?(listen_backlog = 128) sockaddr name =
  let domain =
    match sockaddr with
    | Unix.ADDR_UNIX path ->
        debug "Establishing Unix domain server on path: %s" path ;
        Unix.PF_UNIX
    | Unix.ADDR_INET (_, _) ->
        debug "Establishing inet domain server" ;
        Unix.domain_of_sockaddr sockaddr
  in
  let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
  (* Make sure exceptions cause the socket to be closed *)
  try
    Unix.set_close_on_exec sock ;
    Unix.setsockopt sock Unix.SO_REUSEADDR true ;
    Unix.setsockopt sock Unix.SO_KEEPALIVE true ;
    ( match sockaddr with
    | Unix.ADDR_INET _ ->
        Unixext.set_tcp_nodelay sock true
    | _ ->
        ()
    ) ;
    Unix.bind sock sockaddr ;
    Unix.listen sock listen_backlog ;
    (sock, name)
  with e ->
    debug "Caught exception in Http_svr.bind (closing socket): %s"
      (Printexc.to_string e) ;
    Unix.close sock ;
    raise e

let bind_retry ?(listen_backlog = 128) sockaddr =
  let description =
    match sockaddr with
    | Unix.ADDR_INET (ip, port) ->
        Printf.sprintf "INET %s:%d" (Unix.string_of_inet_addr ip) port
    | Unix.ADDR_UNIX path ->
        Printf.sprintf "UNIX %s" path
  in
  (* Sometimes we see failures which we hope are transient. If this
     	   happens then we'll retry a couple of times before failing. *)
  let result = ref None in
  let start = Unix.gettimeofday () in
  let timeout = 30.0 in
  (* 30s *)
  while !result = None && Unix.gettimeofday () -. start < timeout do
    try result := Some (bind ~listen_backlog sockaddr description)
    with Unix.Unix_error (code, _, _) ->
      debug "While binding %s: %s" description (Unix.error_message code) ;
      Thread.delay 5.
  done ;
  match !result with
  | None ->
      failwith (Printf.sprintf "Repeatedly failed to bind: %s" description)
  | Some s ->
      info "Successfully bound socket to: %s" description ;
      s

(* Maps sockets to Server_io.server records *)
let socket_table = Hashtbl.create 10

type socket = Unix.file_descr * string

(* Start an HTTP server on a new socket *)
let start ?header_read_timeout ?header_total_timeout ?max_header_length
    ~conn_limit (x : Server.t) (socket, name) =
  let handler =
    {
      Server_io.name
    ; body=
        handle_connection ~header_read_timeout ~header_total_timeout
          ~max_header_length x
    ; lock= Semaphore.Counting.make conn_limit
    }
  in
  let server = Server_io.server ~by_thread:true handler socket in
  Hashtbl.add socket_table socket server

(* Start an HTTP server on a new socket *)
let start2 ~conn_limit (x : Server.t) (socket, name) =
  let handler =
    {
      Server_io.name
    ; body= handle_connection2 x
    ; lock= Semaphore.Counting.make conn_limit
    }
  in
  let server = Server_io.server ~by_thread:false handler socket in
  Hashtbl.add socket_table socket server

exception Socket_not_found

(* Stop an HTTP server running on a socket *)
let stop (socket, _name) =
  let server =
    match Hashtbl.find_opt socket_table socket with
    | Some x ->
        x
    | None ->
        raise Socket_not_found
  in
  Hashtbl.remove socket_table socket ;
  server.Server_io.shutdown ()

exception Client_requested_size_over_limit

(** Read the body of an HTTP request (requires a content-length: header). *)
let read_body ?limit req fd =
  match req.Request.content_length with
  | None ->
      failwith "We require a content-length: HTTP header"
  | Some length ->
      let length = Int64.to_int length in
      Option.fold ~none:()
        ~some:(fun l ->
          if length > l then raise Client_requested_size_over_limit
        )
        limit ;
      Unixext.really_read_string fd length

let read_body2_h1 reqd callback =
  let open Httpun in
  let request_body = Reqd.request_body reqd in
  let body = Buffer.create 1024 in
  let rec on_read buffer ~off ~len =
    debug "on_read" ;
    let fragment = Bytes.create len in
    Bigstringaf.blit_to_bytes buffer ~src_off:off fragment ~dst_off:0 ~len ;
    debug "on_read: %s" (Bytes.to_string fragment) ;
    Buffer.add_bytes body fragment ;
    Body.Reader.schedule_read request_body ~on_eof ~on_read
  and on_eof () =
    debug "EOF; calling back" ;
    let b = Buffer.contents body in
    debug "BODY: %s" b ; callback b
  in
  debug "scheduling body ready" ;
  Body.Reader.schedule_read request_body ~on_eof ~on_read

let read_body2_h2 reqd callback =
  let open H2 in
  let request_body = Reqd.request_body reqd in
  let body = Buffer.create 1024 in
  let rec on_read buffer ~off ~len =
    debug "on_read" ;
    let fragment = Bytes.create len in
    Bigstringaf.blit_to_bytes buffer ~src_off:off fragment ~dst_off:0 ~len ;
    debug "on_read: %s" (Bytes.to_string fragment) ;
    Buffer.add_bytes body fragment ;
    Body.Reader.schedule_read request_body ~on_eof ~on_read
  and on_eof () =
    debug "EOF; calling back" ;
    let b = Buffer.contents body in
    debug "BODY: %s" b ; callback b
  in
  debug "scheduling body ready" ;
  Body.Reader.schedule_read request_body ~on_eof ~on_read

let read_body2 reqd callback =
  match reqd with
  | H1_reqd r ->
      read_body2_h1 r callback
  | H2_reqd r ->
      read_body2_h2 r callback
  | No_reqd ->
      ()

let read_body_to_pipe_h1 reqd s callback =
  let open Httpun in
  match Reqd.request reqd |> Request.body_length with
  | `Fixed 0L ->
      (* xapi "chunked" encoding: no content-length provided *)
      callback s
  | _ ->
      let request_body = Reqd.request_body reqd in
      let fd_out, fd_in = Unix.pipe () in
      let t =
        Thread.create
          (fun () ->
            debug "calling back with pipe" ;
            callback fd_out ;
            debug "callback finished" ;
            Unix.close fd_out
          )
          ()
      in
      let rec on_read buffer ~off ~len =
        debug "on_read" ;
        let fragment = Bytes.create len in
        Bigstringaf.blit_to_bytes buffer ~src_off:off fragment ~dst_off:0 ~len ;
        debug "on_read: %s" (Bytes.to_string fragment) ;
        let _ : int = Unix.write_bigarray fd_in buffer off len in
        Body.Reader.schedule_read request_body ~on_eof ~on_read
      and on_eof () = debug "EOF" ; Unix.close fd_in ; Thread.join t in
      debug "scheduling body read" ;
      Body.Reader.schedule_read request_body ~on_eof ~on_read

let read_body_to_pipe reqd s callback =
  match reqd with
  | H1_reqd r ->
      read_body_to_pipe_h1 r s callback
  | H2_reqd _r ->
      ()
  | No_reqd ->
      ()

let bufsize = 4096

let respond_with_pipe_h1 reqd callback =
  let open Httpun in
  let fd_out, fd_in = Unix.pipe () in
  let headers_ch = Event.new_channel () in
  let send_headers headers = Event.send headers_ch headers |> Event.sync in
  let t =
    Thread.create
      (fun () ->
        debug "respond_with_pipe_h1: calling back with pipe" ;
        callback fd_in send_headers ;
        debug "respond_with_pipe_h1: callback finished" ;
        Unix.close fd_in
      )
      ()
  in
  let headers' = Event.receive headers_ch |> Event.sync in
  let headers = Headers.of_list headers' in
  let response = Httpun.Response.create ~headers `OK in
  let writer =
    Httpun.Reqd.respond_with_streaming ~flush_headers_immediately:true reqd
      response
  in
  let b = Bigstringaf.create bufsize in
  let rec loop () =
    debug "respond_with_pipe_h1: read from pipe" ;
    let n = Unix.read_bigarray fd_out b 0 bufsize in
    if n > 0 then (
      debug "respond_with_pipe_h1: write %d bytes to stream" n ;
      Body.Writer.write_bigstring writer ~len:n b ;
      loop ()
    ) else (
      debug "respond_with_pipe_h1: finished" ;
      Thread.join t ;
      Body.Writer.flush writer (function
        | `Written ->
            debug "respond_with_pipe_h1: flush written"
        | `Closed ->
            debug "respond_with_pipe_h1: flush closed"
        ) ;
      Body.Writer.close writer ;
      Unix.close fd_out
    )
  in
  loop ()

let respond_with_pipe reqd callback =
  match reqd with
  | H1_reqd r ->
      respond_with_pipe_h1 r callback
  | H2_reqd _r ->
      ()
  | No_reqd ->
      ()

(* Helpers to determine the client of a call *)

type protocol = Https | Http

let string_of_protocol = function Https -> "HTTPS" | Http -> "HTTP"

type client = protocol * Ipaddr.t

let clean_addr_of_string ip =
  (* in the IPv4 case, users should see 127.0.0.1 rather than ::ffff:127.0.0.1 *)
  let ipv4_affix = "::ffff:" in
  ( if Astring.String.is_prefix ~affix:ipv4_affix ip then
      Astring.String.drop ~max:(String.length ipv4_affix) ip
    else
      ip
  )
  |> Ipaddr.of_string
  |> Stdlib.Result.to_option

let https_client_of_req req =
  (* this relies on 'protocol = proxy' in Xapi_stunnel_server *)
  let stunnel_proxy =
    List.assoc_opt "STUNNEL_PROXY" req.Http.Request.additional_headers
  in
  Option.bind stunnel_proxy (fun proxy ->
      try
        Scanf.sscanf proxy "TCP6 %s %s %d %d" (fun client _ _ _ -> client)
        |> clean_addr_of_string
      with _ ->
        error "Failed to parse STUNNEL_PROXY='%s'" proxy ;
        None
  )

let client_of_req_and_fd req fd =
  match https_client_of_req req with
  | Some client ->
      Some (Https, client)
  | None -> (
    match Unix.getpeername fd with
    | Unix.ADDR_INET (addr, _) ->
        addr
        |> Unix.string_of_inet_addr
        |> clean_addr_of_string
        |> Option.map (fun ip -> (Http, ip))
    | Unix.ADDR_UNIX _ | (exception _) ->
        None
  )

let string_of_client (protocol, ip) =
  Printf.sprintf "%s %s" (string_of_protocol protocol) (Ipaddr.to_string ip)
