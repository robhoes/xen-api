module D = Debug.Make (struct let name = "http-client2" end)

open D

(* adapted from Http *)

let of_method = function
  | Http.Get ->
      `GET
  | Post ->
      `POST
  | Put ->
      `PUT
  | Connect ->
      `CONNECT
  | Options ->
      `OPTIONS
  | Unknown x ->
      failwith ("Unknown " ^ x)

(* Encode @param suitably for appearing in a query parameter in a URL. *)
let urlencode param = Uri.pct_encode ~component:`Query param

let string_of_authorization = function
  | Http.UnknownAuth x ->
      x
  | Basic (username, password) ->
      "Basic " ^ Base64.encode_string (username ^ ":" ^ password)

let kvpairs x =
  String.concat "&"
    (List.map (fun (k, v) -> urlencode k ^ "=" ^ urlencode v) x)

let to_header_list req =
  let open Http.Request in
  let cookie =
    if req.cookie = [] then [] else [Http.Hdr.cookie, kvpairs req.cookie]
  in
  let transfer_encoding =
    Option.fold ~none:[]
      ~some:(fun req -> [Http.Hdr.transfer_encoding, req])
      req.transfer_encoding
  in
  let accept =
    Option.fold ~none:[] ~some:(fun req -> [Http.Hdr.accept, req]) req.accept
  in
  let content_length =
    Option.fold ~none:[]
      ~some:(fun req -> [Http.Hdr.content_length, Printf.sprintf "%Ld" req])
      req.content_length
  in
  let auth =
    Option.fold ~none:[]
      ~some:(fun req -> [Http.Hdr.authorization, string_of_authorization req])
      req.auth
  in
  let task =
    Option.fold ~none:[] ~some:(fun req -> [Http.Hdr.task_id, req]) req.task
  in
  let subtask_of =
    Option.fold ~none:[]
      ~some:(fun req -> [Http.Hdr.subtask_of, req])
      req.subtask_of
  in
  let content_type =
    Option.fold ~none:[]
      ~some:(fun req -> [Http.Hdr.content_type, req])
      req.content_type
  in
  let host =
    Option.fold ~none:[Http.Hdr.host, "host"] ~some:(fun req -> [Http.Hdr.host, req]) req.host
  in
  let user_agent =
    Option.fold ~none:[]
      ~some:(fun req -> [Http.Hdr.user_agent, req])
      req.user_agent
  in
  let close =
    [(Http.Hdr.connection, if req.close then "close" else "keep-alive")]
  in
  cookie
  @ transfer_encoding
  @ accept
  @ content_length
  @ auth
  @ task
  @ subtask_of
  @ content_type
  @ host
  @ user_agent
  @ close
  @ req.additional_headers

(* end from Http *)

type connection = H1_conn of Httpun_unix.Client.t | H2_conn of H2_unix.Client.t

type t =
  { mutable conn : connection
  }

module Http2 = struct
  open H2

  let settings = Config.(to_settings default) |> H2.Settings.to_base64

  let error_handler err =
    let err =
      match err with
      | `Malformed_response err -> Format.sprintf "Malformed response: %s" err
      | `Invalid_response_body_length _ -> "Invalid body length"
      | `Exn exn -> Format.sprintf "Exn raised: %s" (Printexc.to_string exn)
      | `Protocol_error _ -> "Protocol error"
    in
    error "Error handling HTTP/2 connection: %s\n" err

  let connect fd =
    H2_unix.Client.create_connection ~error_handler fd

  let disconnect conn =
    H2_unix.Client.shutdown conn

  let request_of_request req =
    let headers = Headers.of_list (to_header_list req) in
    let meth = of_method req.m in
    let query = if req.query = [] then "" else "?" ^ kvpairs req.query in
    Request.create ~headers ~scheme:"https" meth (req.path ^ query)

  let response_of_response resp body =
    let headers = resp.Response.headers in
    let additional_headers = List.filter
      (fun (n, _) -> List.mem n Http.Hdr.[content_length; task_id])
      (Headers.to_list headers)
    in
    {
      Http.Response.version="2"
    ; frame= false
    ; code= string_of_int (Status.to_code resp.Response.status)
    ; message= "" (* No response reason defined in HTTP/2 *)
    ; content_length= Option.map Int64.of_string (Headers.get headers Http.Hdr.content_length)
    ; task= Headers.get headers Http.Hdr.task_id
    ; additional_headers
    ; body
    }

  let read_body response response_body callback =
    let body = Buffer.create 1024 in
    let rec on_read buffer ~off ~len =
      debug "on_read" ;
      let fragment = Bytes.create len in
      Bigstringaf.blit_to_bytes buffer ~src_off:off fragment ~dst_off:0 ~len ;
      debug "on_read: %s" (Bytes.to_string fragment) ;
      Buffer.add_bytes body fragment ;
      Body.Reader.schedule_read response_body ~on_eof ~on_read
    and on_eof () =
      debug "EOF; calling back" ;
      let b = Buffer.contents body in
      debug "BODY: %s" b ; callback (response_of_response response (Some b))
    in
    debug "scheduling body ready" ;
    Body.Reader.schedule_read response_body ~on_eof ~on_read

  let handler callback response response_body =
    debug "Got response" ;
    match response with
    | { Response.status = `OK; _ } as response ->
      Format.fprintf Format.std_formatter "%a\n" Response.pp_hum response;
      read_body response response_body callback
    | response ->
      Format.fprintf Format.err_formatter "%a\n" Response.pp_hum response ;
      callback (response_of_response response None)

  let request_error_handler finish err =
    let err =
      match err with
      | `Malformed_response err -> Format.sprintf "Malformed response: %s" err
      | `Invalid_response_body_length _ -> "Invalid body length"
      | `Exn exn -> Format.sprintf "Exn raised: %s" (Printexc.to_string exn)
      | `Protocol_error _ -> "Protocol error"
    in
    error "Error handling HTTP/2 response: %s\n" err ;
    finish ()

  let do_request _t conn req f : unit =
    let m = Mutex.create () in
    let exit_cond = Condition.create () in
    let finished = ref false in
    let finish () =
      finished := true ;
      Condition.broadcast exit_cond
    in

    let response_handler =
      handler (fun response ->
        debug "EOF" ;
        Mutex.protect m finish ;
        let _ = f response in
        ()
      )
    in
    debug "doing HTTP/2 request" ;

    let request = request_of_request req in
    let request_body =
      H2_unix.Client.request
        ~error_handler:(request_error_handler finish)
        ~response_handler
        conn
        request
    in
    Option.iter (fun body ->
      debug "sending body......" ;
      Body.Writer.write_string request_body body
    ) req.body ;
    Body.Writer.close request_body ;

    debug "waiting for response......" ;
    Mutex.protect m (fun () ->
      while not !finished do
        Condition.wait exit_cond m
      done
    )

  let upgrade_hander t request callback =
    let { conn=(H1_conn {Httpun_unix.Client.runtime; _} | H2_conn {H2_unix.Client.runtime; _}) } = t in
    let { Httpun.Request.headers; meth; target; _ } = request in
    let connection = H2.Client_connection.create_h2c ~headers ~target ~meth
      ~error_handler
      (handler callback, request_error_handler (fun () -> ())) in
    let result = Result.map
      (fun connection ->
         (* Perform the runtime upgrade -- stop speaking HTTP/1.1, start
          * speaking HTTP/2 by feeding Gluten the `H2.Client_connection`
          * protocol. *)
         Gluten_unix.Client.upgrade
           runtime
           (Gluten.make (module H2.Client_connection) connection);
         { H2_unix.Client.connection; runtime })
      connection
    in
    (match result with
    | Ok connection ->
      debug "Connection state changed (HTTP/2 confirmed)\n%!";
      t.conn <- H2_conn connection
    | Error e ->
      error "Failed to upgrade connection to HTTP/2: %s\n%!" e;
      ()
    )
end

module Http1 = struct
  open Httpun

  let connect fd =
    Httpun_unix.Client.create_connection fd

  let disconnect conn =
    Httpun_unix.Client.shutdown conn

  let h2c_headers =
    [ Http.Hdr.connection, "Upgrade, HTTP2-Settings"
    ; "Upgrade", "h2c"
    ; "HTTP2-Settings", Result.get_ok Http2.settings
    ]

  let request_of_request req upgrade =
    let headers = to_header_list req in
    let headers = if upgrade then h2c_headers @ (List.remove_assoc Http.Hdr.connection headers) else headers in
    let headers = Headers.of_list headers in
    let meth = of_method req.m in
    let query = if req.query = [] then "" else "?" ^ kvpairs req.query in
    Request.create ~headers meth (req.path ^ query)

  let response_of_response resp body =
    let headers = resp.Response.headers in
    let additional_headers = List.filter
      (fun (n, _) -> List.mem n Http.Hdr.[content_length; task_id])
      (Headers.to_list headers)
    in
    {
      Http.Response.version="1.1"
    ; frame= false
    ; code= string_of_int (Status.to_code resp.Response.status)
    ; message= resp.Response.reason
    ; content_length= Option.map Int64.of_string (Headers.get headers Http.Hdr.content_length)
    ; task= Headers.get headers Http.Hdr.task_id
    ; additional_headers
    ; body
    }

  let read_body response response_body callback =
    let body = Buffer.create 1024 in
    let rec on_read buffer ~off ~len =
      debug "on_read" ;
      let fragment = Bytes.create len in
      Bigstringaf.blit_to_bytes buffer ~src_off:off fragment ~dst_off:0 ~len ;
      debug "on_read: %s" (Bytes.to_string fragment) ;
      Buffer.add_bytes body fragment ;
      Body.Reader.schedule_read response_body ~on_eof ~on_read
    and on_eof () =
      debug "EOF; calling back" ;
      let b = Buffer.contents body in
      debug "BODY: %s" b ; callback (response_of_response response (Some b))
    in
    debug "scheduling body ready" ;
    Body.Reader.schedule_read response_body ~on_eof ~on_read

  let handler t conn request callback response response_body =
    debug "Got response" ;
    match response with
    | { Response.status = `OK; _ } as response ->
      Format.fprintf Format.std_formatter "%a\n" Response.pp_hum response;
      read_body response response_body callback
    | { Response.status = `Switching_protocols; _ } as response ->
      debug "101 Switching protocols" ;
      Format.fprintf Format.std_formatter "%a\n\n%!" Response.pp_hum response ;
      Http2.upgrade_hander t request callback
    | response ->
      Format.fprintf Format.err_formatter "%a\n" Response.pp_hum response ;
      callback (response_of_response response None)

  let error_handler finish err =
    let err =
      match err with
      | `Malformed_response err -> Format.sprintf "Malformed response: %s" err
      | `Invalid_response_body_length _ -> "Invalid body length"
      | `Exn exn -> Format.sprintf "Exn raised: %s" (Printexc.to_string exn)
    in
    error "Error handling HTTP/1.x response: %s\n" err ;
    finish ()

  let do_request t conn req upgrade f : unit =
    let m = Mutex.create () in
    let exit_cond = Condition.create () in
    let finished = ref false in
    let finish () =
      finished := true ;
      Condition.broadcast exit_cond
    in

    let request = request_of_request req upgrade in
    let response_handler =
      handler t conn request (fun response ->
        debug "EOF" ;
        Mutex.protect m finish ;
        let _ = f response in
        ()
      )
    in
    debug "doing HTTP/1.1 request" ;

    let request_body =
      Httpun_unix.Client.request
        ~error_handler:(error_handler finish)
        ~response_handler
        conn
        request
    in
    Option.iter (fun body ->
      debug "sending body......" ;
      Body.Writer.write_string request_body body
    ) req.body ;
    Body.Writer.close request_body ;

    debug "waiting for response......" ;
    Mutex.protect m (fun () ->
      while not !finished do
        Condition.wait exit_cond m
      done
    )
end

let connect fd : t =
  { conn= H1_conn (Http1.connect fd) }

let disconnect t : unit =
  match t.conn with
  | H1_conn conn -> Http1.disconnect conn
  | H2_conn conn -> Http2.disconnect conn

let with_connection fd f =
  let connection = connect fd in
  debug "set up httpun connection" ;
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () -> f connection)
    (fun () ->
      debug "shutting down connection" ;
      disconnect connection
    )

(** [rpc request f] marshals the HTTP request represented by [request]
    and then parses the response. On success, [f] is called with an HTTP response record.
    On failure an exception is thrown. *)
let rpc t request (f : Http.Response.t -> 'a) =
  match t.conn with
  | H1_conn conn -> Http1.do_request t conn request true f
  | H2_conn conn -> Http2.do_request t conn request f
