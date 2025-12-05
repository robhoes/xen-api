module D = Debug.Make (struct let name = "http-client2" end)

open D

open Httpun

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
    Option.fold ~none:[] ~some:(fun req -> [Http.Hdr.host, req]) req.host
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

let request_of_request req =
  let headers = Headers.of_list (to_header_list req) in
  let meth = of_method req.m in
  let query = if req.query = [] then "" else "?" ^ kvpairs req.query in
  Request.create ~headers meth (req.path ^ query)

let read_body response_body callback =
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
    debug "BODY: %s" b ; callback (Some b)
  in
  debug "scheduling body ready" ;
  Body.Reader.schedule_read response_body ~on_eof ~on_read

let handler callback response response_body =
  debug "Got response" ;
  match response with
  | { Response.status = `OK; _ } as response ->
    Format.fprintf Format.std_formatter "%a\n" Response.pp_hum response;
    read_body response_body (callback response)
  | response ->
    Format.fprintf Format.err_formatter "%a\n" Response.pp_hum response ;
    callback response None

let error_handler finish error =
  let error =
    match error with
    | `Malformed_response err -> Format.sprintf "Malformed response: %s" err
    | `Invalid_response_body_length _ -> "Invalid body length"
    | `Exn exn -> Format.sprintf "Exn raised: %s" (Printexc.to_string exn)
  in
  Format.eprintf "Error handling response: %s\n" error ;
  finish ()

let with_connection fd f =
  let connection = Httpun_unix.Client.create_connection fd in
  debug "set up httpun connection" ;
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () -> f connection)
    (fun () ->
      debug "shutting down connection" ;
      Httpun_unix.Client.shutdown connection
    )

let do_request conn req f : unit =
  let m = Mutex.create () in
  let exit_cond = Condition.create () in
  let finished = ref false in
  let finish () =
    finished := true ;
    Condition.broadcast exit_cond
  in
  
  let response_handler =
    handler (fun resp body ->
      debug "EOF" ;
      let headers = resp.Response.headers in
      let additional_headers = List.filter
        (fun (n, _) -> List.mem n Http.Hdr.[content_length; task_id])
        (Headers.to_list headers)
      in
      let response =
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
      in
      Mutex.protect m finish ;
      let _ = f response in
      ()
    )
  in
  debug "doing request" ;

  let request = request_of_request req in
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

let rpc ?(_use_fastpath = false) (fd : Unix.file_descr) request (f : Http.Response.t -> 'a) =
  with_connection fd @@ fun conn ->
  do_request conn request f

let rpc ?(_use_fastpath = false) conn request (f : Http.Response.t -> 'a) =
  do_request conn request f


(** [rpc request f] marshals the HTTP request represented by [request] and [body]
    and then parses the response. On success, [f] is called with an HTTP response record.
    On failure an exception is thrown. *)
(*
let rpc ?(_use_fastpath = false) (fd : Unix.file_descr) request f =
  http_rpc_send_query fd request ;
  f (http_rpc_recv_response use_fastpath (Http.Request.to_string request) fd) fd
*)
