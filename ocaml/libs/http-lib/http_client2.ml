module D = Debug.Make (struct let name = "http-client2" end)

open D

open Httpun

let handler ~on_eof response response_body =
  debug "Got response" ;
  match response with
  | { Response.status = `OK; _ } as response ->
    Format.fprintf Format.std_formatter "%a\n" Response.pp_hum response;
    let rec on_read bs ~off ~len =
      Bigstringaf.substring ~off ~len bs |> print_endline;
      flush stdout;
      Body.Reader.schedule_read response_body ~on_read ~on_eof
    in
    Body.Reader.schedule_read response_body ~on_read ~on_eof
  | response ->
    Format.fprintf Format.err_formatter "%a\n" Response.pp_hum response ;
    on_eof ()

let error_handler finish error =
  let error =
    match error with
    | `Malformed_response err -> Format.sprintf "Malformed response: %s" err
    | `Invalid_response_body_length _ -> "Invalid body length"
    | `Exn exn -> Format.sprintf "Exn raised: %s" (Printexc.to_string exn)
  in
  Format.eprintf "Error handling response: %s\n" error ;
  finish ()

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

let request_of_request req =
  let host =
    Option.fold ~none:[] ~some:(fun x -> ["host", x]) req.Http.Request.host
  in
  let content_length =
    Option.fold ~none:[] ~some:(fun l -> [ "content-length", Int64.to_string l])
    req.content_length in
  let headers = Headers.of_list (host @ content_length) in
  let meth = of_method req.m in
  Request.create ~headers meth req.path

let do_request fd req =
  let connection = Httpun_unix.Client.create_connection fd in
  debug "set up httpun connection" ;
  let m = Mutex.create () in
  let exit_cond = Condition.create () in
  let finished = ref false in
  let finish () =
    finished := true ;
    Condition.broadcast exit_cond
  in
  let response_handler =
    handler ~on_eof:(fun () ->
      debug "EOF" ;
      Mutex.protect m finish)
  in
  debug "doing request" ;

  let request = request_of_request req in
  let request_body =
    Httpun_unix.Client.request
      ~error_handler:(error_handler finish)
      ~response_handler
      connection
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
  );
  debug "shutting down connection" ;
  Httpun_unix.Client.shutdown connection


(** [rpc request f] marshals the HTTP request represented by [request] and [body]
    and then parses the response. On success, [f] is called with an HTTP response record.
    On failure an exception is thrown. *)
(*
let rpc ?(_use_fastpath = false) (fd : Unix.file_descr) request f =
  http_rpc_send_query fd request ;
  f (http_rpc_recv_response use_fastpath (Http.Request.to_string request) fd) fd
*)