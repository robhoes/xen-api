module D = Debug.Make (struct let name = "xenapi_grpc_test" end)

open D

open Xenapi_grpc

let main port =
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_any, port) in
  let domain = Unix.domain_of_sockaddr sockaddr in
  let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
  let grpc_server = server () in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock sockaddr;
  Unix.listen sock 5;
  while true do
    let s, caller = Unix.accept ~cloexec:true sock in
    debug "Accepted connection";
    connection_handler grpc_server caller s;
    debug "Dispatched connection handler"
  done

let () =
  let port = ref 8080 in
  Arg.parse
    [ ("-p", Arg.Set_int port, " Listening port number (8080 by default)") ]
    ignore "gRPC server";
  main !port
