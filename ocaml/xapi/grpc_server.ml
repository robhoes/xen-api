module D = Debug.Make (struct let name = "grpc_server" end)

open D

let handler req fd reqd =
  debug "HI" ;
  Http_svr.response2 reqd `OK [] ""