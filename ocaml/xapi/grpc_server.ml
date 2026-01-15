open Grpc_unix
open Xenapi_proto.Xenapi
open Ocaml_protoc_plugin

module D = Debug.Make (struct let name = "grpc_server" end)

open D

let grpc_server = ref None

module Network = struct
  let create (buffer : string) =
    debug "network.create" ;
    let decode, encode = Service.make_service_functions Network_class.create in
    (* Decode the request. *)
    let network_record =
      Reader.create buffer |> decode |> function
      | Ok v -> v
      | Error e ->
          failwith
            (Printf.sprintf "Could not decode request: %s" (Result.show_error e))
    in
    debug "network.create: received {uuid=%s}" network_record.Network.uuid;
    let r = "OpaqueRef:xyz-xyz" in
    debug "network.create: created network %s" r;
    (Grpc.Status.(v OK), Some (r |> encode |> Writer.contents))
end

let xapi_network_service () =
  Server.Service.(
    v () |> add_rpc ~name:"create" ~rpc:(Unary Network.create) |> handle_request)

let get_server () =
  match !grpc_server with
  | None ->
    let server = 
      Server.(
        v ()
        |> add_service ~name:"network" ~service:(xapi_network_service ()))
    in
    grpc_server := Some server ;
    server
  | Some server -> server

let handler req fd reqd =
  debug "Entering gRPC handler" ;
  match reqd with
  | Http_svr.H2_reqd reqd ->
      let _ = Thread.create (fun () -> Grpc_unix.Server.handle_request (get_server ()) reqd) () in
      ()
  | Http_svr.H1_reqd _ | Http_svr.No_reqd ->
      failwith "gRPC requires HTTP/2"
