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

let req' = ref None
let fd' = ref None

module Session = struct
  let login_with_password (buffer : string) =
    debug "session.login_with_password" ;
    let decode, encode = Service.make_service_functions Session.login_with_password in
    (* Decode the request. *)
    let login_with_password_msg =
      Reader.create buffer |> decode |> function
      | Ok v -> v
      | Error e ->
          failwith
            (Printf.sprintf "Could not decode request: %s" (Result.show_error e))
    in
    debug "session.login_with_password: received {uname=%s}" login_with_password_msg.Login_with_password_msg.uname;
    
    let call = Rpc.call "session.login_with_password" [
      Rpc.String login_with_password_msg.Login_with_password_msg.uname;
      Rpc.String login_with_password_msg.Login_with_password_msg.pwd;
      Rpc.String login_with_password_msg.Login_with_password_msg.version;
      Rpc.String login_with_password_msg.Login_with_password_msg.originator
    ] in
    let response = Api_server.Server.dispatch_call (Option.get !req') (Option.get !fd') call in
    let response =
      match response.Rpc.contents with
      | Rpc.String s -> s
      | _ -> "not_implemented"
    in
    (Grpc.Status.(v OK), Some (response |> encode |> Writer.contents))
end

let xapi_network_service () =
  Server.Service.(
    v () |> add_rpc ~name:"create" ~rpc:(Unary Network.create) |> handle_request)
    
let session_service () =
  Server.Service.(
    v () |> add_rpc ~name:"login_with_password" ~rpc:(Unary Session.login_with_password) |> handle_request)

let get_server () =
  match !grpc_server with
  | None ->
    let server = 
      Server.(
        v ()
        |> add_service ~name:"network" ~service:(xapi_network_service ())
        |> add_service ~name:"session" ~service:(session_service ()))
    in
    grpc_server := Some server ;
    server
  | Some server -> server

let handler req fd reqd =
  debug "Entering gRPC handler" ;
  req' := Some req ;
  fd' := Some fd ;
  match reqd with
  | Http_svr.H2_reqd reqd ->
      let _ = Thread.create (fun () -> Grpc_unix.Server.handle_request (get_server ()) reqd) () in
      ()
  | Http_svr.H1_reqd _ | Http_svr.No_reqd ->
      failwith "gRPC requires HTTP/2"
