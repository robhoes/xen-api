open Grpc_unix
open Xenapi_proto.Xenapi
open Ocaml_protoc_plugin

module D = Debug.Make (struct let name = "grpc_server" end)

open D

let grpc_server = ref None

module Network = struct
  let create (buffer : string) _context =
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

module Custom = Api_server_common.Actions
module Forward = Api_server_common.Forwarder

module Session = struct
  let login_with_password (buffer : string) (http_req, fd) =
    debug "session.login_with_password" ;
    let decode, encode = Service.make_service_functions Session.login_with_password in
    (* Decode the request. *)
      Reader.create buffer |> decode |> function
      | Ok login_with_password_msg ->
          debug "session.login_with_password: received {uname=%s}"
            login_with_password_msg.Login_with_password_msg.uname;
          let __call = "session.login_with_password" in
  
          let __label = __call in
          let (__sync_ty, __call) = Server_helpers.sync_ty_and_maybe_remove_prefix __call in

          let subtask_of = if http_req.Http.Request.task <> None then
            http_req.Http.Request.task else http_req.Http.Request.subtask_of in
          let http_other_config = Context.get_http_other_config http_req in
          let resp =
            Server_helpers.exec_with_new_task ("dispatch:" ^ __call) ~http_other_config
              ?subtask_of:(Option.map Ref.of_string subtask_of) @@ fun __context ->
            Server_helpers.dispatch_exn_wrapper @@ fun () ->
  
            let uname = login_with_password_msg.Login_with_password_msg.uname in
            let pwd = login_with_password_msg.Login_with_password_msg.pwd in
            let version = login_with_password_msg.Login_with_password_msg.version in
            let originator = login_with_password_msg.Login_with_password_msg.originator in
  
  
            let rbac __context fn = fn () in
            let marshaller = (fun x -> API.rpc_of_ref_session x) in
            let local_op =
              fun ~__context -> (rbac __context (fun () -> 
                (Custom.Session.login_with_password ~__context:(Context.check_for_foreign_database ~__context)
                  ~uname ~pwd ~version ~originator))) in
            let supports_async = false in
            let generate_task_for = true in
            let forward_op =
              fun ~local_fn ~__context -> (rbac __context (fun () ->
                (Forward.Session.login_with_password ~__context:(Context.check_for_foreign_database ~__context)
                  ~uname ~pwd ~version ~originator) )) in
            let resp = Server_helpers.do_dispatch ~forward_op supports_async __call
                        local_op marshaller fd http_req __label __sync_ty generate_task_for in
            resp
          in
          let response =
            match resp.Rpc.contents with
            | Rpc.String s -> s
            | _ -> "not_implemented"
          in
          Grpc.Status.(v OK), Some (response |> encode |> Writer.contents)
      | Error e ->
          error "Could not decode request: %s" (Result.show_error e) ;
          Grpc.Status.(v Unknown), None
end

module Event = struct
  let stream (buffer : string) f _context =
    debug "event.stream" ;
    let decode, encode = Service.make_service_functions Event.stream in
    (* Decode the request. *)
    Reader.create buffer |> decode |> function
    | Ok msg ->
        encode 1 |> Writer.contents |> f ;
        encode 2 |> Writer.contents |> f ;
        encode 3 |> Writer.contents |> f ;
        encode 4 |> Writer.contents |> f ;
        encode 5 |> Writer.contents |> f ;
        Grpc.Status.(v OK)
    | Error e ->
        error "Could not decode request: %s" (Result.show_error e) ;
        Grpc.Status.(v Unknown)
end

let xapi_network_service () =
  Server.Service.(
    v () |> add_rpc ~name:"create" ~rpc:(Unary Network.create) |> handle_request)
    
let session_service () = (* H2.Reqd.t -> 'a -> unit *)
  Server.Service.(
    v () |> add_rpc ~name:"login_with_password" ~rpc:(Unary Session.login_with_password) |> handle_request)

let event_service () =
  Server.Service.(
    v () |> add_rpc ~name:"stream" ~rpc:(Server_streaming Event.stream) |> handle_request)

let get_server () =
  match !grpc_server with
  | None ->
    let server = 
      Server.(
        v ()
        |> add_service ~name:"network" ~service:(xapi_network_service ())
        |> add_service ~name:"session" ~service:(session_service ())
        |> add_service ~name:"event" ~service:(event_service ()))
    in
    grpc_server := Some server ;
    server
  | Some server -> server

let handler req fd reqd =
  debug "Entering gRPC handler" ;
  let context = (req, fd) in
  match reqd with
  | Http_svr.H2_reqd reqd ->
      let _ = Thread.create (fun () -> Grpc_unix.Server.handle_request (get_server ()) reqd context) () in
      ()
  | Http_svr.H1_reqd _ | Http_svr.No_reqd ->
      failwith "gRPC requires HTTP/2"
