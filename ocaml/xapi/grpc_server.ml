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

module Host = struct
  let get_all (buffer : string) (http_req, fd) =
    debug "host.get_all" ;
    let decode, encode = Service.make_service_functions Host.get_all in
    (* Decode the request. *)
      Reader.create buffer |> decode |> function
      | Ok msg ->
          let __call = "host.get_all" in
  
          let __label = __call in
          let (__sync_ty, __call) = Server_helpers.sync_ty_and_maybe_remove_prefix __call in

          let subtask_of = if http_req.Http.Request.task <> None then
            http_req.Http.Request.task else http_req.Http.Request.subtask_of in
          let http_other_config = Context.get_http_other_config http_req in
          let resp =
            Server_helpers.exec_with_new_task ("dispatch:" ^ __call) ~http_other_config
              ?subtask_of:(Option.map Ref.of_string subtask_of) @@ fun __context ->
            Server_helpers.dispatch_exn_wrapper @@ fun () ->
  
            let session_id = Ref.of_secret_string msg in
            let session_id_rpc = Rpc.String msg in

            Session_check.check ~intra_pool_only:false ~session_id ~action:"host.get_all";
            let arg_names_values = [("session_id", session_id_rpc)] in
            let key_names = [] in
            let rbac __context fn = Rbac.check session_id __call ~args:arg_names_values ~keys:key_names ~__context ~fn in
            let marshaller = (fun x -> API.rpc_of_ref_host_set x) in
            let local_op = fun ~__context ->(rbac __context (fun()->(Db_actions.DB_Action.Host.get_all ~__context:(Context.check_for_foreign_database ~__context) ))) in
            let supports_async = false in
            let generate_task_for = true in
            let resp = Server_helpers.do_dispatch ~session_id  supports_async __call local_op marshaller fd http_req __label __sync_ty generate_task_for in
            resp

          in
          let response = 
            match resp.Rpc.contents with
            | Rpc.Enum t -> List.map Rpc.string_of_rpc t
            | _ -> failwith "not_implemented"
          in
          Grpc.Status.(v OK), Some (response |> encode |> Writer.contents)
      | Error e ->
          error "Could not decode request: %s" (Result.show_error e) ;
          Grpc.Status.(v Unknown), None
end

module Event = struct
  let from_inner http_req fd session_id token =
    let __call = "event.from" in

    let __label = __call in
    let (__sync_ty, __call) = Server_helpers.sync_ty_and_maybe_remove_prefix __call in

    let subtask_of = if http_req.Http.Request.task <> None then
      http_req.Http.Request.task else http_req.Http.Request.subtask_of in
    let http_other_config = Context.get_http_other_config http_req in

    Server_helpers.exec_with_new_task ("dispatch:" ^ __call) ~http_other_config
      ?subtask_of:(Option.map Ref.of_string subtask_of) @@ fun __context ->
    Server_helpers.dispatch_exn_wrapper @@ fun () ->

    let classes = ["VM"] in
    let timeout = 10. in
    let session_id_rpc = Rpc.String (Ref.string_of session_id) in
    let classes_rpc = Rpc.Enum [Rpc.String "VM"] in
    let token_rpc = Rpc.String token in
    let timeout_rpc = Rpc.Float timeout in

    Session_check.check ~intra_pool_only:false ~session_id ~action:"event.from";
    let arg_names_values = [("session_id", session_id_rpc); ("classes", classes_rpc); ("token", token_rpc); ("timeout", timeout_rpc)] in
    let key_names = [] in
    let rbac __context fn = Rbac.check session_id __call ~args:arg_names_values ~keys:key_names ~__context ~fn in
    let marshaller = (fun x -> x) in
    let local_op = fun ~__context ->(rbac __context (fun()->(Custom.Event.from ~__context:(Context.check_for_foreign_database ~__context)  ~classes ~token ~timeout))) in
    let supports_async = false in
    let generate_task_for = false in
    let forward_op = fun ~local_fn ~__context -> (rbac __context (fun()-> (Forward.Event.from ~__context:(Context.check_for_foreign_database ~__context)  ~classes ~token ~timeout) )) in
    Server_helpers.do_dispatch ~session_id ~forward_op supports_async __call local_op marshaller fd http_req __label __sync_ty generate_task_for

  let stream (buffer : string) f (http_req, fd) =
    debug "event.stream" ;
    let decode, encode = Service.make_service_functions Event.stream in
    (* Decode the request. *)
    Reader.create buffer |> decode |> function
    | Ok session_id ->
        let session_id' = Ref.of_secret_string session_id in
        let rec loop token =
          let resp = from_inner http_req fd session_id' token in
          let events, token = match resp.Rpc.contents with
              | Rpc.Dict ["events", Rpc.Enum x; _; "token", Rpc.String token] ->
                  List.length x, token
              | _ -> 0, ""
          in
          debug "event count = %d, token = %s" events token ;
          if events > 0 then
            encode events |> Writer.contents |> f ;
          if token <> "" then
            loop token
        in
        let () = loop "" in
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

let host_service () =
  Server.Service.(
    v () |> add_rpc ~name:"get_all" ~rpc:(Unary Host.get_all) |> handle_request)

let get_server () =
  match !grpc_server with
  | None ->
    let server = 
      Server.(
        v ()
        |> add_service ~name:"network" ~service:(xapi_network_service ())
        |> add_service ~name:"session" ~service:(session_service ())
        |> add_service ~name:"event" ~service:(event_service ())
        |> add_service ~name:"host" ~service:(host_service ()))
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
