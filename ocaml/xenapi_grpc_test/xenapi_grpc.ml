open Grpc_unix
open Xenapi
open Ocaml_protoc_plugin

module D = Debug.Make (struct let name = "xenapi_grpc" end)

open D

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

let server () =
  Server.(
    v ()
    |> add_service ~name:"network" ~service:(xapi_network_service ()))

let connection_handler grpc_server =
  let error_handler _client_socket ?request:_ _error start_response =
    debug "Error in request from" ;
    let response_body = start_response H2.Headers.empty in
    H2.Body.Writer.write_string response_body
      "There was an error handling your request.";
    H2.Body.Writer.close response_body
  in
  let request_handler _client_address _proxy {Gluten.reqd; _ } =
    let { H2.Request.meth; target; _ } = H2.Reqd.request reqd in
    debug "You made a %s request to the following resource: %s" (H2.Method.to_string meth) target ;
    let _ = Thread.create (fun () -> Grpc_unix.Server.handle_request grpc_server reqd) () in
    ()
  in
  fun addr socket ->
    H2_unix.Server.create_connection_handler ~request_handler
      ~error_handler addr socket
