(* include Grpc.Server.S *)

type 'a context = 'a

type 'a t
(** [t] represents a server and its associated services and routing information. *)

val v : unit -> 'a t
(** [v ()] creates a new server. *)

val add_service : name:string -> service:(H2.Reqd.t -> 'a context -> unit) -> 'a t -> 'a t
(** [add_service ~name ~service t] adds [service] to [t] and ensures that it is routable via [name]. *)

val handle_request : 'a t -> H2.Reqd.t -> 'a context -> unit
(** [handle_request t reqd] routes [reqd] to the appropriate service in [t] if available. *)


module Rpc : sig
  type 'a unary = string -> 'a context -> Grpc.Status.t * string option
  (** [unary] is the type for a unary grpc rpc, one request, one response. *)

  type 'a client_streaming = string Seq.t -> 'a context -> Grpc.Status.t * string option
  (** [client_streaming] is the type for an rpc where the client streams the requests and the server responds once. *)

  type 'a server_streaming = string -> (string -> unit) -> 'a context -> Grpc.Status.t
  (** [server_streaming] is the type for an rpc where the client sends one request and the server sends multiple responses. *)

  type 'a bidirectional_streaming =
    string Seq.t -> (string -> unit) -> 'a context -> Grpc.Status.t
  (** [bidirectional_streaming] is the type for an rpc where both the client and server can send multiple messages. *)

  type 'a t =
    | Unary of 'a unary
    | Client_streaming of 'a client_streaming
    | Server_streaming of 'a server_streaming
    | Bidirectional_streaming of 'a bidirectional_streaming

  (** [t] represents the types of rpcs available in gRPC. *)

  val unary : f:'a unary -> H2.Reqd.t -> 'a context -> unit
  (** [unary ~f reqd] calls [f] with the request obtained from [reqd] and handles sending the response. *)

  val client_streaming : f:'a client_streaming -> H2.Reqd.t -> 'a context -> unit
  (** [client_streaming ~f reqd] calls [f] with a stream to pull requests from and handles sending the response. *)

  val server_streaming : f:'a server_streaming -> H2.Reqd.t -> 'a context -> unit
  (** [server_streaming ~f reqd] calls [f] with the request optained from [reqd] and handles sending the responses pushed out. *)

  val bidirectional_streaming : f:'a bidirectional_streaming -> H2.Reqd.t -> 'a context -> unit
  (** [bidirectional_streaming ~f reqd] calls [f] with a stream to pull requests from and andles sending the responses pushed out. *)
end

module Service : sig
  type 'a t
  (** [t] represents a gRPC service with potentially multiple rpcs and the information needed to route to them. *)

  val v : unit -> 'a t
  (** [v ()] creates a new service *)

  val add_rpc : name:string -> rpc:'a Rpc.t -> 'a t -> 'a t
  (** [add_rpc ~name ~rpc t] adds [rpc] to [t] and ensures that [t] can route to it with [name]. *)

  val handle_request : 'a t -> H2.Reqd.t -> 'a context -> unit
  (** [handle_request t reqd] handles routing [reqd] to the correct rpc if available in [t]. *)
end
