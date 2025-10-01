open! Core
open! Bonsai_web
open! Async_kernel
open! Async_rpc_kernel

(** An [Interruptible_persistent_connection.t] provides a connector, whose underlying
    persistent connection can be killed + resumed on demand. This is intended for testing
    how Bonsai computations respond to drops in the underlying RPC connection, and is
    probably too low-level for most tests. *)
type t

val create
  :  connection_state:(Rpc.Connection.t -> 'conn_state)
  -> 'conn_state Rpc.Implementation.t list
  -> t

(** [create'] is a lower-level API than [create], allowing the available implementations
    to change in between connections. *)
val create'
  :  connection_state:(Rpc.Connection.t -> 'conn_state)
  -> implementations:(unit -> 'conn_state Rpc.Implementation.t list)
  -> t

(** {2 Accessors} *)

val connector : t -> Rpc_effect.Connector.t
val kill_connection : t -> unit Deferred.t
val next_connection : t -> unit Deferred.t
