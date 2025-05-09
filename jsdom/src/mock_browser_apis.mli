open! Core

(** Allows [console.log]/[console.warn] output to appear in expect blocks. *)
val mock_console_log : unit -> unit

(** Overrides [HTMLElement.prototype.matches x] for [x = ":hover"], because there's a bug
    in the underlying nwsapi library. *)
val mock_matches : unit -> unit

(** Overrides [document.hasFocus()], because the JSDom implementation checks whether
    there's an [activeElement], whereas the real implementation checks whether the browser
    has system focus. We're interested in mocking system focus, to test that e.g. the
    focus stealer doesn't run in the background. *)
val mock_has_focus : has_focus:(unit -> bool) -> unit

module Animation_frame_tasks : sig
  (** Mocks out [requestAnimationFrame], so that the queue is owned by us, and can be
      flushed synchronously. *)

  val mock : unit -> unit
  val run_queued : unit -> unit
  val cleanup : unit -> unit
end
