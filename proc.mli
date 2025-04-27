open! Core
open! Import
module Linter = Virtual_dom_test_helpers.Node_helpers.Linter

(** [test_selector] converts a [Test_selector.t] into a css selector string, so that it
    can be used as the [~selector] argument to [Handle.click_on], etc.

    We recommend not concatenating test selector CSS strings with other CSS selectors, to
    avoid relying on specific DOM structure. *)
val test_selector : Test_selector.t -> string

module Result_spec : sig
  include module type of struct
    include Bonsai_test.Result_spec
  end

  (** Construct a result spec for printing vdoms in tests.

      [filter_printed_attributes] controls which attributes on a Node will get printed
      analyzing the string name of the attribute.

      The default will filter out all [Bonsai_web.Test_selector.t] attrs.

      Style properties correspond to their property name prefixed with "style.". For
      example, to filter out the "display" CSS property, you should return false for
      "style.display"; to filter out all CSS styles, return false when the string begins
      with "style.". A Node's key corresponds to the string "@key".

      In the snippet:

      ```html <div key1="data1" key2="data2"></div> ```

      "key"'s are the left hand side of the equal and "data"'s are the right hand side of
      the equal sign. This is different from the naming convention mentioned in mdn's
      https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes name's and value's.

      {v
      ┌───────┬────────┐
      │ mdn   │ bonsai │
      ├───────┼────────┤
      │ name  │ key    │
      │ value │ data   │
      └───────┴────────┘
      v} *)
  val vdom
    :  ?filter_printed_attributes:(key:string -> data:string -> bool)
    -> ?censor_paths:bool
    -> ?censor_hash:bool
    -> ?path_censoring_message:string
    -> ?hash_censoring_message:string
    -> ?lint_min_severity:Linter.Severity.t
         (** [lint_min_severity] defaults to [Fatal]. *)
    -> ?lint_expected_failures:Linter.Rule.t list
    -> ?selector:string
    -> ('a -> Vdom.Node.t)
    -> ('a, Nothing.t) t
end

module Handle : sig
  include module type of struct
    include Bonsai_test.Handle
  end

  val create
    :  ('a, 'b) Result_spec.t
    -> ?rpc_implementations:
         Async_rpc_kernel.Rpc.Connection.t Async_rpc_kernel.Rpc.Implementation.t list
    -> ?connectors:(Rpc_effect.Where_to_connect.t -> Rpc_effect.Connector.t)
         (** By default [connectors] always returns
             [Bonsai_web.Rpc_effect.Connector.test_fallback], which uses any provided
             [rpc_implementations] to handle any dispatched RPCs. *)
    -> ?start_time:Time_ns.t
    -> ?optimize:bool
    -> (local_ Bonsai.graph -> 'a Bonsai.t)
    -> ('a, 'b) t

  (** {v
 Runs [recompute_view] and [Async_kernel_scheduler.yield_until_no_jobs_remain]
      in a loop until nothing remains to be done. This is a good sledgehammer
      function to use if you want to wait until all the effects of a user-action
      have completed.

      By default, this function prints "------ between bonsai frame ------" in between
      each iteration to demonstrate when side-effects occur, and how long it took for a
      stable state to be reached. This line is just extra documentation; it is not
      necessarily a sign that something is wrong (unless, of course, the behavior of the
      thing you're trying to test shouldn't result in an extra frame). These lines can be
      removed by passing [~silence_between_frames:true] in case your tests take a
      non-deterministic number of iterations to stabilize.

      [max_iterations] controls how many loop iterations are allowed before the
      function aborts with an exception, in case the default of 100 is too low.
      However, you should rarely, if ever need this parameter.
      v} *)
  val flush_async_and_bonsai
    :  ?max_iterations:int
    -> ?silence_between_frames:bool
    -> ('a, 'b) t
    -> unit Async_kernel.Deferred.t

  (** [min_severity] defaults to [High]. *)
  val lint_vdom
    :  ?min_severity:Linter.Severity.t
    -> ?expected_failures:Linter.Rule.t list
    -> ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> unit

  (** {2 Vdom Controls} *)

  val click_on
    :  ?extra_event_fields:(string * Js_of_ocaml.Js.Unsafe.any) list
    -> ?shift_key_down:bool
    -> ?alt_key_down:bool
    -> ?ctrl_key_down:bool
    -> ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> unit

  val submit_form
    :  ?extra_event_fields:(string * Js_of_ocaml.Js.Unsafe.any) list
    -> ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> unit

  val set_checkbox
    :  ?extra_event_fields:(string * Js_of_ocaml.Js.Unsafe.any) list
    -> ?shift_key_down:bool
    -> ?alt_key_down:bool
    -> ?ctrl_key_down:bool
    -> ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> checked:bool
    -> unit

  val input_text
    :  ?extra_event_fields:(string * Js_of_ocaml.Js.Unsafe.any) list
    -> ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> text:string
    -> unit

  val input_files
    :  ?extra_event_fields:(string * Js_of_ocaml.Js.Unsafe.any) list
    -> ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> files:Js_of_ocaml.File.file Js_of_ocaml.Js.t list
    -> unit

  val keydown
    :  ?extra_event_fields:(string * Js_of_ocaml.Js.Unsafe.any) list
    -> ?shift_key_down:bool
    -> ?alt_key_down:bool
    -> ?ctrl_key_down:bool
    -> ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> key:Js_of_ocaml.Dom_html.Keyboard_code.t
    -> unit

  val change
    :  ?extra_event_fields:(string * Js_of_ocaml.Js.Unsafe.any) list
    -> ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> value:string
    -> unit

  val focus
    :  ?extra_event_fields:(string * Js_of_ocaml.Js.Unsafe.any) list
    -> ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> unit

  val blur
    :  ?extra_event_fields:(string * Js_of_ocaml.Js.Unsafe.any) list
    -> ?related_target:string
    -> ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> unit

  val mousemove
    :  ?extra_event_fields:(string * Js_of_ocaml.Js.Unsafe.any) list
    -> ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> unit

  val mouseenter
    :  ?extra_event_fields:(string * Js_of_ocaml.Js.Unsafe.any) list
    -> ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> unit

  val wheel
    :  ?extra_event_fields:(string * Js_of_ocaml.Js.Unsafe.any) list
    -> ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> delta_y:float
    -> unit

  val trigger_hook
    :  ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> name:string
    -> ('c -> unit Vdom.Effect.t) Type_equal.Id.t
    -> 'c
    -> unit

  val trigger_hook_via
    :  ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> name:string
    -> 't Type_equal.Id.t
    -> f:('t -> 'c -> unit Vdom.Effect.t)
    -> 'c
    -> unit

  val get_hook_value
    :  ('a, 'b) t
    -> get_vdom:('a -> Vdom.Node.t)
    -> selector:string
    -> name:string
    -> 'c Type_equal.Id.t
    -> 'c
end

module Expect_test_config : Expect_test_config_types.S with module IO = Monad.Ident
