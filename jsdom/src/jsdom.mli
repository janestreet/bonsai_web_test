open! Core
open Async_kernel
open Js_of_ocaml

(** This library contains a test handle that can be used in JSDom-based expect tests, as
    well as some helper functions.

    JSDom tests are quite useful for testing low-level vdom attrs / hooks, and bindings to
    external libraries. We expect the [Jsdom] library to mostly be used by people working
    on bindings and reusable components. *)

module Handle_experimental : sig
  open Bonsai_web

  type 'a t

  val with_
    :  here:[%call_pos]
    -> ?filter_printed_attributes:(key:string -> data:string -> bool)
    -> ?start_time:Time_ns.t
    -> ?optimize:bool
         (** Whether the document should start with focus. Defaults to [true]. *)
    -> ?document_starts_with_focus:bool
    -> get_vdom:('a -> Vdom.Node.t)
         (** [get_vdom] should retrieve vdom from the computed component. It should not do
             any meaningful work or transformations over the vdom. Most use cases should
             be [Fn.id], or maybe [fst], [snd], getting a record field, etc. *)
    -> (local_ Bonsai.graph -> 'a Bonsai.t)
    -> ('a t -> unit)
    -> unit

  (** [with_async] allows you to do async operations in your tests. Avoid using this
      unless you need to test asynchronous browser operations.

      The runtime semantics of [Async_js_test]'s de-asyncing of Node and [Async_js] in the
      browser are quite different. It's also VERY slow. *)
  val with_async
    :  here:[%call_pos]
    -> ?filter_printed_attributes:(key:string -> data:string -> bool)
    -> ?start_time:Time_ns.t
    -> ?optimize:bool
         (** Whether the document should start with focus. Defaults to [true]. *)
    -> ?document_starts_with_focus:bool
    -> get_vdom:('a -> Vdom.Node.t)
         (** [get_vdom] should retrieve vdom from the computed component. It should not do
             any meaningful work or transformations over the vdom. Most use cases should
             be [Fn.id], or maybe [fst], [snd], getting a record field, etc. *)
    -> (local_ Bonsai.graph -> 'a Bonsai.t)
    -> ('a t -> unit Deferred.t)
    -> unit Deferred.t

  (** {2 Printing / Showing the DOM} *)

  val print_dom : _ t -> unit

  (** [print_dom_diff] prints the diff since the last time [print_dom] or [print_dom_diff]
      was called, or against [diff_against] if provided. *)
  val print_dom_diff : ?context:int -> ?diff_against:string -> _ t -> unit

  (** [print_active_element] prints [document.activeElement]. Note that a document that
      doesn't have system focus will still have an [activeElement].

      [depth] (default: [0]) controls how many levels of nested elements to print. *)
  val print_active_element : ?depth:int -> _ t -> unit

  (** [last_result] allows you to retrieve the last computed value of ['a]. This can be
      useful if the computation you are testing returns other data in addition to vdom. *)
  val last_result : 'a t -> 'a

  (** {2 Computation Controls} *)

  val one_frame : _ t -> unit

  (* Node.js's asynchronicity is powered by an event loop:
     https://nodejs.org/en/learn/asynchronous-work/event-loop-timers-and-nexttick

     Some JS APIs might use `setTimeout f 0` or `setImmediate`, which operate
     independently of Async's `time_source` tools.

     [bump_event_loop] can be used to "flush" any scheduled async events. *)
  val bump_event_loop : _ t -> unit Deferred.t
  val run_request_animation_frame_tasks : _ t -> unit
  val advance_clock_by : _ t -> Time_ns.Span.t -> unit
  val inject : 'a t -> ('a -> unit Effect.t) -> unit

  (** {2 Simulating Interactions}

      These functions are intended to be higher-level simulations of user interactions, vs
      an API for firing events on specific elements. For instance, [click_on] will trigger
      pointerdown, mousedown, pointerup, mouseup, and click events.

      Additionally, all these functions will raise if you've selected an inert element, as
      inert elements can't be interacted with by users directly.
      https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/inert *)

  val focus : here:[%call_pos] -> _ t -> selector:string -> unit
  val blur : here:[%call_pos] -> _ t -> unit
  val click_on : here:[%call_pos] -> _ t -> selector:string -> unit
  val right_click_on : here:[%call_pos] -> _ t -> selector:string -> unit

  val set_input_element_value
    :  here:[%call_pos]
    -> _ t
    -> selector:string
    -> value:string
    -> unit

  (** [press_key] is shorthand for dispatching [Key_down] followed by [Key_up]. *)
  val press_key
    :  here:[%call_pos]
    -> ?alt_key:bool
    -> ?shift_key:bool
    -> ?ctrl_key:bool
    -> ?meta_key:bool
    -> _ t
    -> selector:string
    -> code:Js_of_ocaml.Dom_html.Keyboard_code.t
    -> unit

  (** [hover] tries to simulate hovering over an element. This is very unprecise, because
      we can't simulate layout in tests. For example, while we fire a singular
      [Mouse_move] at the beginning of a [hover] and [unhover], in practice, multiple
      [Mouse_move]s with different values will be fired when users actually [hover].

      The real sequence of events fired as the mouse moves across the screen is much more
      complex, and is defined in the w3c specs here:

      https://www.w3.org/TR/uievents/#events-mouseevent-event-order *)
  val hover : here:[%call_pos] -> _ t -> selector:string -> unit

  (** See the [hover] doc comment for background on limitations. *)
  val unhover : here:[%call_pos] -> _ t -> unit

  (** {2 Low-Level DOM Manipulation} *)

  val query_selector
    :  _ t
    -> selector:string
    -> Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t option

  val query_selector_exn
    :  here:[%call_pos]
    -> _ t
    -> selector:string
    -> Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t

  val query_selector_all
    :  _ t
    -> selector:string
    -> Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t list
end

(** Nicer types for creating pointer events *)
module Pointer_event : sig
  module Kind : sig
    type t =
      | Pointer_over
      | Pointer_enter
      | Pointer_down
      | Pointer_move
      | Pointer_up
      | Pointer_cancel
      | Pointer_out
      | Pointer_leave
      | Got_pointer_capture
      | Lost_pointer_capture
    [@@deriving to_string]
  end

  module Pointer_type : sig
    type t =
      | Mouse
      | Pen
      | Touch
      | Undefined
    [@@deriving to_string]
  end

  val dispatch
    :  here:[%call_pos]
    -> ?is_primary:bool
    -> ?pointer_type:Pointer_type.t
    -> ?pointer_id:float
    -> kind:Kind.t
    -> selector:string
    -> unit
    -> unit
end

(** Nicer types for creating mouse events *)
module Mouse_event : sig
  module Kind : sig
    type t =
      | Mouse_down
      | Mouse_up
      | Click
      | Mouse_leave
      | Mouse_enter
      | Mouse_out
      | Mouse_over
      | Double_click
      | Context_menu
      | Mouse_move
  end

  val dispatch
    :  here:[%call_pos]
    -> ?alt_key:bool
    -> ?shift_key:bool
    -> ?ctrl_key:bool
    -> ?meta_key:bool
    -> ?button:[ `Left | `Middle | `Right ]
         (** [`Left] corresponds to [Primary], [`Right] to [Secondary], and [`Middle] to
             [Tertiary]. If you are testing left-handed mouse, these values will not be
             accurate *)
    -> kind:Kind.t
    -> selector:string
    -> unit
    -> unit
end

module Keyboard_event : sig
  module Kind : sig
    type t =
      | Key_down
      | Key_up
  end

  (* Including both "key" and "code" feels a bit redundant, but *)
  val dispatch
    :  here:[%call_pos]
    -> ?alt_key:bool
    -> ?shift_key:bool
    -> ?ctrl_key:bool
    -> ?meta_key:bool
    -> ?repeat:bool
    -> ?key:string
    -> code:Js_of_ocaml.Dom_html.Keyboard_code.t
    -> kind:Kind.t
    -> selector:string
    -> unit
    -> unit
end

module Expert_for_custom_test_handles : sig
  (** [dispatch_event] will dispatch an event of type [event_name] on the first element
      selected by [selector]. *)
  val dispatch_event
    :  ?event_type:string
    -> ?props:(string * Js_of_ocaml.Js.Unsafe.any) list
    -> ?bubbles:bool
    -> event_name:string
    -> selector:string
    -> unit
    -> unit

  (** [dispatch_event'] takes an event target (Document, HTMLElement, Window) instead of
      finding a target using a selector.

      You can pass [Dom_html.document] to target the document itself. *)
  val dispatch_event'
    :  ?event_type:string
    -> ?props:(string * Js_of_ocaml.Js.Unsafe.any) list
    -> ?bubbles:bool
    -> event_name:string
    -> < dispatchEvent : Dom_html.event Js.t -> 'res Js.meth ; .. > Js.t
    -> unit

  (* Node.js's asynchronicity is powered by an event loop:
   https://nodejs.org/en/learn/asynchronous-work/event-loop-timers-and-nexttick

   Some JS APIs might use `setTimeout f 0` or `setImmediate`, which operate
   independently of Async's `time_source` tools.

   [bump_event_loop] can be used to "flush" any scheduled async events. *)
  val bump_event_loop : unit -> unit Deferred.t

  (** Runs any pending [requestAnimationFrame] tasks. Will error if
      [reset_global_state_for_startup] has not been called. *)
  val run_request_animation_frame_tasks : unit -> unit

  (** Clears the DOM, and sets up various mocks of browser APIs. *)
  val reset_global_state_for_startup
    :  here:[%call_pos]
    -> ?has_focus:(unit -> bool)
    -> unit
    -> unit

  (** Clears the DOM, and cleans up various global state. *)
  val reset_global_state_for_shutdown : unit -> unit

  module Dom_serialization = Dom_serialization
end
