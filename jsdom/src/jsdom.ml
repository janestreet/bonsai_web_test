open! Core
open Js_of_ocaml

module Expert_for_custom_test_handles = struct
  let event typ : (Js.js_string Js.t -> 'a Js.Optdef.t -> Dom_html.event Js.t) Js.constr =
    match Js.Unsafe.get Js.Unsafe.global (Js.string typ) |> Js.Optdef.to_option with
    | None -> raise_s [%message "Event type not found on global object" (typ : string)]
    | Some constr -> constr
  ;;

  let dispatch_event' ?(event_type = "Event") ?props ?(bubbles = true) ~event_name element
    =
    let event_constr = event event_type in
    let props =
      match props with
      | None -> Js.Unsafe.obj [||]
      | Some props -> Js.Unsafe.obj (Array.of_list props)
    in
    Js.Unsafe.set props (Js.string "bubbles") (Js.bool bubbles);
    let event = new%js event_constr (Js.string event_name) props in
    element##dispatchEvent event |> Fn.ignore
  ;;

  let dispatch_event ?event_type ?props ?bubbles ~event_name ~selector () =
    let element =
      Dom_html.document##querySelector (Js.string selector) |> Js.Opt.to_option
    in
    match element with
    | None -> print_endline [%string "No element with selector `%{selector}` found!"]
    | Some element -> dispatch_event' ?event_type ?props ?bubbles ~event_name element
  ;;

  let clear_dom () =
    Dom_html.document##.documentElement##.innerHTML
    := Js.string
         {|<!DOCTYPE html>
<html>
    <head>
        <meta charset="UTF-8">
    </head>
    <body>
        <div id="app"></div>
    </body>
</html>
|}
  ;;

  let build_warning = "Have you enabled JSDom in your build system?"

  let assert_in_jsdom_context ~here () =
    match Am_running_how_js.am_running_how with
    | `Browser | `Browser_test | `Browser_benchmark | `Node_jsdom_test -> ()
    | `Node | `Node_benchmark | `Node_test ->
      print_s
        [%message
          [%string
            "WARNING: Attempted to run tests while not in JSDom or browser context! Your \
             tests are likely to malfunction. %{build_warning}"]
            (here : Source_code_position.t)
            (Am_running_how_js.am_running_how
             : [ `Browser
               | `Browser_test
               | `Browser_benchmark
               | `Node
               | `Node_benchmark
               | `Node_test
               | `Node_jsdom_test
               ])]
  ;;

  let reset_global_state_for_startup ~(here : [%call_pos]) ?has_focus () =
    assert_in_jsdom_context ~here ();
    Byo_toplayer_private_vdom.For_jsdom_tests.reset_inertness ();
    clear_dom ();
    Mock_browser_apis.mock_console_log ();
    (match has_focus with
     | Some has_focus -> Mock_browser_apis.mock_has_focus ~has_focus
     | None -> ());
    Mock_browser_apis.mock_matches ();
    Mock_browser_apis.Animation_frame_tasks.mock ();
    Byo_toplayer_private_vdom.For_jsdom_tests.reset_inertness ()
  ;;

  let reset_global_state_for_shutdown () =
    Byo_toplayer_private_vdom.For_jsdom_tests.reset_inertness ();
    clear_dom ();
    Byo_toplayer_private_vdom.For_jsdom_tests.reset_inertness ();
    Mock_browser_apis.Animation_frame_tasks.cleanup ()
  ;;

  let bump_event_loop () =
    let open Async_kernel in
    let ivar = Ivar.create () in
    Js_of_ocaml.Dom_html.setTimeout (fun () -> Ivar.fill_if_empty ivar ()) 0.
    |> (ignore : Js_of_ocaml.Dom_html.timeout_id_safe -> unit);
    Ivar.read ivar
  ;;

  let run_request_animation_frame_tasks () =
    Mock_browser_apis.Animation_frame_tasks.run_queued ()
  ;;

  module Dom_serialization = Dom_serialization
end

module Pointer_event = struct
  module Kind = struct
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

    let to_string = function
      | Pointer_over -> "pointerover"
      | Pointer_enter -> "pointerenter"
      | Pointer_down -> "pointerdown"
      | Pointer_move -> "pointermove"
      | Pointer_up -> "pointerup"
      | Pointer_cancel -> "pointercancel"
      | Pointer_out -> "pointerout"
      | Pointer_leave -> "pointerleave"
      | Got_pointer_capture -> "gotpointercapture"
      | Lost_pointer_capture -> "lostpointercapture"
    ;;
  end

  module Pointer_type = struct
    type t =
      | Mouse
      | Pen
      | Touch
      | Undefined

    let to_string = function
      | Mouse -> "mouse"
      | Pen -> "pen"
      | Touch -> "touch"
      | Undefined -> ""
    ;;
  end

  let dispatch'
    ?(is_primary = false)
    ?(pointer_type = Pointer_type.Undefined)
    ?(pointer_id = 0.)
    ~kind
    node
    =
    let event_name = Kind.to_string kind in
    let pointer_type = Pointer_type.to_string pointer_type in
    Expert_for_custom_test_handles.dispatch_event'
      ~event_name
      ~event_type:"MouseEvent"
      ~props:
        [ "isPrimary", Js.bool is_primary |> Js.Unsafe.inject
        ; "pointerType", Js.string pointer_type |> Js.Unsafe.inject
        ; "pointerId", Js.number_of_float pointer_id |> Js.Unsafe.inject
        ]
      node
  ;;

  let dispatch
    ~(here : [%call_pos])
    ?is_primary
    ?pointer_type
    ?pointer_id
    ~kind
    ~selector
    ()
    =
    let element =
      Dom_html.document##querySelector (Js.string selector) |> Js.Opt.to_option
    in
    match element with
    | None ->
      print_endline
        [%string
          "No element with selector `%{selector}` found! %{here#Source_code_position}"]
    | Some element -> dispatch' ?is_primary ?pointer_type ?pointer_id ~kind element
  ;;
end

module Mouse_event = struct
  module Kind = struct
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

    let to_string = function
      | Mouse_down -> "mousedown"
      | Mouse_up -> "mouseup"
      | Click -> "click"
      | Mouse_leave -> "mouseleave"
      | Mouse_enter -> "mouseenter"
      | Mouse_out -> "mouseout"
      | Mouse_over -> "mouseover"
      | Double_click -> "dblclick"
      | Context_menu -> "contextmenu"
      | Mouse_move -> "mousemove"
    ;;
  end

  let dispatch'
    ?(alt_key = false)
    ?(shift_key = false)
    ?(ctrl_key = false)
    ?(meta_key = false)
    ?(button = `Left)
    ~kind
    node
    =
    let event_name = Kind.to_string kind in
    let button_int =
      match button with
      | `Left -> 0.
      | `Middle -> 1.
      | `Right -> 2.
    in
    Expert_for_custom_test_handles.dispatch_event'
      ~event_name
      ~event_type:"MouseEvent"
      ~props:
        [ "altKey", Js.bool alt_key |> Js.Unsafe.inject
        ; "shiftKey", Js.bool shift_key |> Js.Unsafe.inject
        ; "ctrlKey", Js.bool ctrl_key |> Js.Unsafe.inject
        ; "metaKey", Js.bool meta_key |> Js.Unsafe.inject
        ; "button", Js.number_of_float button_int |> Js.Unsafe.inject
        ]
      node
  ;;

  let dispatch
    ~(here : [%call_pos])
    ?alt_key
    ?shift_key
    ?ctrl_key
    ?meta_key
    ?button
    ~kind
    ~selector
    ()
    =
    let element =
      Dom_html.document##querySelector (Js.string selector) |> Js.Opt.to_option
    in
    match element with
    | None ->
      print_endline
        [%string
          "No element with selector `%{selector}` found! %{here#Source_code_position}"]
    | Some element ->
      dispatch' ?alt_key ?shift_key ?ctrl_key ?meta_key ?button ~kind element
  ;;
end

module Keyboard_event = struct
  module Kind = struct
    type t =
      | Key_down
      | Key_up

    let to_string = function
      | Key_down -> "keydown"
      | Key_up -> "keyup"
    ;;
  end

  let dispatch'
    ?(alt_key = false)
    ?(shift_key = false)
    ?(ctrl_key = false)
    ?(meta_key = false)
    ?(repeat = false)
    ?key
    ~code
    ~kind
    node
    =
    let event_name = Kind.to_string kind in
    let code_string = Vdom_keyboard.Keystroke.Keyboard_code.to_code_string code in
    let location =
      Vdom_keyboard.Keystroke.Keyboard_code.to_location code |> float_of_int
    in
    let key_opt =
      match key with
      | None -> Js.Optdef.empty
      | Some key -> Js.string key |> Js.Optdef.return
    in
    Expert_for_custom_test_handles.dispatch_event'
      ~event_name
      ~event_type:"KeyboardEvent"
      ~props:
        [ "altKey", Js.bool alt_key |> Js.Unsafe.inject
        ; "shiftKey", Js.bool shift_key |> Js.Unsafe.inject
        ; "ctrlKey", Js.bool ctrl_key |> Js.Unsafe.inject
        ; "metaKey", Js.bool meta_key |> Js.Unsafe.inject
        ; "repeat", Js.bool repeat |> Js.Unsafe.inject
        ; "code", Js.string code_string |> Js.Unsafe.inject
        ; "location", Js.number_of_float location |> Js.Unsafe.inject
        ; "key", key_opt |> Js.Unsafe.inject
        ]
      node
  ;;

  let dispatch
    ~(here : [%call_pos])
    ?alt_key
    ?shift_key
    ?ctrl_key
    ?meta_key
    ?repeat
    ?key
    ~code
    ~kind
    ~selector
    ()
    =
    let element =
      Dom_html.document##querySelector (Js.string selector) |> Js.Opt.to_option
    in
    match element with
    | None ->
      print_endline
        [%string
          "No element with selector `%{selector}` found! %{here#Source_code_position}"]
    | Some element ->
      dispatch' ?alt_key ?shift_key ?ctrl_key ?meta_key ?repeat ?key ~code ~kind element
  ;;
end

module Handle_experimental = struct
  module Bonsai_private = Bonsai.Private
  open Bonsai_web
  open Expert_for_custom_test_handles

  type 'a t =
    { handle : 'a Bonsai_web.Driver.t
    ; bonsai_time_source : Bonsai.Time_source.t
    ; get_vdom : 'a -> Vdom.Node.t
    ; filter_printed_attributes : (key:string -> data:string -> bool) option
    ; document_has_focus : bool ref
    ; mutable currently_hovered : Dom_html.element Js.t option
    ; mutable last_result : string
    }

  let create
    (type a)
    ~here
    ?filter_printed_attributes
    ?(start_time = Time_ns.epoch)
    ?optimize
    ~document_starts_with_focus
    ~get_vdom
    (computation : local_ Bonsai.graph -> a Bonsai.t)
    =
    let document_has_focus = ref document_starts_with_focus in
    reset_global_state_for_startup ~here ~has_focus:(fun () -> !document_has_focus) ();
    let bonsai_time_source = Bonsai.Time_source.create ~start:start_time in
    let handle =
      Bonsai_web.Driver.create
        ?optimize
        ~instrumentation:(fun ~should_debug:_ ~should_profile:_ ->
          { Bonsai_private.Instrumentation.Config.instrument_for_computation_watcher =
              Incr.return Bonsai_private.Instrumentation.Watching.Not_watching
          ; instrument_for_profiling =
              Incr.return Bonsai_private.Instrumentation.Profiling.Not_profiling
          ; set_latest_graph_info = (fun _ -> ())
          ; computation_watcher_queue = Queue.create ()
          ; start_timer = (fun _ -> ())
          ; stop_timer = (fun () -> ())
          })
        ~bind_to_element_with_id:"app"
        ~time_source:bonsai_time_source
        computation
      |> Bonsai_web.Driver.start ~on_display_for_marking_started:(fun () -> ()) ~get_vdom
    in
    (* The real Bonsai runtime does a synchronous [recompute] immediately after starting up.*)
    Bonsai_web.Driver.recompute handle;
    { handle
    ; bonsai_time_source
    ; get_vdom
    ; filter_printed_attributes
    ; document_has_focus
    ; currently_hovered = None
    ; last_result = ""
    }
  ;;

  let destroy { handle; _ } =
    Bonsai_web.Driver.destroy handle;
    reset_global_state_for_shutdown ()
  ;;

  let with_
    ~(here : [%call_pos])
    ?filter_printed_attributes
    ?start_time
    ?optimize
    ?(document_starts_with_focus = true)
    ~get_vdom
    computation
    callback
    =
    let handle =
      create
        ~here
        ?filter_printed_attributes
        ?start_time
        ?optimize
        ~document_starts_with_focus
        ~get_vdom
        computation
    in
    protect ~f:(fun () -> callback handle) ~finally:(fun () -> destroy handle)
  ;;

  let with_async
    ~(here : [%call_pos])
    ?filter_printed_attributes
    ?start_time
    ?optimize
    ?(document_starts_with_focus = true)
    ~get_vdom
    computation
    callback
    =
    let open Async_kernel in
    let handle =
      create
        ~here
        ?filter_printed_attributes
        ?start_time
        ?optimize
        ~document_starts_with_focus
        ~get_vdom
        computation
    in
    Monitor.protect
      (fun () -> callback handle)
      ~finally:(fun () ->
        destroy handle;
        return ())
  ;;

  let last_result { handle; _ } = Bonsai_web.Driver.result handle

  let print_dom t =
    let result =
      Dom_serialization.dom_to_string
        ?filter_printed_attributes:t.filter_printed_attributes
        ()
    in
    t.last_result <- result;
    print_endline result
  ;;

  let print_dom_diff ?context ?diff_against t =
    let old = Option.value diff_against ~default:t.last_result in
    let new_ =
      Dom_serialization.dom_to_string
        ?filter_printed_attributes:t.filter_printed_attributes
        ()
    in
    t.last_result <- new_;
    Expect_test_patdiff.print_patdiff ?context old new_
  ;;

  let inject ({ handle; _ } as t) callback =
    let r = last_result t in
    let effect = callback r in
    Bonsai_web.Driver.schedule_event handle effect
  ;;

  let advance_clock_by { bonsai_time_source; _ } span =
    Bonsai.Time_source.advance_clock_by bonsai_time_source span
  ;;

  let one_frame { handle; _ } =
    (* We considered having this function bump the time source by 1/16 ms, but decided not
       to do so, because it makes manual control of the time source harder. *)
    Bonsai_web.Driver.recompute handle
  ;;

  let bump_event_loop (_ : _ t) = bump_event_loop ()

  let run_request_animation_frame_tasks (_ : _ t) =
    Mock_browser_apis.Animation_frame_tasks.run_queued ()
  ;;

  let query_selector (_ : _ t) ~selector =
    Dom_html.document##querySelector (selector |> Js.string) |> Js.Opt.to_option
  ;;

  let query_selector_exn ~here t ~selector =
    query_selector t ~selector |> Option.value_exn ~here
  ;;

  let query_selector_all (_ : _ t) ~selector =
    let nodes = Dom_html.document##querySelectorAll (selector |> Js.string) in
    Js.Unsafe.global ##. Array##from nodes |> Js.to_array |> Array.to_list
  ;;

  let is_inert element =
    Js.Unsafe.meth_call element "closest" [| Js.Unsafe.inject (Js.string "[inert]") |]
    |> Js.Opt.to_option
    |> Option.is_some
  ;;

  let query_selector_non_inert_exn ~here t ~selector =
    match query_selector t ~selector with
    | None ->
      raise_s
        [%message
          "No element matching query" (selector : string) (here : Source_code_position.t)]
    | Some element ->
      if is_inert element
      then
        raise_s
          [%message
            "Element matching query is inert; is there an open modal?"
              (selector : string)
              (here : Source_code_position.t)]
      else element
  ;;

  let dispatch_event ?event_type ?props ~event_name element =
    dispatch_event' ?event_type ?props ~event_name element
  ;;

  let focus ~(here : [%call_pos]) harness ~selector =
    let node = query_selector_non_inert_exn ~here harness ~selector in
    node##focus
  ;;

  let blur ~(here : [%call_pos]) (_ : _ t) =
    match Dom_html.document##.activeElement |> Js.Opt.to_option with
    | Some elem -> elem##blur
    | None ->
      print_s
        [%message "Could not blur; no active element." (here : Source_code_position.t)]
  ;;

  (** This method fires click and associated events for the primary button

      https://www.w3.org/TR/pointerevents/#mapping-for-devices-that-do-not-support-hover

      One thing to note is that click events are actually fired after all other events
      happen, so if the user [unhover]s the element after clicking, those events should
      fire before the click event. With how this is implemented, that isn't the case, but
      it should be fine. *)
  let click_on ~(here : [%call_pos]) harness ~selector =
    let node = query_selector_non_inert_exn ~here harness ~selector in
    Pointer_event.dispatch' ~kind:Pointer_down node;
    Mouse_event.dispatch' ~kind:Mouse_down ~button:`Left node;
    Pointer_event.dispatch' ~kind:Pointer_up node;
    Mouse_event.dispatch' ~kind:Mouse_up ~button:`Left node;
    Mouse_event.dispatch' ~kind:Click ~button:`Left node
  ;;

  let right_click_on ~(here : [%call_pos]) harness ~selector =
    let node = query_selector_non_inert_exn ~here harness ~selector in
    Mouse_event.dispatch' ~kind:Mouse_down ~button:`Right node;
    Mouse_event.dispatch' ~kind:Mouse_up ~button:`Right node;
    Mouse_event.dispatch' ~kind:Context_menu ~button:`Right node
  ;;

  let set_input_element_value ~(here : [%call_pos]) harness ~selector ~value =
    let node = query_selector_non_inert_exn ~here harness ~selector in
    let node_with_value : < value : Js.js_string Js.t Js.optdef_prop > Js.t =
      Js.Unsafe.coerce node
    in
    match
      node_with_value##.value |> Js.Optdef.to_option |> Option.map ~f:Js.to_string
    with
    | None ->
      print_s
        [%message
          "Not setting value, because node doesn't have a value prop"
            (value : string)
            (here : Source_code_position.t)]
    | Some old_value when String.equal old_value value ->
      print_s
        [%message
          "Not setting value, because current value is the same as the new value"
            (value : string)
            (here : Source_code_position.t)]
    | Some _ ->
      Js.Unsafe.set node (Js.string "value") (Js.string value);
      dispatch_event node ~event_type:"Event" ~event_name:"change";
      dispatch_event node ~event_type:"InputEvent" ~event_name:"input"
  ;;

  let press_key
    ~(here : [%call_pos])
    ?alt_key
    ?shift_key
    ?ctrl_key
    ?meta_key
    harness
    ~selector
    ~code
    =
    let node = query_selector_non_inert_exn ~here harness ~selector in
    Keyboard_event.dispatch'
      ?alt_key
      ?shift_key
      ?ctrl_key
      ?meta_key
      ~code
      ~kind:Keyboard_event.Kind.Key_down
      node;
    Keyboard_event.dispatch'
      ?alt_key
      ?shift_key
      ?ctrl_key
      ?meta_key
      ~code
      ~kind:Keyboard_event.Kind.Key_up
      node
  ;;

  (* As noted in the MLI, [hover] and [unhover] are rough simulations, and can't actually
   capture moving a mouse across the page from one element to another.

   The "real" mechanics are defined here: https://www.w3.org/TR/uievents/#events-mouseevent-event-order

   Note that pointer events are fired before mouse events:
      https://www.w3.org/TR/pointerevents/#mapping-for-devices-that-support-hover
      https://www.w3.org/TR/pointerevents/#mapping-for-devices-that-do-not-support-hover
  *)

  let hover ~(here : [%call_pos]) harness ~selector =
    let node = query_selector_non_inert_exn ~here harness ~selector in
    harness.currently_hovered <- Some node;
    Js.Unsafe.set Js.Unsafe.global "hoveredFromHandle" node;
    Mouse_event.dispatch' ~kind:Mouse_move node;
    Pointer_event.dispatch' ~kind:Pointer_over node;
    Pointer_event.dispatch' ~kind:Pointer_enter node;
    Mouse_event.dispatch' ~kind:Mouse_over node;
    Mouse_event.dispatch' ~kind:Mouse_enter node
  ;;

  let unhover ~(here : [%call_pos]) harness =
    (* [here] is here for API consistency.*)
    ignore (here : Source_code_position.t);
    Option.iter harness.currently_hovered ~f:(fun node ->
      Mouse_event.dispatch' ~kind:Mouse_move node;
      Pointer_event.dispatch' ~kind:Pointer_out node;
      Pointer_event.dispatch' ~kind:Pointer_leave node;
      Mouse_event.dispatch' ~kind:Mouse_out node;
      Mouse_event.dispatch' ~kind:Mouse_leave node);
    Js.Unsafe.delete Js.Unsafe.global "hoveredFromHandle";
    harness.currently_hovered <- None
  ;;

  let print_active_element
    ?(depth = 0)
    ({ document_has_focus; filter_printed_attributes; _ } : _ t)
    =
    (* We could use [document.activeElement] or [querySelector(":focus")] to track focus.
      We prefer [activeElement], because that is set regardless of whether the document
      itself has focus. *)
    match Dom_html.document##.activeElement |> Js.Opt.to_option with
    | Some elem ->
      let elem =
        Dom_serialization.dom_to_string ?filter_printed_attributes ~depth ~node:elem ()
      in
      let suffix = if not !document_has_focus then " (document unfocused)" else "" in
      elem ^ suffix |> print_endline
    | None -> print_endline "<nothing focused>"
  ;;

  (* Re-exported so the [here] arg isn't optional for the other helper functions. *)
  let query_selector_exn ~(here : [%call_pos]) t ~selector =
    query_selector_exn ~here t ~selector
  ;;
end
