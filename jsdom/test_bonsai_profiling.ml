open! Core
open Bonsai_web
open Async_kernel
open Async_js_test
open Js_of_ocaml
module Jsdom_expert = Jsdom.Expert_for_custom_test_handles

module Dom_printer = struct
  let create () = ref ""
  let store t s = t := s

  let print t =
    let new_ = Jsdom_expert.Dom_serialization.dom_to_string () in
    store t new_;
    print_endline new_
  ;;

  let print_diff t =
    let old = !t in
    let new_ = Jsdom_expert.Dom_serialization.dom_to_string () in
    store t new_;
    Expect_test_patdiff.print_patdiff old new_
  ;;
end

(* Although it is otherwise discouraged, these tests MUST run [Bonsai_web.Start] and therefore
   be async, so that we can be confident that we are testing profiling config correctly. *)
let test computation f =
  Jsdom_expert.reset_global_state_for_startup ();
  let handle =
    Bonsai_web.Start.start_and_get_handle
      Bonsai_web.Start.Result_spec.just_the_view
      ~bind_to_element_with_id:"app"
      ~enable_bonsai_telemetry:Disabled
      computation
  in
  let%bind.Deferred () = Bonsai_web.Start.Handle.started handle in
  let%bind.Deferred () = Jsdom_expert.bump_event_loop () in
  let printer = Dom_printer.create () in
  let%bind () = f printer in
  let%bind.Deferred () = Jsdom_expert.bump_event_loop () in
  Bonsai_web.Start.Handle.stop handle;
  return ()
;;

let counter graph =
  let open Bonsai.Let_syntax in
  let count, update = Bonsai.state' 0 graph in
  let%arr count and update in
  {%html|
    <div>
      <button id="prev" on_click=%{fun _ -> update pred}>-</button>
      %{count#Int}
      <button id="next" on_click=%{fun _ -> update succ}>+</button>
    </div>
  |}
;;

let start_profiling () : unit =
  let f : unit -> unit =
    Js.Unsafe.pure_js_expr
      {js|(function(){
      bonsaiBugStartProfiling();
      bonsaiBugStopComputationWatcher();
    })|js}
  in
  Js.Unsafe.fun_call f [||]
;;

let print_events () =
  let f : unit -> Js.js_string Js.t =
    Js.Unsafe.pure_js_expr
      {js|
        (function(){
          return bonsaiBugPopEvents();
        })
      |js}
  in
  let events_string = Js.Unsafe.fun_call f [||] in
  let message =
    events_string
    |> Js.to_bytestring
    |> Bin_prot.Reader.of_string Bonsai_protocol.Versioned_message.bin_reader_t
  in
  let messages = Bonsai_protocol.Versioned_message.to_latest message in
  let performance_measure_count =
    List.count messages ~f:(function
      | Graph_info _ -> false
      | Performance_measure _ -> true)
  in
  print_endline [%string "performance_measure_count: %{performance_measure_count#Int}"]
;;

let%expect_test "Simple counter creates introspection events!" =
  let%bind.With dom_printer = test counter in
  Dom_printer.print dom_printer;
  [%expect
    {|
    <html>
      <head>
        <meta charset="UTF-8"> </meta>
      </head>
      <body>
        <div tabindex="0" style="outline: none;">
          <button id="prev"> - </button>
          0
          <button id="next"> + </button>
        </div>
      </body>
    </html>
    |}];
  Jsdom_expert.dispatch_event
    ~event_type:"MouseEvent"
    ~event_name:"click"
    ~selector:"#next"
    ();
  Jsdom_expert.run_request_animation_frame_tasks ();
  let%bind () = Jsdom_expert.bump_event_loop () in
  Dom_printer.print_diff dom_printer;
  [%expect
    {|
    -1,12 +1,12
      <html>
        <head>
          <meta charset="UTF-8"> </meta>
        </head>
        <body>
          <div tabindex="0" style="outline: none;">
            <button id="prev"> - </button>
    -|      0
    +|      1
            <button id="next"> + </button>
          </div>
        </body>
      </html>
    |}];
  print_events ();
  (* Initially, we ran without profiling, so there should be no measures. *)
  [%expect {| performance_measure_count: 0 |}];
  start_profiling ();
  [%expect {| Starting the Bonsai Bug profiler. |}];
  Dom_printer.print_diff dom_printer;
  [%expect {| |}];
  Jsdom_expert.dispatch_event
    ~event_type:"MouseEvent"
    ~event_name:"click"
    ~selector:"#next"
    ();
  Jsdom_expert.run_request_animation_frame_tasks ();
  let%bind () = Jsdom_expert.bump_event_loop () in
  Dom_printer.print_diff dom_printer;
  (* Note that state does not change when adding / removing instrumentation, because the
     shape of the state graph stays the same. *)
  [%expect
    {|
    -1,12 +1,12
      <html>
        <head>
          <meta charset="UTF-8"> </meta>
        </head>
        <body>
          <div tabindex="0" style="outline: none;">
            <button id="prev"> - </button>
    -|      1
    +|      2
            <button id="next"> + </button>
          </div>
        </body>
      </html>
    |}];
  print_events ();
  (* Now, there are measures! *)
  [%expect {| performance_measure_count: 23 |}];
  return ()
;;
