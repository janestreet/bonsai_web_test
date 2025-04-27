open! Core
module Bonsai_private = Bonsai.Private
open Bonsai_web
open Bonsai_web_test
open Bonsai.Let_syntax

let component ~n (local_ graph) =
  let component =
    Fn.apply_n_times
      ~n
      (fun f (local_ graph) ->
        let x = f graph in
        let state, inject = Bonsai.state 0 graph in
        let%arr state and inject and x in
        let _ = state
        and _ = inject
        and _ = x in
        ())
      (fun (local_ _graph) -> Bonsai.return ())
  in
  component graph
;;

module%test Stack_overflow_during_graph_application = struct
  let test ~n ~(here : [%call_pos]) () =
    Expect_test_helpers_core.require_does_not_raise ~here (fun () ->
      let _ : unit Bonsai_private.Computation.t =
        Bonsai_private.top_level_handle (fun (local_ graph) -> component ~n graph)
      in
      ());
    print_endline "did not raise"
  ;;

  let%expect_test ("JSOO - Stack overflow during graph application"
    [@tags "js-only", "no-wasm"])
    =
    test ~n:1_000 ();
    [%expect {| did not raise |}]
  ;;

  (* let%expect_test ("WASM - Stack overflow during graph application" [@tags "wasm-only"]) = *)
  (*   test ~n:10_000 () *)
  (* ;; *)
end

module%test Stack_overflow_during_handle_create = struct
  let test ~n ~(here : [%call_pos]) () =
    Expect_test_helpers_core.require_does_raise ~here (fun () ->
      let _ : (unit, never_returns) Handle.t =
        Handle.create Result_spec.invisible (fun (local_ graph) -> component ~n graph)
      in
      ())
  ;;

  let%expect_test ("JSOO - Stack overflow during handle creation"
    [@tags "js-only", "no-wasm"])
    =
    test ~n:1_000 ();
    [%expect {| ("Stack overflow") |}]
  ;;

  let why_is_the_above_test_stack_overflowing () =
    try
      let _ : (unit, never_returns) Handle.t =
        Handle.create Result_spec.invisible (fun (local_ graph) ->
          component ~n:1_000 graph)
      in
      ()
    with
    | exn ->
      let stack_trace =
        Js_of_ocaml.Js.Js_error.of_exn exn |> Option.bind ~f:Js_of_ocaml.Js.Js_error.stack
      in
      print_s [%message (stack_trace : string option)]
  ;;

  let _ = why_is_the_above_test_stack_overflowing

  (* let%expect_test "Why is the above test still stack overflowing?" = *)
  (*   why_is_the_above_test_stack_overflowing () *)
  (* ;; *)

  (* let%expect_test ("WASM - Stack overflow during handle creation" [@tags "wasm-only"]) = *)
  (*   test ~n:10_000 () *)
  (* ;; *)
end
