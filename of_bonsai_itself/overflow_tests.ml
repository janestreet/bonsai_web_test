open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Bonsai_web_test
open Bonsai_test_shared_for_testing_bonsai.Big_computation_regression_util

let overflow_height = 1_000

let%expect_test ("Bonsai prints an error message if there is a stack overflow in a \
                  computation" [@tags "no-wasm"])
  =
  let app always_safe graph =
    match%sub always_safe with
    | `Safe -> Bonsai.return String.Set.empty
    | `Overflow -> For_cont.basic ~height:overflow_height ~width:1 graph
  in
  let _handle = Handle.create lengths_result_spec (app (Bonsai.return `Safe)) in
  (* Explicitly not calling Handle.show here *)
  [%expect
    {|
    Stack overflow inside of a bonsai computation is not supported! In a future release your app might crash.
    RangeError: Maximum call stack size exceeded
    <truncated stack to preserve determinism between fast-build and fast-exe>
    |}]
;;

let%expect_test ("Bonsai actually stack overflows when an overflowed computation is \
                  active" [@tags "no-wasm"])
  =
  Expect_test_helpers_core.require_does_raise (fun () ->
    let app always_overflow graph =
      match%sub always_overflow with
      | `Safe -> Bonsai.return String.Set.empty
      | `Overflow -> For_cont.basic ~height:overflow_height ~width:1 graph
    in
    let _handle = Handle.create lengths_result_spec (app (Bonsai.return `Overflow)) in
    (* Explicitly not calling Handle.show here *)
    ());
  [%expect
    {|
    Stack overflow inside of a bonsai computation is not supported! In a future release your app might crash.
    RangeError: Maximum call stack size exceeded
    <truncated stack to preserve determinism between fast-build and fast-exe>("Stack overflow")
    |}]
;;
