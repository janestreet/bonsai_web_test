open! Core
open Bonsai_web
open Bonsai_web_test

module Expect_test_config = struct
  include Expect_test_config

  let regexp = Re.Str.regexp "/usr.*jenga/sandbox/[0-9]+/"

  let sanitize s =
    Expect_test_helpers_core.hide_positions_in_string (Re.Str.global_replace regexp "" s)
  ;;
end

let%expect_test "stack overflow regression test: long chain of subs" =
  (* NOTE: This expect test is a regression test against stack overflows in the
     Bonsai.Cont API. *)
  let computation (local_ graph) =
    Fn.apply_n_times
      ~n:1_000
      (fun x ->
        let _state, _set_state = Bonsai.state () graph in
        x)
      (Bonsai.Expert.Var.value (Bonsai.Expert.Var.create ())) [@nontail]
  in
  Expect_test_helpers_core.require_does_not_raise (fun () ->
    Fn.ignore @@ Handle.create ~optimize:true (Result_spec.sexp (module Unit)) computation;
    print_endline "Created handle successfully!");
  [%expect {| Created handle successfully! |}]
;;

let run_long_chain_test ~n =
  let computation (local_ _graph) =
    let value = ref (Import.opaque_const_value 0) in
    for _ = 0 to n do
      value := Bonsai.map !value ~f:(( + ) 1)
    done;
    !value
  in
  Handle.create ~optimize:true (Result_spec.sexp (module Int)) computation
;;

let breaking_point = 1018

let%expect_test "long chain of Value.map" =
  (* At values higher than this, we get "node with too large value" *)
  let handle = run_long_chain_test ~n:breaking_point in
  Handle.show handle;
  [%expect {| 1019 |}]
;;

let%expect_test ("js: first long Value.map chain to fail" [@tags "js-only", "no-wasm"]) =
  Expect_test_helpers_base.require_does_raise (fun () ->
    let _ : _ Handle.t = run_long_chain_test ~n:(breaking_point + 1) in
    ());
  [%expect {| ("Stack overflow") |}]
;;

let%expect_test ("wasm: first long Value.map chain to fail" [@tags "wasm-only"]) =
  Expect_test_helpers_base.require_does_raise (fun () ->
    let _ : _ Handle.t = run_long_chain_test ~n:(breaking_point + 1) in
    ());
  [%expect
    {|
    ("node with too large height"
      ((Height 1025)
       (Max    1024))
      lib/incremental/src/adjust_heights_heap.ml:LINE:COL)
    |}]
;;
