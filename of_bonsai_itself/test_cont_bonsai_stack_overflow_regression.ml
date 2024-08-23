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
  (* NOTE: This expect test is a regression test against stack overflows in the Bonsai.Cont API. *)
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
  (* NOTE: This expect test is a regression test against stack overflows in the Bonsai.Cont API. *)
  let computation (local_ _graph) =
    let value = ref (Import.opaque_const_value 0) in
    (* At values higher than this, _Incremental_ starts stack overflowing.
       A problem for another day... *)
    for _ = 0 to n do
      value := Bonsai.map !value ~f:(( + ) 1)
    done;
    !value
  in
  Handle.create ~optimize:true (Result_spec.sexp (module Int)) computation
;;

let%expect_test "stack overflow regression test: long chain of Value.map" =
  (* At values higher than this, _Incremental_ starts stack overflowing.  A problem for another day... *)
  let handle = run_long_chain_test ~n:400 in
  Handle.show handle;
  [%expect {| 401 |}]
;;

let%expect_test ("BUG stack overflow regression test: long chain of Value.map" [@tags
                                                                                 "js-only"
                                                                                 , "no-wasm"])
  =
  Expect_test_helpers_base.require_does_raise (fun () ->
    let _ : _ Handle.t = run_long_chain_test ~n:1000 in
    ());
  (* the stack overflow is in Incremental *)
  [%expect {| ("Stack overflow") |}]
;;

let%expect_test ("BUG stack overflow regression test: long chain of Value.map" [@tags
                                                                                 "wasm-only"])
  =
  Expect_test_helpers_base.require_does_not_raise (fun () ->
    let _ : _ Handle.t = run_long_chain_test ~n:1000 in
    ());
  (* the stack overflow is in Incremental *)
  [%expect {| |}]
;;
