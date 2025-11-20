open! Core
module Bonsai_private = Bonsai.Private
open Bonsai_web
open Bonsai_web_test

(* This file is a copy of [test_big_computation_stack_overflows.ml] but it doesn't add any
   interdependencies between the subcomputations, which should hopefully restrict the
   stack overflow to Bonsai, and avoid stack overflows that deep in Incremental. *)

let component ~n (local_ graph) =
  for _ = 0 to n do
    let _ : int Bonsai.t * (int -> unit Bonsai.Effect.t) Bonsai.t =
      Bonsai.state 0 graph
    in
    ()
  done;
  Bonsai.return ()
;;

module%test Stack_overflow_during_graph_application = struct
  let test ~n () =
    let _ : unit Bonsai_private.Computation.t =
      Bonsai_private.top_level_handle (fun (local_ graph) -> component ~n graph)
    in
    ()
  ;;

  let%expect_test ("JSOO - Stack overflow during graph application" [@tags "js-only"]) =
    test ~n:10_000 ()
  ;;
end

module%test Stack_overflow_during_handle_create = struct
  let test ~n () =
    let _ : (unit, never_returns) Handle.t =
      Handle.create Result_spec.invisible (fun (local_ graph) -> component ~n graph)
    in
    ()
  ;;

  let%expect_test ("JSOO - Stack overflow during handle creation" [@tags "js-only"]) =
    test ~n:10_000 ()
  ;;
end
