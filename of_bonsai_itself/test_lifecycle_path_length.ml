open! Core
open Bonsai_web_test
open Bonsai_test_shared_for_testing_bonsai.Big_computation_regression_util

module%test
  [@name "Comparing path id lengths to demonstrate potential cause of slowness."] _ =
struct
  let%expect_test "Proc Syntax" =
    let handle = Handle.create values_result_spec (For_proc.basic ~height:5 ~width:7) in
    Handle.show handle;
    [%expect
      {|
      (bonsai_path_x bonsai_path_y_x bonsai_path_y_y_x bonsai_path_y_y_y_x
       bonsai_path_y_y_y_y_x bonsai_path_y_y_y_y_y)
      |}];
    let handle = Handle.create lengths_result_spec (For_proc.basic ~height:5 ~width:7) in
    Handle.show handle;
    [%expect {| (13 15 17 19 21 21) |}]
  ;;

  let%expect_test "Cont Syntax" =
    let () =
      try
        let handle =
          Handle.create values_result_spec (For_cont.basic ~height:5 ~width:7)
        in
        Handle.show handle
      with
      | exn ->
        Option.iter (Js_of_ocaml.Js_error.of_exn exn) ~f:(fun js_error ->
          print_endline (Js_of_ocaml.Js_error.to_string js_error))
    in
    (* NOTE: Paths in cont are really big. We believe this is making <app-name> ~50 times
         slower after having been migrated to the CONT API. *)
    [%expect
      {|
      (bonsai_path_x_x bonsai_path_x_y bonsai_path_y_x_x bonsai_path_y_x_y
       bonsai_path_y_y_x bonsai_path_y_y_y)
      |}];
    let _ : _ = [%expect.output] in
    [%expect {| |}];
    let handle = Handle.create lengths_result_spec (For_cont.basic ~height:5 ~width:7) in
    Handle.show handle;
    [%expect {| (15 15 17 17 17 17) |}]
  ;;
end

module%test [@name "Comparing path id lengths bigger example"] _ = struct
  let%expect_test "Proc Syntax" =
    let handle = Handle.create lengths_result_spec (For_proc.basic ~height:10 ~width:7) in
    Handle.show handle;
    [%expect {| (13 15 17 19 21 23 25 27 29 31 31) |}]
  ;;

  let%expect_test "Cont Syntax" =
    let handle = Handle.create lengths_result_spec (For_cont.basic ~height:10 ~width:7) in
    Handle.show handle;
    [%expect {| (17 17 19 19 17 19 19 17 17 19 19) |}]
  ;;
end
