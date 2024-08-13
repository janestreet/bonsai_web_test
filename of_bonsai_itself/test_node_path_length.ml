open! Core
open Bonsai_web_test
open Bonsai_test_shared_for_testing_bonsai.Big_computation_regression_util
module Node_path = Bonsai.Private.Node_path

let test computation =
  let skeleton =
    Bonsai.Private.Skeleton.Computation.of_computation
      (Bonsai.Private.top_level_handle computation)
  in
  let lengths : int Int.Table.t = Int.Table.create () in
  let seen = String.Hash_set.create () in
  let tally (lazy node_path) =
    let string = Node_path.to_string node_path in
    Hash_set.add seen string;
    Hashtbl.update lengths (String.length string) ~f:(function
      | None -> 1
      | Some x -> x + 1)
  in
  let pre_order_printer =
    object
      inherit Bonsai.Private.Skeleton.Traverse.map as super

      method! value value =
        tally value.node_path;
        super#value value

      method! computation computation =
        tally computation.node_path;
        super#computation computation
    end
  in
  (ignore : Bonsai.Private.Skeleton.Computation.t -> unit)
    (pre_order_printer#computation skeleton);
  print_s [%message "" ~total_unique_seen_node_paths:(Hash_set.length seen : int)];
  Expectable.print
    (Hashtbl.to_alist lengths
     |> List.sort ~compare:(Comparable.lift ~f:fst Int.descending)
     |> List.map ~f:(fun (node_path_length, count) ->
       [%message (node_path_length : int) (count : int)]))
;;

let%test_module "Comparing node path lengths to demonstrate instrumentation slowness" =
  (module struct
    let%expect_test "Proc Syntax" =
      test (For_proc.basic ~height:5 ~width:7);
      [%expect
        {|
        (total_unique_seen_node_paths 479)
        ┌──────────────────┬───────┐
        │ node_path_length │ count │
        ├──────────────────┼───────┤
        │ 37               │  1    │
        │ 36               │  8    │
        │ 35               │  9    │
        │ 34               │ 20    │
        │ 33               │ 36    │
        │ 31               │ 16    │
        │ 30               │  8    │
        │ 29               │ 15    │
        │ 28               │ 20    │
        │ 27               │ 35    │
        │ 25               │ 16    │
        │ 24               │  8    │
        │ 23               │ 15    │
        │ 22               │ 20    │
        │ 21               │ 35    │
        │ 19               │ 16    │
        │ 18               │  8    │
        │ 17               │ 15    │
        │ 16               │ 20    │
        │ 15               │ 35    │
        │ 13               │ 16    │
        │ 12               │  8    │
        │ 11               │ 15    │
        │ 10               │ 20    │
        │  9               │ 35    │
        │  7               │ 16    │
        │  5               │ 10    │
        │  3               │  2    │
        │  2               │  1    │
        └──────────────────┴───────┘
        |}]
    ;;

    let%expect_test "Cont Syntax" =
      test (For_cont.basic ~height:5 ~width:7);
      [%expect
        {|
        (total_unique_seen_node_paths 414)
        ┌──────────────────┬───────┐
        │ node_path_length │ count │
        ├──────────────────┼───────┤
        │ 13               │   2   │
        │ 12               │   4   │
        │ 11               │  21   │
        │ 10               │  70   │
        │  9               │  52   │
        │  8               │ 148   │
        │  7               │ 104   │
        │  5               │  10   │
        │  3               │   2   │
        │  2               │   1   │
        └──────────────────┴───────┘
        |}]
    ;;
  end)
;;

let%test_module "Comparing path id lengths bigger example" =
  (module struct
    let%expect_test "Proc Syntax" =
      test (For_proc.basic ~height:10 ~width:7);
      [%expect
        {|
        (total_unique_seen_node_paths 949)
        ┌──────────────────┬───────┐
        │ node_path_length │ count │
        ├──────────────────┼───────┤
        │ 67               │  1    │
        │ 66               │  8    │
        │ 65               │  9    │
        │ 64               │ 20    │
        │ 63               │ 36    │
        │ 61               │ 16    │
        │ 60               │  8    │
        │ 59               │ 15    │
        │ 58               │ 20    │
        │ 57               │ 35    │
        │ 55               │ 16    │
        │ 54               │  8    │
        │ 53               │ 15    │
        │ 52               │ 20    │
        │ 51               │ 35    │
        │ 49               │ 16    │
        │ 48               │  8    │
        │ 47               │ 15    │
        │ 46               │ 20    │
        │ 45               │ 35    │
        │ 43               │ 16    │
        │ 42               │  8    │
        │ 41               │ 15    │
        │ 40               │ 20    │
        │ 39               │ 35    │
        │ 37               │ 16    │
        │ 36               │  8    │
        │ 35               │ 15    │
        │ 34               │ 20    │
        │ 33               │ 35    │
        │ 31               │ 16    │
        │ 30               │  8    │
        │ 29               │ 15    │
        │ 28               │ 20    │
        │ 27               │ 35    │
        │ 25               │ 16    │
        │ 24               │  8    │
        │ 23               │ 15    │
        │ 22               │ 20    │
        │ 21               │ 35    │
        │ 19               │ 16    │
        │ 18               │  8    │
        │ 17               │ 15    │
        │ 16               │ 20    │
        │ 15               │ 35    │
        │ 13               │ 16    │
        │ 12               │  8    │
        │ 11               │ 15    │
        │ 10               │ 20    │
        │  9               │ 35    │
        │  7               │ 16    │
        │  5               │ 10    │
        │  3               │  2    │
        │  2               │  1    │
        └──────────────────┴───────┘
        |}]
    ;;

    let%expect_test "Cont Syntax" =
      test (For_cont.basic ~height:10 ~width:7);
      [%expect
        {|
        (total_unique_seen_node_paths 819)
        ┌──────────────────┬───────┐
        │ node_path_length │ count │
        ├──────────────────┼───────┤
        │ 13               │   7   │
        │ 12               │   4   │
        │ 11               │ 116   │
        │ 10               │  70   │
        │  9               │ 357   │
        │  8               │ 148   │
        │  7               │ 104   │
        │  5               │  10   │
        │  3               │   2   │
        │  2               │   1   │
        └──────────────────┴───────┘
        |}]
    ;;
  end)
;;
