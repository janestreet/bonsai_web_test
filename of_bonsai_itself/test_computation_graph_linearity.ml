open! Core
module Bonsai_private = Bonsai.Private
open Bonsai
open Bonsai_web_test
open Bonsai_test_shared_for_testing_bonsai.Big_computation_regression_util

let sexp_of_computation
  : type a. ?optimize:bool -> (local_ Bonsai.graph -> a Bonsai.t) -> Sexp.t
  =
  fun ?(optimize = true) c ->
  Bonsai_private.top_level_handle c
  |> (if optimize then Bonsai_private.pre_process else Fn.id)
  |> Bonsai_private.Skeleton.Computation.of_computation
  |> Bonsai_private.Skeleton.Computation.sanitize_for_testing
  |> Bonsai_private.Skeleton.Computation.minimal_sexp_of_t
;;

module%test [@name "Comparing graph structure."] _ = struct
  let%expect_test "Proc Syntax" =
    print_s (sexp_of_computation (For_proc.basic ~height:2 ~width:2));
    [%expect
      {|
      (Sub
       (from
        (Sub (from Leaf0) (via (Test 0))
         (into
          (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
           (via (Test 1))
           (into
            (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
             (via (Test 2))
             (into
              (Sub (from Leaf0) (via (Test 3))
               (into
                (Sub
                 (from (Return (value (Mapn (inputs ((Named (uid (Test 3)))))))))
                 (via (Test 4))
                 (into
                  (Sub
                   (from (Return (value (Mapn (inputs ((Named (uid (Test 3)))))))))
                   (via (Test 5))
                   (into
                    (Return
                     (value
                      (Mapn
                       (inputs ((Named (uid (Test 4))) (Named (uid (Test 5)))))))))))))))))))))
       (via (Test 6))
       (into
        (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 6)))))))))
         (via (Test 7))
         (into
          (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 6)))))))))
           (via (Test 8))
           (into
            (Sub
             (from
              (Sub (from Path) (via (Test 9))
               (into (Return (value (Mapn (inputs ((Named (uid (Test 9)))))))))))
             (via (Test 10))
             (into
              (Sub
               (from
                (Sub
                 (from
                  (Sub (from Leaf0) (via (Test 11))
                   (into
                    (Sub
                     (from
                      (Return (value (Mapn (inputs ((Named (uid (Test 11)))))))))
                     (via (Test 12))
                     (into
                      (Sub
                       (from
                        (Return (value (Mapn (inputs ((Named (uid (Test 11)))))))))
                       (via (Test 13))
                       (into
                        (Sub (from Leaf0) (via (Test 14))
                         (into
                          (Sub
                           (from
                            (Return
                             (value (Mapn (inputs ((Named (uid (Test 14)))))))))
                           (via (Test 15))
                           (into
                            (Sub
                             (from
                              (Return
                               (value (Mapn (inputs ((Named (uid (Test 14)))))))))
                             (via (Test 16))
                             (into
                              (Return
                               (value
                                (Mapn
                                 (inputs
                                  ((Named (uid (Test 15))) (Named (uid (Test 16)))))))))))))))))))))
                 (via (Test 17))
                 (into
                  (Sub
                   (from
                    (Return (value (Mapn (inputs ((Named (uid (Test 17)))))))))
                   (via (Test 18))
                   (into
                    (Sub
                     (from
                      (Return (value (Mapn (inputs ((Named (uid (Test 17)))))))))
                     (via (Test 19))
                     (into
                      (Sub
                       (from
                        (Sub (from Path) (via (Test 20))
                         (into
                          (Return
                           (value (Mapn (inputs ((Named (uid (Test 20)))))))))))
                       (via (Test 21))
                       (into
                        (Sub
                         (from
                          (Sub
                           (from
                            (Sub (from Path) (via (Test 22))
                             (into
                              (Return
                               (value (Mapn (inputs ((Named (uid (Test 22)))))))))))
                           (via (Test 23))
                           (into
                            (Return
                             (value (Mapn (inputs ((Named (uid (Test 23)))))))))))
                         (via (Test 24))
                         (into
                          (Return
                           (value
                            (Mapn
                             (inputs
                              ((Named (uid (Test 24))) (Named (uid (Test 21)))))))))))))))))))
               (via (Test 25))
               (into
                (Return
                 (value
                  (Mapn (inputs ((Named (uid (Test 25))) (Named (uid (Test 10))))))))))))))))))
      |}]
  ;;

  let%expect_test "Cont Syntax" =
    print_s (sexp_of_computation (For_cont.basic ~height:2 ~width:2));
    [%expect
      {|
      (Sub (from Leaf0) (via (Test 0))
       (into
        (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
         (via (Test 1))
         (into
          (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
           (via (Test 2))
           (into
            (Sub (from Leaf0) (via (Test 3))
             (into
              (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 3)))))))))
               (via (Test 4))
               (into
                (Sub
                 (from (Return (value (Mapn (inputs ((Named (uid (Test 3)))))))))
                 (via (Test 5))
                 (into
                  (Sub
                   (from
                    (Sub (from Path) (via (Test 6))
                     (into
                      (Return (value (Mapn (inputs ((Named (uid (Test 6)))))))))))
                   (via (Test 7))
                   (into
                    (Sub (from Leaf0) (via (Test 8))
                     (into
                      (Sub
                       (from
                        (Return (value (Mapn (inputs ((Named (uid (Test 8)))))))))
                       (via (Test 9))
                       (into
                        (Sub
                         (from
                          (Return (value (Mapn (inputs ((Named (uid (Test 8)))))))))
                         (via (Test 10))
                         (into
                          (Sub (from Leaf0) (via (Test 11))
                           (into
                            (Sub
                             (from
                              (Return
                               (value (Mapn (inputs ((Named (uid (Test 11)))))))))
                             (via (Test 12))
                             (into
                              (Sub
                               (from
                                (Return
                                 (value (Mapn (inputs ((Named (uid (Test 11)))))))))
                               (via (Test 13))
                               (into
                                (Sub
                                 (from
                                  (Sub (from Path) (via (Test 14))
                                   (into
                                    (Return
                                     (value
                                      (Mapn (inputs ((Named (uid (Test 14)))))))))))
                                 (via (Test 15))
                                 (into
                                  (Sub
                                   (from
                                    (Sub (from Path) (via (Test 16))
                                     (into
                                      (Return
                                       (value
                                        (Mapn (inputs ((Named (uid (Test 16)))))))))))
                                   (via (Test 17))
                                   (into
                                    (Sub
                                     (from
                                      (Return
                                       (value
                                        (Mapn (inputs ((Named (uid (Test 17)))))))))
                                     (via (Test 18))
                                     (into
                                      (Sub
                                       (from
                                        (Return
                                         (value
                                          (Mapn
                                           (inputs
                                            ((Named (uid (Test 18)))
                                             (Named (uid (Test 15)))))))))
                                       (via (Test 19))
                                       (into
                                        (Return
                                         (value
                                          (Mapn
                                           (inputs
                                            ((Named (uid (Test 19)))
                                             (Named (uid (Test 7))))))))))))))))))))))))))))))))))))))))))
      |}]
  ;;
end

module%test [@name "With Assocs."] _ = struct
  let%expect_test "Proc Syntax" =
    print_s (sexp_of_computation (For_proc.with_assoc ~height:1 ~width:2 ~num_assocs:5));
    [%expect
      {|
      (Sub
       (from
        (Sub
         (from
          (Sub (from Leaf0) (via (Test 0))
           (into
            (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
             (via (Test 1))
             (into
              (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
               (via (Test 2))
               (into
                (Sub (from Leaf0) (via (Test 3))
                 (into
                  (Sub
                   (from (Return (value (Mapn (inputs ((Named (uid (Test 3)))))))))
                   (via (Test 4))
                   (into
                    (Sub
                     (from
                      (Return (value (Mapn (inputs ((Named (uid (Test 3)))))))))
                     (via (Test 5))
                     (into
                      (Return
                       (value
                        (Mapn
                         (inputs ((Named (uid (Test 4))) (Named (uid (Test 5)))))))))))))))))))))
         (via (Test 6))
         (into
          (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 6)))))))))
           (via (Test 7))
           (into
            (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 6)))))))))
             (via (Test 8))
             (into
              (Sub
               (from
                (Sub (from Path) (via (Test 9))
                 (into (Return (value (Mapn (inputs ((Named (uid (Test 9)))))))))))
               (via (Test 10))
               (into
                (Sub
                 (from
                  (Sub
                   (from
                    (Sub (from Path) (via (Test 11))
                     (into
                      (Return (value (Mapn (inputs ((Named (uid (Test 11)))))))))))
                   (via (Test 12))
                   (into
                    (Return (value (Mapn (inputs ((Named (uid (Test 12)))))))))))
                 (via (Test 13))
                 (into
                  (Return
                   (value
                    (Mapn
                     (inputs ((Named (uid (Test 13))) (Named (uid (Test 10)))))))))))))))))))
       (via (Test 14))
       (into
        (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 14)))))))))
         (via (Test 15))
         (into
          (Sub (from (Leaf_incr (input (Named (uid (Test 15)))))) (via (Test 16))
           (into
            (Assoc (map (Named (uid (Test 16)))) (key_id (Test 17))
             (cmp_id (Test 18)) (data_id (Test 19))
             (by
              (Sub
               (from
                (Sub (from Leaf0) (via (Test 20))
                 (into
                  (Sub
                   (from
                    (Return (value (Mapn (inputs ((Named (uid (Test 20)))))))))
                   (via (Test 21))
                   (into
                    (Sub
                     (from
                      (Return (value (Mapn (inputs ((Named (uid (Test 20)))))))))
                     (via (Test 22))
                     (into
                      (Sub (from Leaf0) (via (Test 23))
                       (into
                        (Sub
                         (from
                          (Return
                           (value (Mapn (inputs ((Named (uid (Test 23)))))))))
                         (via (Test 24))
                         (into
                          (Sub
                           (from
                            (Return
                             (value (Mapn (inputs ((Named (uid (Test 23)))))))))
                           (via (Test 25))
                           (into
                            (Return
                             (value
                              (Mapn
                               (inputs
                                ((Named (uid (Test 24))) (Named (uid (Test 25)))))))))))))))))))))
               (via (Test 26))
               (into
                (Sub
                 (from (Return (value (Mapn (inputs ((Named (uid (Test 26)))))))))
                 (via (Test 27))
                 (into
                  (Sub
                   (from
                    (Return (value (Mapn (inputs ((Named (uid (Test 26)))))))))
                   (via (Test 28))
                   (into
                    (Sub
                     (from
                      (Sub (from Path) (via (Test 29))
                       (into
                        (Return (value (Mapn (inputs ((Named (uid (Test 29)))))))))))
                     (via (Test 30))
                     (into
                      (Sub
                       (from
                        (Sub
                         (from
                          (Sub (from Path) (via (Test 31))
                           (into
                            (Return
                             (value (Mapn (inputs ((Named (uid (Test 31)))))))))))
                         (via (Test 32))
                         (into
                          (Return
                           (value (Mapn (inputs ((Named (uid (Test 32)))))))))))
                       (via (Test 33))
                       (into
                        (Return
                         (value
                          (Mapn
                           (inputs
                            ((Named (uid (Test 33))) (Named (uid (Test 30))))))))))))))))))))))))))
      |}]
  ;;

  let%expect_test "Cont Syntax" =
    print_s (sexp_of_computation (For_cont.with_assoc ~height:1 ~width:2 ~num_assocs:5));
    [%expect
      {|
      (Sub (from Leaf0) (via (Test 0))
       (into
        (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
         (via (Test 1))
         (into
          (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
           (via (Test 2))
           (into
            (Sub (from Leaf0) (via (Test 3))
             (into
              (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 3)))))))))
               (via (Test 4))
               (into
                (Sub
                 (from (Return (value (Mapn (inputs ((Named (uid (Test 3)))))))))
                 (via (Test 5))
                 (into
                  (Sub
                   (from
                    (Sub (from Path) (via (Test 6))
                     (into
                      (Return (value (Mapn (inputs ((Named (uid (Test 6)))))))))))
                   (via (Test 7))
                   (into
                    (Sub
                     (from
                      (Sub (from Path) (via (Test 8))
                       (into
                        (Return (value (Mapn (inputs ((Named (uid (Test 8)))))))))))
                     (via (Test 9))
                     (into
                      (Sub
                       (from
                        (Return (value (Mapn (inputs ((Named (uid (Test 9)))))))))
                       (via (Test 10))
                       (into
                        (Sub
                         (from
                          (Return
                           (value
                            (Mapn
                             (inputs
                              ((Named (uid (Test 10))) (Named (uid (Test 7)))))))))
                         (via (Test 11))
                         (into
                          (Sub
                           (from
                            (Return
                             (value (Mapn (inputs ((Named (uid (Test 11)))))))))
                           (via (Test 12))
                           (into
                            (Sub (from (Leaf_incr (input (Named (uid (Test 12))))))
                             (via (Test 13))
                             (into
                              (Assoc (map (Named (uid (Test 13))))
                               (key_id (Test 14)) (cmp_id (Test 15))
                               (data_id (Test 16))
                               (by
                                (Sub (from Leaf0) (via (Test 17))
                                 (into
                                  (Sub
                                   (from
                                    (Return
                                     (value
                                      (Mapn (inputs ((Named (uid (Test 17)))))))))
                                   (via (Test 18))
                                   (into
                                    (Sub
                                     (from
                                      (Return
                                       (value
                                        (Mapn (inputs ((Named (uid (Test 17)))))))))
                                     (via (Test 19))
                                     (into
                                      (Sub (from Leaf0) (via (Test 20))
                                       (into
                                        (Sub
                                         (from
                                          (Return
                                           (value
                                            (Mapn
                                             (inputs ((Named (uid (Test 20)))))))))
                                         (via (Test 21))
                                         (into
                                          (Sub
                                           (from
                                            (Return
                                             (value
                                              (Mapn
                                               (inputs ((Named (uid (Test 20)))))))))
                                           (via (Test 22))
                                           (into
                                            (Sub
                                             (from
                                              (Sub (from Path) (via (Test 23))
                                               (into
                                                (Return
                                                 (value
                                                  (Mapn
                                                   (inputs
                                                    ((Named (uid (Test 23)))))))))))
                                             (via (Test 24))
                                             (into
                                              (Sub
                                               (from
                                                (Sub (from Path) (via (Test 25))
                                                 (into
                                                  (Return
                                                   (value
                                                    (Mapn
                                                     (inputs
                                                      ((Named (uid (Test 25)))))))))))
                                               (via (Test 26))
                                               (into
                                                (Sub
                                                 (from
                                                  (Return
                                                   (value
                                                    (Mapn
                                                     (inputs
                                                      ((Named (uid (Test 26)))))))))
                                                 (via (Test 27))
                                                 (into
                                                  (Return
                                                   (value
                                                    (Mapn
                                                     (inputs
                                                      ((Named (uid (Test 27)))
                                                       (Named (uid (Test 24))))))))))))))))))))))))))))))))))))))))))))))))))))
      |}]
  ;;
end

module%test [@name "With match%sub."] _ = struct
  let%expect_test "Proc Syntax" =
    print_s (sexp_of_computation (For_proc.with_switch ~height:1 ~width:2));
    [%expect
      {|
      (Sub
       (from
        (Sub
         (from
          (Sub (from Leaf0) (via (Test 0))
           (into
            (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
             (via (Test 1))
             (into
              (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
               (via (Test 2))
               (into
                (Sub (from Leaf0) (via (Test 3))
                 (into
                  (Sub
                   (from (Return (value (Mapn (inputs ((Named (uid (Test 3)))))))))
                   (via (Test 4))
                   (into
                    (Sub
                     (from
                      (Return (value (Mapn (inputs ((Named (uid (Test 3)))))))))
                     (via (Test 5))
                     (into
                      (Return
                       (value
                        (Mapn
                         (inputs ((Named (uid (Test 4))) (Named (uid (Test 5)))))))))))))))))))))
         (via (Test 6))
         (into
          (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 6)))))))))
           (via (Test 7))
           (into
            (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 6)))))))))
             (via (Test 8))
             (into
              (Sub
               (from
                (Sub (from Path) (via (Test 9))
                 (into (Return (value (Mapn (inputs ((Named (uid (Test 9)))))))))))
               (via (Test 10))
               (into
                (Sub
                 (from
                  (Sub
                   (from
                    (Sub (from Path) (via (Test 11))
                     (into
                      (Return (value (Mapn (inputs ((Named (uid (Test 11)))))))))))
                   (via (Test 12))
                   (into
                    (Return (value (Mapn (inputs ((Named (uid (Test 12)))))))))))
                 (via (Test 13))
                 (into
                  (Return
                   (value
                    (Mapn
                     (inputs ((Named (uid (Test 13))) (Named (uid (Test 10)))))))))))))))))))
       (via (Test 14))
       (into
        (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 14)))))))))
         (via (Test 15))
         (into
          (Switch (match_ (Mapn (inputs ((Named (uid (Test 15)))))))
           (arms
            ((Return (value Constant))
             (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 15)))))))))
              (via (Test 16))
              (into (Return (value (Mapn (inputs ((Named (uid (Test 16))))))))))
             (Sub
              (from
               (Sub
                (from
                 (Sub (from Leaf0) (via (Test 17))
                  (into
                   (Sub
                    (from
                     (Return (value (Mapn (inputs ((Named (uid (Test 17)))))))))
                    (via (Test 18))
                    (into
                     (Sub
                      (from
                       (Return (value (Mapn (inputs ((Named (uid (Test 17)))))))))
                      (via (Test 19))
                      (into
                       (Sub (from Leaf0) (via (Test 20))
                        (into
                         (Sub
                          (from
                           (Return
                            (value (Mapn (inputs ((Named (uid (Test 20)))))))))
                          (via (Test 21))
                          (into
                           (Sub
                            (from
                             (Return
                              (value (Mapn (inputs ((Named (uid (Test 20)))))))))
                            (via (Test 22))
                            (into
                             (Return
                              (value
                               (Mapn
                                (inputs
                                 ((Named (uid (Test 21))) (Named (uid (Test 22)))))))))))))))))))))
                (via (Test 23))
                (into
                 (Sub
                  (from (Return (value (Mapn (inputs ((Named (uid (Test 23)))))))))
                  (via (Test 24))
                  (into
                   (Sub
                    (from
                     (Return (value (Mapn (inputs ((Named (uid (Test 23)))))))))
                    (via (Test 25))
                    (into
                     (Sub
                      (from
                       (Sub (from Path) (via (Test 26))
                        (into
                         (Return (value (Mapn (inputs ((Named (uid (Test 26)))))))))))
                      (via (Test 27))
                      (into
                       (Sub
                        (from
                         (Sub
                          (from
                           (Sub (from Path) (via (Test 28))
                            (into
                             (Return
                              (value (Mapn (inputs ((Named (uid (Test 28)))))))))))
                          (via (Test 29))
                          (into
                           (Return
                            (value (Mapn (inputs ((Named (uid (Test 29)))))))))))
                        (via (Test 30))
                        (into
                         (Return
                          (value
                           (Mapn
                            (inputs
                             ((Named (uid (Test 30))) (Named (uid (Test 27)))))))))))))))))))
              (via (Test 31))
              (into (Return (value (Mapn (inputs ((Named (uid (Test 31)))))))))))))))))
      |}]
  ;;

  let%expect_test "Cont Syntax" =
    print_s (sexp_of_computation (For_cont.with_switch ~height:1 ~width:2));
    [%expect
      {|
      (Sub (from Leaf0) (via (Test 0))
       (into
        (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
         (via (Test 1))
         (into
          (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
           (via (Test 2))
           (into
            (Sub (from Leaf0) (via (Test 3))
             (into
              (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 3)))))))))
               (via (Test 4))
               (into
                (Sub
                 (from (Return (value (Mapn (inputs ((Named (uid (Test 3)))))))))
                 (via (Test 5))
                 (into
                  (Sub
                   (from
                    (Sub (from Path) (via (Test 6))
                     (into
                      (Return (value (Mapn (inputs ((Named (uid (Test 6)))))))))))
                   (via (Test 7))
                   (into
                    (Sub
                     (from
                      (Sub (from Path) (via (Test 8))
                       (into
                        (Return (value (Mapn (inputs ((Named (uid (Test 8)))))))))))
                     (via (Test 9))
                     (into
                      (Sub
                       (from
                        (Return (value (Mapn (inputs ((Named (uid (Test 9)))))))))
                       (via (Test 10))
                       (into
                        (Sub
                         (from
                          (Return
                           (value
                            (Mapn
                             (inputs
                              ((Named (uid (Test 10))) (Named (uid (Test 7)))))))))
                         (via (Test 11))
                         (into
                          (Sub
                           (from
                            (Return
                             (value (Mapn (inputs ((Named (uid (Test 11)))))))))
                           (via (Test 12))
                           (into
                            (Sub
                             (from
                              (Return
                               (value (Mapn (inputs ((Named (uid (Test 12)))))))))
                             (via (Test 13))
                             (into
                              (Switch (match_ (Named (uid (Test 13))))
                               (arms
                                ((Return (value Constant))
                                 (Sub
                                  (from
                                   (Return
                                    (value
                                     (Mapn (inputs ((Named (uid (Test 12)))))))))
                                  (via (Test 14))
                                  (into
                                   (Return
                                    (value
                                     (Mapn (inputs ((Named (uid (Test 14))))))))))
                                 (Sub (from Leaf0) (via (Test 15))
                                  (into
                                   (Sub
                                    (from
                                     (Return
                                      (value
                                       (Mapn (inputs ((Named (uid (Test 15)))))))))
                                    (via (Test 16))
                                    (into
                                     (Sub
                                      (from
                                       (Return
                                        (value
                                         (Mapn (inputs ((Named (uid (Test 15)))))))))
                                      (via (Test 17))
                                      (into
                                       (Sub (from Leaf0) (via (Test 18))
                                        (into
                                         (Sub
                                          (from
                                           (Return
                                            (value
                                             (Mapn
                                              (inputs ((Named (uid (Test 18)))))))))
                                          (via (Test 19))
                                          (into
                                           (Sub
                                            (from
                                             (Return
                                              (value
                                               (Mapn
                                                (inputs ((Named (uid (Test 18)))))))))
                                            (via (Test 20))
                                            (into
                                             (Sub
                                              (from
                                               (Sub (from Path) (via (Test 21))
                                                (into
                                                 (Return
                                                  (value
                                                   (Mapn
                                                    (inputs
                                                     ((Named (uid (Test 21)))))))))))
                                              (via (Test 22))
                                              (into
                                               (Sub
                                                (from
                                                 (Sub (from Path) (via (Test 23))
                                                  (into
                                                   (Return
                                                    (value
                                                     (Mapn
                                                      (inputs
                                                       ((Named (uid (Test 23)))))))))))
                                                (via (Test 24))
                                                (into
                                                 (Sub
                                                  (from
                                                   (Return
                                                    (value
                                                     (Mapn
                                                      (inputs
                                                       ((Named (uid (Test 24)))))))))
                                                  (via (Test 25))
                                                  (into
                                                   (Sub
                                                    (from
                                                     (Return
                                                      (value
                                                       (Mapn
                                                        (inputs
                                                         ((Named (uid (Test 25)))
                                                          (Named (uid (Test 22)))))))))
                                                    (via (Test 26))
                                                    (into
                                                     (Return
                                                      (value
                                                       (Mapn
                                                        (inputs
                                                         ((Named (uid (Test 26)))))))))))))))))))))))))))))))))))))))))))))))))))))))
      |}]
  ;;
end
