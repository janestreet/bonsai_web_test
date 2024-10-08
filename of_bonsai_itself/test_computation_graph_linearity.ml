open! Core
open Bonsai_web_test
open Bonsai_test_shared_for_testing_bonsai.Big_computation_regression_util

module Bonsai_cont = struct
  include Bonsai.Cont
  module Private = Bonsai.Private
end

let sexp_of_computation
  : type a. ?optimize:bool -> (Bonsai_cont.graph -> a Bonsai_cont.t) -> Sexp.t
  =
  fun ?(optimize = true) c ->
  Bonsai_cont.Private.top_level_handle c
  |> (if optimize then Bonsai_cont.Private.pre_process else Fn.id)
  |> Bonsai_cont.Private.Skeleton.Computation.of_computation
  |> Bonsai_cont.Private.Skeleton.Computation.sanitize_for_testing
  |> Bonsai_cont.Private.Skeleton.Computation.minimal_sexp_of_t
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
           (via (Test 2))
           (into
            (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
             (via (Test 4))
             (into
              (Sub (from Leaf0) (via (Test 5))
               (into
                (Sub
                 (from (Return (value (Mapn (inputs ((Named (uid (Test 5)))))))))
                 (via (Test 7))
                 (into
                  (Sub
                   (from (Return (value (Mapn (inputs ((Named (uid (Test 5)))))))))
                   (via (Test 9))
                   (into
                    (Return
                     (value
                      (Mapn
                       (inputs ((Named (uid (Test 7))) (Named (uid (Test 9)))))))))))))))))))))
       (via (Test 11))
       (into
        (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 11)))))))))
         (via (Test 13))
         (into
          (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 11)))))))))
           (via (Test 15))
           (into
            (Sub
             (from
              (Sub (from Path) (via (Test 16))
               (into (Return (value (Mapn (inputs ((Named (uid (Test 16)))))))))))
             (via (Test 18))
             (into
              (Sub
               (from
                (Sub
                 (from
                  (Sub (from Leaf0) (via (Test 19))
                   (into
                    (Sub
                     (from
                      (Return (value (Mapn (inputs ((Named (uid (Test 19)))))))))
                     (via (Test 21))
                     (into
                      (Sub
                       (from
                        (Return (value (Mapn (inputs ((Named (uid (Test 19)))))))))
                       (via (Test 23))
                       (into
                        (Sub (from Leaf0) (via (Test 24))
                         (into
                          (Sub
                           (from
                            (Return
                             (value (Mapn (inputs ((Named (uid (Test 24)))))))))
                           (via (Test 26))
                           (into
                            (Sub
                             (from
                              (Return
                               (value (Mapn (inputs ((Named (uid (Test 24)))))))))
                             (via (Test 28))
                             (into
                              (Return
                               (value
                                (Mapn
                                 (inputs
                                  ((Named (uid (Test 26))) (Named (uid (Test 28)))))))))))))))))))))
                 (via (Test 30))
                 (into
                  (Sub
                   (from
                    (Return (value (Mapn (inputs ((Named (uid (Test 30)))))))))
                   (via (Test 32))
                   (into
                    (Sub
                     (from
                      (Return (value (Mapn (inputs ((Named (uid (Test 30)))))))))
                     (via (Test 34))
                     (into
                      (Sub
                       (from
                        (Sub (from Path) (via (Test 35))
                         (into
                          (Return
                           (value (Mapn (inputs ((Named (uid (Test 35)))))))))))
                       (via (Test 37))
                       (into
                        (Sub
                         (from
                          (Sub
                           (from
                            (Sub (from Path) (via (Test 38))
                             (into
                              (Return
                               (value (Mapn (inputs ((Named (uid (Test 38)))))))))))
                           (via (Test 40))
                           (into
                            (Return
                             (value (Mapn (inputs ((Named (uid (Test 40)))))))))))
                         (via (Test 42))
                         (into
                          (Return
                           (value
                            (Mapn
                             (inputs
                              ((Named (uid (Test 42))) (Named (uid (Test 37)))))))))))))))))))
               (via (Test 44))
               (into
                (Return
                 (value
                  (Mapn (inputs ((Named (uid (Test 44))) (Named (uid (Test 18))))))))))))))))))
      |}]
  ;;

  let%expect_test "Cont Syntax" =
    print_s (sexp_of_computation (For_cont.basic ~height:2 ~width:2));
    [%expect
      {|
      (Sub (from Leaf0) (via (Test 0))
       (into
        (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
         (via (Test 2))
         (into
          (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
           (via (Test 4))
           (into
            (Sub (from Leaf0) (via (Test 5))
             (into
              (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 5)))))))))
               (via (Test 7))
               (into
                (Sub
                 (from (Return (value (Mapn (inputs ((Named (uid (Test 5)))))))))
                 (via (Test 9))
                 (into
                  (Sub
                   (from
                    (Sub (from Path) (via (Test 10))
                     (into
                      (Return (value (Mapn (inputs ((Named (uid (Test 10)))))))))))
                   (via (Test 12))
                   (into
                    (Sub (from Leaf0) (via (Test 13))
                     (into
                      (Sub
                       (from
                        (Return (value (Mapn (inputs ((Named (uid (Test 13)))))))))
                       (via (Test 15))
                       (into
                        (Sub
                         (from
                          (Return
                           (value (Mapn (inputs ((Named (uid (Test 13)))))))))
                         (via (Test 17))
                         (into
                          (Sub (from Leaf0) (via (Test 18))
                           (into
                            (Sub
                             (from
                              (Return
                               (value (Mapn (inputs ((Named (uid (Test 18)))))))))
                             (via (Test 20))
                             (into
                              (Sub
                               (from
                                (Return
                                 (value (Mapn (inputs ((Named (uid (Test 18)))))))))
                               (via (Test 22))
                               (into
                                (Sub
                                 (from
                                  (Sub (from Path) (via (Test 23))
                                   (into
                                    (Return
                                     (value
                                      (Mapn (inputs ((Named (uid (Test 23)))))))))))
                                 (via (Test 25))
                                 (into
                                  (Sub
                                   (from
                                    (Sub (from Path) (via (Test 26))
                                     (into
                                      (Return
                                       (value
                                        (Mapn (inputs ((Named (uid (Test 26)))))))))))
                                   (via (Test 28))
                                   (into
                                    (Sub
                                     (from
                                      (Return
                                       (value
                                        (Mapn (inputs ((Named (uid (Test 28)))))))))
                                     (via (Test 30))
                                     (into
                                      (Sub
                                       (from
                                        (Return
                                         (value
                                          (Mapn
                                           (inputs
                                            ((Named (uid (Test 30)))
                                             (Named (uid (Test 25)))))))))
                                       (via (Test 32))
                                       (into
                                        (Return
                                         (value
                                          (Mapn
                                           (inputs
                                            ((Named (uid (Test 32)))
                                             (Named (uid (Test 12))))))))))))))))))))))))))))))))))))))))))
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
             (via (Test 2))
             (into
              (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
               (via (Test 4))
               (into
                (Sub (from Leaf0) (via (Test 5))
                 (into
                  (Sub
                   (from (Return (value (Mapn (inputs ((Named (uid (Test 5)))))))))
                   (via (Test 7))
                   (into
                    (Sub
                     (from
                      (Return (value (Mapn (inputs ((Named (uid (Test 5)))))))))
                     (via (Test 9))
                     (into
                      (Return
                       (value
                        (Mapn
                         (inputs ((Named (uid (Test 7))) (Named (uid (Test 9)))))))))))))))))))))
         (via (Test 11))
         (into
          (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 11)))))))))
           (via (Test 13))
           (into
            (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 11)))))))))
             (via (Test 15))
             (into
              (Sub
               (from
                (Sub (from Path) (via (Test 16))
                 (into (Return (value (Mapn (inputs ((Named (uid (Test 16)))))))))))
               (via (Test 18))
               (into
                (Sub
                 (from
                  (Sub
                   (from
                    (Sub (from Path) (via (Test 19))
                     (into
                      (Return (value (Mapn (inputs ((Named (uid (Test 19)))))))))))
                   (via (Test 21))
                   (into
                    (Return (value (Mapn (inputs ((Named (uid (Test 21)))))))))))
                 (via (Test 23))
                 (into
                  (Return
                   (value
                    (Mapn
                     (inputs ((Named (uid (Test 23))) (Named (uid (Test 18)))))))))))))))))))
       (via (Test 25))
       (into
        (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 25)))))))))
         (via (Test 27))
         (into
          (Sub (from (Leaf_incr (input (Named (uid (Test 27)))))) (via (Test 28))
           (into
            (Assoc (map (Named (uid (Test 28)))) (key_id (Test 29))
             (cmp_id (Test 30)) (data_id (Test 31))
             (by
              (Sub
               (from
                (Sub (from Leaf0) (via (Test 32))
                 (into
                  (Sub
                   (from
                    (Return (value (Mapn (inputs ((Named (uid (Test 32)))))))))
                   (via (Test 34))
                   (into
                    (Sub
                     (from
                      (Return (value (Mapn (inputs ((Named (uid (Test 32)))))))))
                     (via (Test 36))
                     (into
                      (Sub (from Leaf0) (via (Test 37))
                       (into
                        (Sub
                         (from
                          (Return
                           (value (Mapn (inputs ((Named (uid (Test 37)))))))))
                         (via (Test 39))
                         (into
                          (Sub
                           (from
                            (Return
                             (value (Mapn (inputs ((Named (uid (Test 37)))))))))
                           (via (Test 41))
                           (into
                            (Return
                             (value
                              (Mapn
                               (inputs
                                ((Named (uid (Test 39))) (Named (uid (Test 41)))))))))))))))))))))
               (via (Test 43))
               (into
                (Sub
                 (from (Return (value (Mapn (inputs ((Named (uid (Test 43)))))))))
                 (via (Test 45))
                 (into
                  (Sub
                   (from
                    (Return (value (Mapn (inputs ((Named (uid (Test 43)))))))))
                   (via (Test 47))
                   (into
                    (Sub
                     (from
                      (Sub (from Path) (via (Test 48))
                       (into
                        (Return (value (Mapn (inputs ((Named (uid (Test 48)))))))))))
                     (via (Test 50))
                     (into
                      (Sub
                       (from
                        (Sub
                         (from
                          (Sub (from Path) (via (Test 51))
                           (into
                            (Return
                             (value (Mapn (inputs ((Named (uid (Test 51)))))))))))
                         (via (Test 53))
                         (into
                          (Return
                           (value (Mapn (inputs ((Named (uid (Test 53)))))))))))
                       (via (Test 55))
                       (into
                        (Return
                         (value
                          (Mapn
                           (inputs
                            ((Named (uid (Test 55))) (Named (uid (Test 50))))))))))))))))))))))))))
      |}]
  ;;

  let%expect_test "Cont Syntax" =
    print_s (sexp_of_computation (For_cont.with_assoc ~height:1 ~width:2 ~num_assocs:5));
    [%expect
      {|
      (Sub (from Leaf0) (via (Test 0))
       (into
        (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
         (via (Test 2))
         (into
          (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
           (via (Test 4))
           (into
            (Sub (from Leaf0) (via (Test 5))
             (into
              (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 5)))))))))
               (via (Test 7))
               (into
                (Sub
                 (from (Return (value (Mapn (inputs ((Named (uid (Test 5)))))))))
                 (via (Test 9))
                 (into
                  (Sub
                   (from
                    (Sub (from Path) (via (Test 10))
                     (into
                      (Return (value (Mapn (inputs ((Named (uid (Test 10)))))))))))
                   (via (Test 12))
                   (into
                    (Sub
                     (from
                      (Sub (from Path) (via (Test 13))
                       (into
                        (Return (value (Mapn (inputs ((Named (uid (Test 13)))))))))))
                     (via (Test 15))
                     (into
                      (Sub
                       (from
                        (Return (value (Mapn (inputs ((Named (uid (Test 15)))))))))
                       (via (Test 17))
                       (into
                        (Sub
                         (from
                          (Return
                           (value
                            (Mapn
                             (inputs
                              ((Named (uid (Test 17))) (Named (uid (Test 12)))))))))
                         (via (Test 19))
                         (into
                          (Sub
                           (from
                            (Return
                             (value (Mapn (inputs ((Named (uid (Test 19)))))))))
                           (via (Test 21))
                           (into
                            (Sub (from (Leaf_incr (input (Named (uid (Test 21))))))
                             (via (Test 22))
                             (into
                              (Assoc (map (Named (uid (Test 22))))
                               (key_id (Test 23)) (cmp_id (Test 24))
                               (data_id (Test 25))
                               (by
                                (Sub (from Leaf0) (via (Test 26))
                                 (into
                                  (Sub
                                   (from
                                    (Return
                                     (value
                                      (Mapn (inputs ((Named (uid (Test 26)))))))))
                                   (via (Test 28))
                                   (into
                                    (Sub
                                     (from
                                      (Return
                                       (value
                                        (Mapn (inputs ((Named (uid (Test 26)))))))))
                                     (via (Test 30))
                                     (into
                                      (Sub (from Leaf0) (via (Test 31))
                                       (into
                                        (Sub
                                         (from
                                          (Return
                                           (value
                                            (Mapn
                                             (inputs ((Named (uid (Test 31)))))))))
                                         (via (Test 33))
                                         (into
                                          (Sub
                                           (from
                                            (Return
                                             (value
                                              (Mapn
                                               (inputs ((Named (uid (Test 31)))))))))
                                           (via (Test 35))
                                           (into
                                            (Sub
                                             (from
                                              (Sub (from Path) (via (Test 36))
                                               (into
                                                (Return
                                                 (value
                                                  (Mapn
                                                   (inputs
                                                    ((Named (uid (Test 36)))))))))))
                                             (via (Test 38))
                                             (into
                                              (Sub
                                               (from
                                                (Sub (from Path) (via (Test 39))
                                                 (into
                                                  (Return
                                                   (value
                                                    (Mapn
                                                     (inputs
                                                      ((Named (uid (Test 39)))))))))))
                                               (via (Test 41))
                                               (into
                                                (Sub
                                                 (from
                                                  (Return
                                                   (value
                                                    (Mapn
                                                     (inputs
                                                      ((Named (uid (Test 41)))))))))
                                                 (via (Test 43))
                                                 (into
                                                  (Return
                                                   (value
                                                    (Mapn
                                                     (inputs
                                                      ((Named (uid (Test 43)))
                                                       (Named (uid (Test 38))))))))))))))))))))))))))))))))))))))))))))))))))))
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
             (via (Test 2))
             (into
              (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
               (via (Test 4))
               (into
                (Sub (from Leaf0) (via (Test 5))
                 (into
                  (Sub
                   (from (Return (value (Mapn (inputs ((Named (uid (Test 5)))))))))
                   (via (Test 7))
                   (into
                    (Sub
                     (from
                      (Return (value (Mapn (inputs ((Named (uid (Test 5)))))))))
                     (via (Test 9))
                     (into
                      (Return
                       (value
                        (Mapn
                         (inputs ((Named (uid (Test 7))) (Named (uid (Test 9)))))))))))))))))))))
         (via (Test 11))
         (into
          (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 11)))))))))
           (via (Test 13))
           (into
            (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 11)))))))))
             (via (Test 15))
             (into
              (Sub
               (from
                (Sub (from Path) (via (Test 16))
                 (into (Return (value (Mapn (inputs ((Named (uid (Test 16)))))))))))
               (via (Test 18))
               (into
                (Sub
                 (from
                  (Sub
                   (from
                    (Sub (from Path) (via (Test 19))
                     (into
                      (Return (value (Mapn (inputs ((Named (uid (Test 19)))))))))))
                   (via (Test 21))
                   (into
                    (Return (value (Mapn (inputs ((Named (uid (Test 21)))))))))))
                 (via (Test 23))
                 (into
                  (Return
                   (value
                    (Mapn
                     (inputs ((Named (uid (Test 23))) (Named (uid (Test 18)))))))))))))))))))
       (via (Test 25))
       (into
        (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 25)))))))))
         (via (Test 27))
         (into
          (Switch (match_ (Mapn (inputs ((Named (uid (Test 27)))))))
           (arms
            ((Return (value (Constant (id (Test 29)))))
             (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 27)))))))))
              (via (Test 31))
              (into (Return (value (Mapn (inputs ((Named (uid (Test 31))))))))))
             (Sub
              (from
               (Sub
                (from
                 (Sub (from Leaf0) (via (Test 33))
                  (into
                   (Sub
                    (from
                     (Return (value (Mapn (inputs ((Named (uid (Test 33)))))))))
                    (via (Test 35))
                    (into
                     (Sub
                      (from
                       (Return (value (Mapn (inputs ((Named (uid (Test 33)))))))))
                      (via (Test 37))
                      (into
                       (Sub (from Leaf0) (via (Test 38))
                        (into
                         (Sub
                          (from
                           (Return
                            (value (Mapn (inputs ((Named (uid (Test 38)))))))))
                          (via (Test 40))
                          (into
                           (Sub
                            (from
                             (Return
                              (value (Mapn (inputs ((Named (uid (Test 38)))))))))
                            (via (Test 42))
                            (into
                             (Return
                              (value
                               (Mapn
                                (inputs
                                 ((Named (uid (Test 40))) (Named (uid (Test 42)))))))))))))))))))))
                (via (Test 44))
                (into
                 (Sub
                  (from (Return (value (Mapn (inputs ((Named (uid (Test 44)))))))))
                  (via (Test 46))
                  (into
                   (Sub
                    (from
                     (Return (value (Mapn (inputs ((Named (uid (Test 44)))))))))
                    (via (Test 48))
                    (into
                     (Sub
                      (from
                       (Sub (from Path) (via (Test 49))
                        (into
                         (Return (value (Mapn (inputs ((Named (uid (Test 49)))))))))))
                      (via (Test 51))
                      (into
                       (Sub
                        (from
                         (Sub
                          (from
                           (Sub (from Path) (via (Test 52))
                            (into
                             (Return
                              (value (Mapn (inputs ((Named (uid (Test 52)))))))))))
                          (via (Test 54))
                          (into
                           (Return
                            (value (Mapn (inputs ((Named (uid (Test 54)))))))))))
                        (via (Test 56))
                        (into
                         (Return
                          (value
                           (Mapn
                            (inputs
                             ((Named (uid (Test 56))) (Named (uid (Test 51)))))))))))))))))))
              (via (Test 58))
              (into (Return (value (Mapn (inputs ((Named (uid (Test 58)))))))))))))))))
      |}]
  ;;

  let%expect_test "Cont Syntax" =
    print_s (sexp_of_computation (For_cont.with_switch ~height:1 ~width:2));
    [%expect
      {|
      (Sub (from Leaf0) (via (Test 0))
       (into
        (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
         (via (Test 2))
         (into
          (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
           (via (Test 4))
           (into
            (Sub (from Leaf0) (via (Test 5))
             (into
              (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 5)))))))))
               (via (Test 7))
               (into
                (Sub
                 (from (Return (value (Mapn (inputs ((Named (uid (Test 5)))))))))
                 (via (Test 9))
                 (into
                  (Sub
                   (from
                    (Sub (from Path) (via (Test 10))
                     (into
                      (Return (value (Mapn (inputs ((Named (uid (Test 10)))))))))))
                   (via (Test 12))
                   (into
                    (Sub
                     (from
                      (Sub (from Path) (via (Test 13))
                       (into
                        (Return (value (Mapn (inputs ((Named (uid (Test 13)))))))))))
                     (via (Test 15))
                     (into
                      (Sub
                       (from
                        (Return (value (Mapn (inputs ((Named (uid (Test 15)))))))))
                       (via (Test 17))
                       (into
                        (Sub
                         (from
                          (Return
                           (value
                            (Mapn
                             (inputs
                              ((Named (uid (Test 17))) (Named (uid (Test 12)))))))))
                         (via (Test 19))
                         (into
                          (Sub
                           (from
                            (Return
                             (value (Mapn (inputs ((Named (uid (Test 19)))))))))
                           (via (Test 21))
                           (into
                            (Sub
                             (from
                              (Return
                               (value (Mapn (inputs ((Named (uid (Test 21)))))))))
                             (via (Test 23))
                             (into
                              (Switch (match_ (Named (uid (Test 23))))
                               (arms
                                ((Return (value (Constant (id (Test 24)))))
                                 (Sub
                                  (from
                                   (Return
                                    (value
                                     (Mapn (inputs ((Named (uid (Test 21)))))))))
                                  (via (Test 26))
                                  (into
                                   (Return
                                    (value
                                     (Mapn (inputs ((Named (uid (Test 26))))))))))
                                 (Sub (from Leaf0) (via (Test 28))
                                  (into
                                   (Sub
                                    (from
                                     (Return
                                      (value
                                       (Mapn (inputs ((Named (uid (Test 28)))))))))
                                    (via (Test 30))
                                    (into
                                     (Sub
                                      (from
                                       (Return
                                        (value
                                         (Mapn (inputs ((Named (uid (Test 28)))))))))
                                      (via (Test 32))
                                      (into
                                       (Sub (from Leaf0) (via (Test 33))
                                        (into
                                         (Sub
                                          (from
                                           (Return
                                            (value
                                             (Mapn
                                              (inputs ((Named (uid (Test 33)))))))))
                                          (via (Test 35))
                                          (into
                                           (Sub
                                            (from
                                             (Return
                                              (value
                                               (Mapn
                                                (inputs ((Named (uid (Test 33)))))))))
                                            (via (Test 37))
                                            (into
                                             (Sub
                                              (from
                                               (Sub (from Path) (via (Test 38))
                                                (into
                                                 (Return
                                                  (value
                                                   (Mapn
                                                    (inputs
                                                     ((Named (uid (Test 38)))))))))))
                                              (via (Test 40))
                                              (into
                                               (Sub
                                                (from
                                                 (Sub (from Path) (via (Test 41))
                                                  (into
                                                   (Return
                                                    (value
                                                     (Mapn
                                                      (inputs
                                                       ((Named (uid (Test 41)))))))))))
                                                (via (Test 43))
                                                (into
                                                 (Sub
                                                  (from
                                                   (Return
                                                    (value
                                                     (Mapn
                                                      (inputs
                                                       ((Named (uid (Test 43)))))))))
                                                  (via (Test 45))
                                                  (into
                                                   (Sub
                                                    (from
                                                     (Return
                                                      (value
                                                       (Mapn
                                                        (inputs
                                                         ((Named (uid (Test 45)))
                                                          (Named (uid (Test 40)))))))))
                                                    (via (Test 47))
                                                    (into
                                                     (Return
                                                      (value
                                                       (Mapn
                                                        (inputs
                                                         ((Named (uid (Test 47)))))))))))))))))))))))))))))))))))))))))))))))))))))))
      |}]
  ;;
end
