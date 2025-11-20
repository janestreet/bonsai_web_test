open! Core
open! Bonsai_web
module Poll_result = Rpc_effect.Poll_result

let timestamp = Time_ns.epoch
let error = Error.of_string "example_error"
let add_seconds secs = Time_ns.add timestamp (Time_ns.Span.of_sec secs)

(** [bisimulate_with_without_details] prints the standard type along with the
    [_with_details] variant in a compact within the respective labeled argument function *)
let bisimulate_with_without_details print1 print2 f =
  f print1 ~expect_diff:(fun ~without_details ~with_details:_ -> without_details ());
  f print2 ~expect_diff:(fun ~without_details:_ ~with_details -> with_details ())
;;

module%test [@name "Output_type.Abstract"] _ = struct
  let%expect_test "abstract returns the poll result itself" =
    let t =
      Poll_result.For_testing.create
        ~last_ok_response:(1, "foo", timestamp)
        ~last_error:(2, error, timestamp)
        ~inflight_query:(3, timestamp)
        ()
    in
    (* Sexp representation of [t] is nice and succinct, minus timestamps which are kind of
       noisy *)
    let sexp_of = [%sexp_of: (int, string) Poll_result.t] in
    print_s (sexp_of t);
    [%expect
      {|
      ((last_ok_response (1 foo "1970-01-01 00:00:00Z"))
       (last_error (2 example_error "1970-01-01 00:00:00Z"))
       (inflight_query (3 "1970-01-01 00:00:00Z")))
      |}];
    (* Getting ~output_type:Abstract is a no-op. We can't compare [Poll_result.t]s for
       equality, so we compare their sexps *)
    let t' = Poll_result.get_output t ~output_type:Abstract in
    Expect_test_helpers_base.require_equal (module Sexp) (sexp_of t) (sexp_of t');
    (* We can use it to get arbitrary outputs *)
    let response = Poll_result.get_output t' ~output_type:Last_ok_response in
    print_s [%message (response : string option)];
    [%expect {| (response (foo)) |}]
  ;;
end

module%test [@name "Output_type.Pending_or_error"] _ = struct
  let print_pending_or_error t =
    let result = Poll_result.get_output t ~output_type:Pending_or_error in
    print_s [%message (result : string Poll_result.Pending_or_error.t)]
  ;;

  let%expect_test "pending when there's no response yet" =
    Poll_result.empty |> print_pending_or_error;
    [%expect {| (result Pending) |}];
    Poll_result.For_testing.create
      ~inflight_query:(1, timestamp)
      ~equal_query:[%equal: int]
      ()
    |> print_pending_or_error;
    [%expect {| (result Pending) |}]
  ;;

  let%expect_test "pending when the query changes" =
    Poll_result.For_testing.create
      ~last_ok_response:(1, "foo", timestamp)
      ~inflight_query:(2, timestamp)
      ~equal_query:[%equal: int]
      ()
    |> print_pending_or_error;
    [%expect {| (result Pending) |}];
    Poll_result.For_testing.create
      ~last_ok_response:(2, "foo", timestamp)
      ~last_error:(1, error, timestamp)
      ~inflight_query:(2, timestamp)
      ~equal_query:[%equal: int]
      ()
    |> print_pending_or_error;
    [%expect {| (result Pending) |}]
  ;;

  let%expect_test "not pending when the query hasn't changed" =
    Poll_result.For_testing.create
      ~last_ok_response:(1, "foo", timestamp)
      ~inflight_query:(1, timestamp)
      ~equal_query:[%equal: int]
      ()
    |> print_pending_or_error;
    [%expect {| (result (Ok foo)) |}];
    Poll_result.For_testing.create
      ~last_ok_response:(1, "foo", add_seconds 0.)
      ~last_error:(2, error, add_seconds 1.)
      ~inflight_query:(2, add_seconds 2.)
      ~equal_query:[%equal: int]
      ()
    |> print_pending_or_error;
    [%expect {| (result (Error example_error)) |}]
  ;;

  let%expect_test "ok when last response was ok" =
    Poll_result.For_testing.create ~last_ok_response:(1, "foo", timestamp) ()
    |> print_pending_or_error;
    [%expect {| (result (Ok foo)) |}]
  ;;

  let%expect_test "error when last response was error" =
    Poll_result.For_testing.create
      ~last_ok_response:(1, "foo", add_seconds 0.)
      ~last_error:(1, error, add_seconds 1.)
      ()
    |> print_pending_or_error;
    [%expect {| (result (Error example_error)) |}]
  ;;
end

module%test [@name "Output_type.Response_state[with_details]"] _ = struct
  let print_response_state t =
    let result = Poll_result.get_output t ~output_type:Response_state in
    print_s [%message (result : string Poll_result.Response_state.t)]
  ;;

  let print_response_state_with_details t =
    let result = Poll_result.get_output t ~output_type:Response_state_with_details in
    print_s [%message (result : (int, string) Poll_result.Response_state_with_details.t)]
  ;;

  let () =
    bisimulate_with_without_details print_response_state print_response_state_with_details
    @@ fun print_response_state ~expect_diff ->
    let module _ = struct
      let%expect_test "no response yet cases" =
        Poll_result.empty |> print_response_state;
        [%expect {| (result No_response_yet) |}];
        Poll_result.For_testing.create
          ~inflight_query:(1, timestamp)
          ~equal_query:[%equal: int]
          ()
        |> print_response_state;
        [%expect {| (result No_response_yet) |}]
      ;;

      module%test [@name "Ok cases"] _ = struct
        let%expect_test "only ok response" =
          Poll_result.For_testing.create ~last_ok_response:(1, "foo", timestamp) ()
          |> print_response_state;
          expect_diff
            ~without_details:(fun () -> [%expect {| (result (Ok foo)) |}])
            ~with_details:(fun () ->
              [%expect
                {|
                (result
                 (Ok ((response foo) (got_response_at "1970-01-01 00:00:00Z") (query 1))))
                |}])
        ;;

        let%expect_test "with inflight query" =
          Poll_result.For_testing.create
            ~last_ok_response:(1, "foo", add_seconds 0.)
            ~inflight_query:(2, add_seconds 1.)
            ~equal_query:[%equal: int]
            ()
          |> print_response_state;
          (* Note that this is different from Pending_or_error, which would be Pending
             here. *)
          expect_diff
            ~without_details:(fun () -> [%expect {| (result (Ok foo)) |}])
            ~with_details:(fun () ->
              [%expect
                {|
                (result
                 (Ok ((response foo) (got_response_at "1970-01-01 00:00:00Z") (query 1))))
                |}])
        ;;
      end

      module%test [@name "Error cases"] _ = struct
        let%expect_test "only error" =
          Poll_result.For_testing.create ~last_error:(1, error, timestamp) ()
          |> print_response_state;
          expect_diff
            ~without_details:(fun () ->
              [%expect {| (result (Error (error example_error) (last_ok_response ()))) |}])
            ~with_details:(fun () ->
              [%expect
                {|
                (result
                 (Error (error example_error) (got_error_at "1970-01-01 00:00:00Z") (query 1)
                  (last_ok_response ())))
                |}])
        ;;

        let%expect_test "with last ok response" =
          Poll_result.For_testing.create
            ~last_ok_response:(1, "foo", add_seconds 0.)
            ~last_error:(2, error, add_seconds 1.)
            ()
          |> print_response_state;
          expect_diff
            ~without_details:(fun () ->
              [%expect
                {| (result (Error (error example_error) (last_ok_response (foo)))) |}])
            ~with_details:(fun () ->
              [%expect
                {|
                (result
                 (Error (error example_error) (got_error_at "1970-01-01 00:00:01Z") (query 2)
                  (last_ok_response
                   (((response foo) (got_response_at "1970-01-01 00:00:00Z") (query 1))))))
                |}])
        ;;

        let%expect_test "with inflight query" =
          Poll_result.For_testing.create
            ~last_error:(1, error, add_seconds 0.)
            ~inflight_query:(2, add_seconds 1.)
            ~equal_query:[%equal: int]
            ()
          |> print_response_state;
          expect_diff
            ~without_details:(fun () ->
              [%expect {| (result (Error (error example_error) (last_ok_response ()))) |}])
            ~with_details:(fun () ->
              [%expect
                {|
                (result
                 (Error (error example_error) (got_error_at "1970-01-01 00:00:00Z") (query 1)
                  (last_ok_response ())))
                |}])
        ;;

        let%expect_test "with inflight query and ok response" =
          Poll_result.For_testing.create
            ~last_ok_response:(1, "foo", add_seconds 0.)
            ~last_error:(2, error, add_seconds 1.)
            ~inflight_query:(3, add_seconds 2.)
            ~equal_query:[%equal: int]
            ()
          |> print_response_state;
          expect_diff
            ~without_details:(fun () ->
              [%expect
                {| (result (Error (error example_error) (last_ok_response (foo)))) |}])
            ~with_details:(fun () ->
              [%expect
                {|
                (result
                 (Error (error example_error) (got_error_at "1970-01-01 00:00:01Z") (query 2)
                  (last_ok_response
                   (((response foo) (got_response_at "1970-01-01 00:00:00Z") (query 1))))))
                |}])
        ;;
      end
    end
    in
    ()
  ;;
end

module%test [@name "Output_type.Legacy_record"] _ = struct
  let%expect_test "all fields populated" =
    let t =
      Poll_result.For_testing.create
        ~last_ok_response:(1, "foo", add_seconds 0.)
        ~last_error:(2, error, add_seconds 1.)
        ~inflight_query:(3, add_seconds 2.)
        ()
    in
    let result = Poll_result.get_output t ~output_type:Legacy_record in
    print_s [%message (result : (int, string) Poll_result.Legacy_record.t)];
    [%expect
      {|
      (result
       ((last_ok_response ((1 foo))) (last_error ((2 example_error)))
        (inflight_query (3)) (refresh <opaque>)))
      |}]
  ;;

  let%expect_test "empty record" =
    let t = Poll_result.empty in
    let result = Poll_result.get_output t ~output_type:Legacy_record in
    print_s [%message (result : (int, string) Poll_result.Legacy_record.t)];
    [%expect
      {|
      (result
       ((last_ok_response ()) (last_error ()) (inflight_query ())
        (refresh <opaque>)))
      |}]
  ;;
end

module%test [@name "Output_type.Raw_representation"] _ = struct
  let%expect_test "raw representation is identical to abstract type" =
    let t =
      Poll_result.For_testing.create
        ~last_ok_response:(1, "foo", add_seconds 0.)
        ~last_error:(2, error, add_seconds 1.)
        ~inflight_query:(3, add_seconds 2.)
        ()
    in
    let raw = Poll_result.get_output t ~output_type:Raw_representation in
    Expect_test_helpers_base.require_equal
      (module Sexp)
      ([%sexp_of: (int, string) Poll_result.t] t)
      ([%sexp_of: (int, string) Poll_result.Raw_representation.t] raw);
    [%expect {| |}]
  ;;
end

module%test [@name "Output_type.Last_query"] _ = struct
  let%expect_test "last query is Ok" =
    let t = Poll_result.For_testing.create ~last_ok_response:(5, "foo", timestamp) () in
    let last_query = Poll_result.get_output t ~output_type:Last_query in
    print_s [%message (last_query : int option)];
    [%expect {| (last_query (5)) |}]
  ;;

  let%expect_test "last query is Error" =
    let t =
      Poll_result.For_testing.create
        ~last_ok_response:(5, "foo", add_seconds 0.)
        ~last_error:(6, Error.of_string "bar", add_seconds 1.)
        ()
    in
    let last_query = Poll_result.get_output t ~output_type:Last_query in
    print_s [%message (last_query : int option)];
    [%expect {| (last_query (6)) |}]
  ;;

  let%expect_test "inflight query doesn't affect last query" =
    let t =
      Poll_result.For_testing.create
        ~last_ok_response:(5, "foo", add_seconds 0.)
        ~last_error:(6, Error.of_string "bar", add_seconds 1.)
        ~inflight_query:(7, add_seconds 2.)
        ()
    in
    let last_query = Poll_result.get_output t ~output_type:Last_query in
    print_s [%message (last_query : int option)];
    [%expect {| (last_query (6)) |}]
  ;;
end

module%test [@name "Output_type.Fetching_status[_with_details]"] _ = struct
  let print_fetching_status t =
    let status = Poll_result.get_output t ~output_type:Fetching_status in
    print_s [%message (status : int Poll_result.Fetching_status.t)]
  ;;

  let print_fetching_status_with_details t =
    let status = Poll_result.get_output t ~output_type:Fetching_status_with_details in
    print_s
      [%message (status : (int * Time_ns.Alternate_sexp.t) Poll_result.Fetching_status.t)]
  ;;

  let () =
    bisimulate_with_without_details
      print_fetching_status
      print_fetching_status_with_details
    @@ fun print_fetching_status ~expect_diff ->
    let module _ = struct
      let%expect_test "not fetching when no inflight query" =
        Poll_result.For_testing.create ~last_ok_response:(1, "foo", timestamp) ()
        |> print_fetching_status;
        [%expect {| (status Not_fetching) |}]
      ;;

      let%expect_test "fetching with first query" =
        Poll_result.For_testing.create
          ~inflight_query:(1, timestamp)
          ~equal_query:[%equal: int]
          ()
        |> print_fetching_status;
        expect_diff
          ~without_details:(fun () ->
            [%expect
              {|
              (status
               (Fetching (query 1) (query_changed true) (query_changed_since_last_ok true)))
              |}])
          ~with_details:(fun () ->
            [%expect
              {|
              (status
               (Fetching (query (1 "1970-01-01 00:00:00Z")) (query_changed true)
                (query_changed_since_last_ok true)))
              |}])
      ;;

      let%expect_test "fetching with unchanged query" =
        Poll_result.For_testing.create
          ~last_ok_response:(1, "foo", add_seconds 0.)
          ~inflight_query:(1, add_seconds 1.)
          ~equal_query:[%equal: int]
          ()
        |> print_fetching_status;
        expect_diff
          ~without_details:(fun () ->
            [%expect
              {|
              (status
               (Fetching (query 1) (query_changed false)
                (query_changed_since_last_ok false)))
              |}])
          ~with_details:(fun () ->
            [%expect
              {|
              (status
               (Fetching (query (1 "1970-01-01 00:00:01Z")) (query_changed false)
                (query_changed_since_last_ok false)))
              |}])
      ;;

      let%expect_test "fetching with changed query" =
        Poll_result.For_testing.create
          ~last_ok_response:(1, "foo", add_seconds 0.)
          ~inflight_query:(2, add_seconds 1.)
          ~equal_query:[%equal: int]
          ()
        |> print_fetching_status;
        expect_diff
          ~without_details:(fun () ->
            [%expect
              {|
              (status
               (Fetching (query 2) (query_changed true) (query_changed_since_last_ok true)))
              |}])
          ~with_details:(fun () ->
            [%expect
              {|
              (status
               (Fetching (query (2 "1970-01-01 00:00:01Z")) (query_changed true)
                (query_changed_since_last_ok true)))
              |}])
      ;;

      let%expect_test "fetching with query that's changed since last ok response but not \
                       last response"
        =
        Poll_result.For_testing.create
          ~last_ok_response:(1, "foo", add_seconds 0.)
          ~last_error:(2, error, add_seconds 1.)
          ~inflight_query:(2, add_seconds 2.)
          ~equal_query:[%equal: int]
          ()
        |> print_fetching_status;
        expect_diff
          ~without_details:(fun () ->
            [%expect
              {|
              (status
               (Fetching (query 2) (query_changed false)
                (query_changed_since_last_ok true)))
              |}])
          ~with_details:(fun () ->
            [%expect
              {|
              (status
               (Fetching (query (2 "1970-01-01 00:00:02Z")) (query_changed false)
                (query_changed_since_last_ok true)))
              |}])
      ;;

      let%expect_test "fetching with query that's changed since last response but not \
                       last ok response"
        =
        Poll_result.For_testing.create
          ~last_ok_response:(2, "foo", add_seconds 0.)
          ~last_error:(1, error, add_seconds 1.)
          ~inflight_query:(2, add_seconds 2.)
          ~equal_query:[%equal: int]
          ()
        |> print_fetching_status;
        expect_diff
          ~without_details:(fun () ->
            [%expect
              {|
              (status
               (Fetching (query 2) (query_changed true)
                (query_changed_since_last_ok false)))
              |}])
          ~with_details:(fun () ->
            [%expect
              {|
              (status
               (Fetching (query (2 "1970-01-01 00:00:02Z")) (query_changed true)
                (query_changed_since_last_ok false)))
              |}])
      ;;
    end
    in
    ()
  ;;
end

module%test [@name "Output_type.Error[_with_details]"] _ = struct
  let print_error t =
    let error = Poll_result.get_output t ~output_type:Error in
    print_s [%message (error : Error.t option)]
  ;;

  let print_error_with_details t =
    let error = Poll_result.get_output t ~output_type:Error_with_details in
    print_s [%message (error : (int * Error.t * Time_ns.Alternate_sexp.t) option)]
  ;;

  let () =
    bisimulate_with_without_details print_error print_error_with_details
    @@ fun print_error ~expect_diff ->
    let module _ = struct
      let%expect_test "error when last response was error" =
        Poll_result.For_testing.create
          ~last_ok_response:(1, "foo", add_seconds 0.)
          ~last_error:(1, error, add_seconds 1.)
          ()
        |> print_error;
        expect_diff
          ~without_details:(fun () -> [%expect {| (error (example_error)) |}])
          ~with_details:(fun () ->
            [%expect {| (error ((1 example_error "1970-01-01 00:00:01Z"))) |}])
      ;;

      let%expect_test "no error when last response was ok" =
        Poll_result.For_testing.create ~last_ok_response:(1, "foo", timestamp) ()
        |> print_error;
        [%expect {| (error ()) |}]
      ;;
    end
    in
    ()
  ;;
end

module%test [@name "Output_type.Last_ok_response[_with_details]"] _ = struct
  let print_response t =
    let response = Poll_result.get_output t ~output_type:Last_ok_response in
    print_s [%message (response : string option)]
  ;;

  let print_response_with_details t =
    let response = Poll_result.get_output t ~output_type:Last_ok_response_with_details in
    print_s [%message (response : (int * string * Time_ns.Alternate_sexp.t) option)]
  ;;

  let () =
    bisimulate_with_without_details print_response print_response_with_details
    @@ fun print_response ~expect_diff ->
    let module _ = struct
      let%expect_test "last ok response persists through errors" =
        Poll_result.For_testing.create
          ~last_ok_response:(1, "foo", add_seconds 0.)
          ~last_error:(2, error, add_seconds 1.)
          ()
        |> print_response;
        expect_diff
          ~without_details:(fun () -> [%expect {| (response (foo)) |}])
          ~with_details:(fun () ->
            [%expect {| (response ((1 foo "1970-01-01 00:00:00Z"))) |}])
      ;;

      let%expect_test "no last ok response when never succeeded" =
        Poll_result.For_testing.create ~last_error:(1, error, timestamp) ()
        |> print_response;
        [%expect {| (response ()) |}]
      ;;
    end
    in
    ()
  ;;
end

module%test [@name "Output_type.Refresh_effect"] _ = struct
  let%expect_test "refresh effect is available" =
    let t = Poll_result.For_testing.create () in
    let refresh = Poll_result.get_output t ~output_type:Refresh_effect in
    Effect.Expert.handle_non_dom_event_exn refresh;
    [%expect {| refreshed! |}]
  ;;
end

module%test [@name "Output_type.Join_or_error"] _ = struct
  let print_joined t =
    let result = Poll_result.get_output t ~output_type:(Join_or_error Abstract) in
    print_s [%message (result : (int, string) Poll_result.t)]
  ;;

  let%expect_test "join_or_error with ok response" =
    Poll_result.For_testing.create ~last_ok_response:(1, Ok "response ok", timestamp) ()
    |> print_joined;
    [%expect {| (result ((last_ok_response (1 "response ok" "1970-01-01 00:00:00Z")))) |}]
  ;;

  let%expect_test "join_or_error with error in response" =
    Poll_result.For_testing.create
      ~last_ok_response:(1, Error (Error.of_string "response error"), timestamp)
      ()
    |> print_joined;
    [%expect {| (result ((last_error (1 "response error" "1970-01-01 00:00:00Z")))) |}]
  ;;

  let%expect_test "join_or_error with actual error" =
    Poll_result.For_testing.create
      ~last_ok_response:(1, Error (Error.of_string "response error"), add_seconds 0.)
      ~last_error:(1, Error.of_string "actual error", add_seconds 1.)
      ()
    |> print_joined;
    [%expect {| (result ((last_error (1 "actual error" "1970-01-01 00:00:01Z")))) |}]
  ;;

  let%expect_test "if the last response was ok, it sticks around" =
    Poll_result.For_testing.create
      ~last_ok_response:(1, Ok "response ok", add_seconds 0.)
      ~last_error:(1, Error.of_string "actual error", add_seconds 1.)
      ()
    |> print_joined;
    [%expect
      {|
      (result
       ((last_ok_response (1 "response ok" "1970-01-01 00:00:00Z"))
        (last_error (1 "actual error" "1970-01-01 00:00:01Z"))))
      |}]
  ;;
end

module%test [@name "Output_type.[]/::"] _ = struct
  let%expect_test "empty list" =
    let t = Poll_result.For_testing.create ~last_ok_response:(1, "foo", timestamp) () in
    let [] = Poll_result.get_output t ~output_type:[] in
    ()
  ;;

  let%expect_test "cons operator" =
    let t =
      Poll_result.For_testing.create
        ~last_ok_response:(1, "foo", add_seconds 0.)
        ~inflight_query:(2, add_seconds 1.)
        ()
    in
    let [ last_response; response_state; fetching_status ] =
      Poll_result.get_output
        t
        ~output_type:[ Last_ok_response; Response_state; Fetching_status ]
    in
    print_s
      [%message
        (last_response : string option)
          (response_state : string Poll_result.Response_state.t)
          (fetching_status : int Poll_result.Fetching_status.t)];
    [%expect
      {|
      ((last_response (foo)) (response_state (Ok foo))
       (fetching_status
        (Fetching (query 2) (query_changed true)
         (query_changed_since_last_ok true))))
      |}]
  ;;
end
