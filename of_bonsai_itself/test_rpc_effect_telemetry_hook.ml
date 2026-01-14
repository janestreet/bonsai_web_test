open! Core
open! Async_kernel
open Bonsai_web_proc
open Async_rpc_kernel
open Bonsai_web_test_async

let () = Async_js.init ()

module Query_and_response = struct
  type t = string * Rpc_effect.Inflight_query_id.t [@@deriving bin_io, equal, sexp]
end

let reverse_rpc =
  Rpc.Rpc.create
    ~name:"reverse-rpc"
    ~version:1
    ~bin_query:Query_and_response.bin_t
    ~bin_response:Query_and_response.bin_t
    ~include_in_error_count:Only_on_exn
;;

let reverse_rpc_implementation =
  Rpc.Rpc.implement' reverse_rpc (fun _ (query, id) -> String.rev query, id)
;;

module Poller_handle = struct
  module Poller_result_spec = struct
    type t = { set_query : query:Query_and_response.t -> unit Effect.t }
    type incoming = Set_query of Query_and_response.t

    let view _ = ""

    let incoming { set_query } = function
      | Set_query query -> set_query ~query
    ;;
  end

  let component
    ~(poller : Query_and_response.t Value.t -> Query_and_response.t option Computation.t)
    =
    let open Bonsai.Let_syntax in
    let%sub query, set_query = Bonsai.state_opt () in
    let%sub result_spec =
      let%arr set_query in
      let set_query ~query =
        let%bind.Effect () =
          Effect.of_sync_fun
            (fun () -> print_s [%message "Changing query" (query : Query_and_response.t)])
            ()
        in
        set_query (Some query)
      in
      { Poller_result_spec.set_query }
    in
    let%sub () =
      match%sub query with
      | None -> Bonsai.const ()
      | Some query ->
        let%sub last_ok_response = poller query in
        Bonsai.Edge.on_change
          last_ok_response
          ~trigger:`Before_display
          ~equal:[%equal: Query_and_response.t option]
          ~callback:
            (Value.return (function
              | None -> Effect.Ignore
              | Some response ->
                Effect.print_s [%message "Got response" (response : Query_and_response.t)]))
    in
    return result_spec
  ;;

  let create ~poller =
    Handle.create
      ~rpc_implementations:[ reverse_rpc_implementation ]
      ~connectors:(fun _ -> Bonsai_web.Rpc_effect.Connector.test_fallback)
      (module Poller_result_spec)
      (component ~poller)
  ;;
end

let dummy_id = Rpc_effect.Inflight_query_id.create ()

let%expect_test "telemetry hook runs properly on poll" =
  let handle =
    Poller_handle.create
      ~poller:
        (Rpc_effect.Rpc.poll
           reverse_rpc
           ~equal_query:[%equal: Query_and_response.t]
           ~intercept_query:
             (Value.return (fun (query, _) id -> Effect.return (query, id)))
           ~every:(Value.return Time_ns.Span.second)
           ~output_type:Last_ok_response)
  in
  Handle.do_actions handle [ Set_query ("foo", dummy_id) ];
  Handle.recompute_view handle;
  [%expect {| ("Changing query" (query (foo 0))) |}];
  let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
  Handle.recompute_view handle;
  [%expect {| |}];
  let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
  Handle.recompute_view handle;
  [%expect {| ("Got response" (response (oof 1))) |}];
  Handle.do_actions handle [ Set_query ("bar", dummy_id); Set_query ("baz", dummy_id) ];
  Handle.recompute_view handle;
  [%expect
    {|
    ("Changing query" (query (bar 0)))
    ("Changing query" (query (baz 0)))
    |}];
  let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
  Handle.recompute_view handle;
  [%expect {| |}];
  let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
  Handle.recompute_view handle;
  [%expect {| ("Got response" (response (zab 2))) |}];
  return ()
;;
