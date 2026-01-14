open! Core
open! Bonsai_web
open Bonsai_web_test

module%test [@name "sexp_of_t"] _ = struct
  module U1 = Rpc_effect.Where_to_connect.Register ()
  module U2 = Rpc_effect.Where_to_connect.Register ()
  module S = Rpc_effect.Where_to_connect.Register1 (String)

  module Record = struct
    type t =
      { foo : int
      ; bar : string
      }
    [@@deriving compare, sexp_of]
  end

  module Custom = Rpc_effect.Where_to_connect.Register1 (Record)

  let%expect_test "" =
    let u1 = U1.where_to_connect in
    print_s [%sexp (u1 : Rpc_effect.Where_to_connect.t)];
    [%expect {| (Custom ()) |}];
    let u2 = U2.where_to_connect in
    print_s [%sexp (u2 : Rpc_effect.Where_to_connect.t)];
    [%expect {| (Custom ()) |}];
    let s = S.where_to_connect "x" in
    print_s [%sexp (s : Rpc_effect.Where_to_connect.t)];
    [%expect {| (Custom x) |}];
    let custom = Custom.where_to_connect { Record.foo = 1; bar = "x" } in
    print_s [%sexp (custom : Rpc_effect.Where_to_connect.t)];
    [%expect {| (Custom ((foo 1) (bar x))) |}]
  ;;
end

module%test [@name "compare works"] _ = struct
  module U1 = Rpc_effect.Where_to_connect.Register ()
  module U2 = Rpc_effect.Where_to_connect.Register ()
  module S1 = Rpc_effect.Where_to_connect.Register1 (String)
  module S2 = Rpc_effect.Where_to_connect.Register1 (String)
  module Int_t = Rpc_effect.Where_to_connect.Register1 (Int)

  let%expect_test _ =
    let open Expect_test_helpers_base in
    let string1 = S1.where_to_connect "1" in
    let string2 = S1.where_to_connect "2" in
    let int1 () = Int_t.where_to_connect 1 in
    let int2 = Int_t.where_to_connect 2 in
    (let all =
       [ string1
       ; string2
       ; int1 ()
       ; int2
       ; Rpc_effect.Where_to_connect.self ~on_conn_failure:Retry_until_success ()
       ]
     in
     (* Reflexive *)
     List.iter all ~f:(fun t ->
       require_equal (module Int) (Rpc_effect.Where_to_connect.compare t t) 0);
     (* Anti-symmetric *)
     List.iter all ~f:(fun t1 ->
       List.iter all ~f:(fun t2 ->
         match
           ( Ordering.of_int (Rpc_effect.Where_to_connect.compare t1 t2)
           , Ordering.of_int (Rpc_effect.Where_to_connect.compare t2 t1) )
         with
         | Less, Greater | Greater, Less | Equal, Equal -> ()
         | _ ->
           Expect_test_helpers_base.print_cr
             [%message "Comparison function is not symmetric"]));
     (* Transitive *)
     List.iter all ~f:(fun t1 ->
       List.iter all ~f:(fun t2 ->
         List.iter all ~f:(fun t3 ->
           match
             ( Ordering.of_int (Rpc_effect.Where_to_connect.compare t1 t2)
             , Ordering.of_int (Rpc_effect.Where_to_connect.compare t2 t3)
             , Ordering.of_int (Rpc_effect.Where_to_connect.compare t1 t3) )
           with
           | Less, Less, Less
           | Less, Equal, Less
           | Equal, Less, Less
           | Equal, Equal, Equal
           | Greater, Greater, Greater
           | Greater, Equal, Greater
           | Equal, Greater, Greater
           | Less, Greater, _
           | Greater, Less, _ -> ()
           | _ ->
             Expect_test_helpers_base.print_cr [%message "Comparison is not transitive!"])));
     (* Non-trivial *)
     List.iteri all ~f:(fun i1 t1 ->
       List.iteri all ~f:(fun i2 t2 ->
         if Rpc_effect.Where_to_connect.compare t1 t2 = 0 then assert (i1 = i2))))
    (* Items that are structurally equal, but not physically equal. *);
    List.iter
      [ int1 (); int1 () ]
      ~f:(fun t1 ->
        List.iter
          [ int1 (); int1 () ]
          ~f:(fun t2 ->
            Expect_test_helpers_core.require_compare_equal
              (module struct
                include Rpc_effect.Where_to_connect

                let sexp_of_t = sexp_of_opaque
              end)
              t1
              t2))
  ;;

  let assert_equal a b = assert (Comparable.equal Rpc_effect.Where_to_connect.compare a b)

  let assert_not_equal a b =
    assert (not (Comparable.equal Rpc_effect.Where_to_connect.compare a b))
  ;;

  let%expect_test "equal" =
    assert_equal U1.where_to_connect U1.where_to_connect;
    [%expect {| |}];
    assert_equal U2.where_to_connect U2.where_to_connect;
    [%expect {| |}];
    assert_equal (S1.where_to_connect "x") (S1.where_to_connect "x");
    [%expect {| |}];
    ()
  ;;

  let%expect_test "not equal" =
    assert_not_equal U1.where_to_connect U2.where_to_connect;
    [%expect {| |}];
    assert_not_equal (S1.where_to_connect "x") (S2.where_to_connect "x");
    [%expect {| |}];
    assert_not_equal (S1.where_to_connect "x") (S1.where_to_connect "y");
    [%expect {| |}];
    assert_not_equal (S1.where_to_connect "x") (S1.where_to_connect "y");
    [%expect {| |}];
    ()
  ;;
end

module%test [@name "Multiple custom [where_to_connect]s"] _ = struct
  open! Async_kernel
  open Async_rpc_kernel
  open Async_js_test

  let connection_name_rpc =
    Rpc.Rpc.create
      ~name:"connection-name"
      ~version:0
      ~bin_query:[%bin_type_class: unit]
      ~bin_response:[%bin_type_class: string]
      ~include_in_error_count:Only_on_exn
  ;;

  let implementation1 =
    Rpc.Rpc.implement' connection_name_rpc (fun _connection_state () -> "U1")
  ;;

  let implementation2 =
    Rpc.Rpc.implement' connection_name_rpc (fun _connection_state () -> "U2")
  ;;

  module U1 = Rpc_effect.Where_to_connect.Register ()
  module U2 = Rpc_effect.Where_to_connect.Register ()

  let%expect_test "Multiple custom connectors are routed correctly" =
    let handle =
      Handle.create
        ~connectors:(function
          | Custom U1.T ->
            Rpc_effect.Connector.for_test
              ~connection_state:Fn.id
              (Rpc.Implementations.create_exn
                 ~implementations:[ implementation1 ]
                 ~on_unknown_rpc:`Raise
                 ~on_exception:Log_on_background_exn)
          | Custom U2.T ->
            Rpc_effect.Connector.for_test
              ~connection_state:Fn.id
              (Rpc.Implementations.create_exn
                 ~implementations:[ implementation2 ]
                 ~on_unknown_rpc:`Raise
                 ~on_exception:Log_on_background_exn)
          | _ -> Rpc_effect.Connector.test_fallback)
        (Result_spec.vdom Fn.id)
        (fun graph ->
          let open Bonsai.Let_syntax in
          let%arr dispatcher1 =
            Rpc_effect.Rpc.dispatcher
              connection_name_rpc
              ~where_to_connect:(return U1.where_to_connect)
              graph
          and dispatcher2 =
            Rpc_effect.Rpc.dispatcher
              connection_name_rpc
              ~where_to_connect:(return U2.where_to_connect)
              graph
          in
          let print_on_click eff _evt =
            let%bind.Effect r = eff () in
            Effect.print_s ([%sexp_of: string Or_error.t] r)
          in
          {%html|
            <div>
              <button id="button1" on_click=%{print_on_click dispatcher1}>button1</button>
              <button id="button2" on_click=%{print_on_click dispatcher2}>button2</button>
            </div>
          |})
    in
    Handle.click_on ~get_vdom:Fn.id handle ~selector:"#button1";
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    [%expect {| (Ok U1) |}];
    Handle.click_on ~get_vdom:Fn.id handle ~selector:"#button2";
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    [%expect {| (Ok U2) |}];
    Handle.click_on ~get_vdom:Fn.id handle ~selector:"#button1";
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    [%expect {| (Ok U1) |}];
    return ()
  ;;
end

module Var = Bonsai.Expert.Var

module%test Dynamic_where_to_connect = struct
  open! Async_kernel
  open Async_rpc_kernel
  open Async_js_test

  module Server_state = struct
    type t =
      { num_times_polled : int ref
      ; server_name : string
      }

    let create ~server_name = { num_times_polled = ref 0; server_name }
    let poll { num_times_polled; _ } = incr num_times_polled
  end

  module Connection_state = struct
    type t =
      { num_times_polled : int ref
      ; connection : Rpc.Connection.t
      }

    let create connection = { num_times_polled = ref 0; connection }
    let poll { num_times_polled; _ } = incr num_times_polled
  end

  module Response = struct
    module T = struct
      type t =
        { server : string
        ; query : int
        ; num_times_anyone_polled : int
        ; num_times_connection_polled : int
        ; num_times_client_polled : int option
        }
      [@@deriving sexp, bin_io, compare, equal, diff, hash]
    end

    include T
    include Diffable_polling_state_rpc_response.Polling_state_rpc_response.Make (T)

    let make ~connection_state ~server_state ~query ~num_times_client_polled =
      { server = server_state.Server_state.server_name
      ; query
      ; num_times_anyone_polled = !(server_state.num_times_polled)
      ; num_times_connection_polled =
          !(connection_state.Connection_state.num_times_polled)
      ; num_times_client_polled
      }
    ;;
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"a"
      ~version:0
      ~bin_query:bin_int
      ~bin_response:[%bin_type_class: Response.t]
      ~include_in_error_count:Only_on_exn
  ;;

  let polling_state_rpc =
    Polling_state_rpc.create
      ~name:"polling_state_rpc"
      ~version:0
      ~query_equal:[%equal: int]
      ~bin_query:bin_int
      (module Response)
  ;;

  let print_sexp' ~origin s =
    let prefix =
      match origin with
      | `Server -> "[server]"
      | `Client -> "[client]"
    in
    [%string "%{prefix} %{Sexp.to_string_hum s}"]
  ;;

  let print_sexp ~origin s = print_sexp' ~origin s |> print_endline

  let rpc_implementation ~server_state () =
    let server_name = server_state.Server_state.server_name in
    Rpc.Rpc.implement rpc (fun connection_state query ->
      Server_state.poll server_state;
      Connection_state.poll connection_state;
      let open Async_kernel in
      print_sexp
        ~origin:`Server
        [%message "Got request" (query : int) (server_name : string)];
      return
        (Response.make
           ~connection_state
           ~server_state
           ~query
           ~num_times_client_polled:None))
  ;;

  let polling_state_rpc_implementation ~server_state () =
    let server_name = server_state.Server_state.server_name in
    Rpc.Implementation.lift
      ~f:(fun (connection_state : Connection_state.t) ->
        connection_state, connection_state.connection)
      (Polling_state_rpc.implement_with_client_state
         polling_state_rpc
         ~on_client_and_server_out_of_sync:
           (Expect_test_helpers_core.print_s ~hide_positions:true)
         ~create_client_state:(fun _connection_state -> ref 0)
         ~for_first_request:(fun connection_state client_state query ->
           let open Async_kernel in
           Server_state.poll server_state;
           Connection_state.poll connection_state;
           incr client_state;
           print_sexp
             ~origin:`Server
             [%message "First request" (query : int) (server_name : string)];
           return
             (Response.make
                ~connection_state
                ~server_state
                ~query
                ~num_times_client_polled:(Some !client_state)))
         (fun connection_state client_state query ->
           let open Async_kernel in
           Server_state.poll server_state;
           Connection_state.poll connection_state;
           incr client_state;
           print_sexp
             ~origin:`Server
             [%message "Followup Request" (query : int) (server_name : string)];
           return
             (Response.make
                ~connection_state
                ~server_state
                ~query
                ~num_times_client_polled:(Some !client_state))))
  ;;

  module First = Rpc_effect.Where_to_connect.Register ()
  module Second = Rpc_effect.Where_to_connect.Register ()

  let connectors () =
    let first_server_state = Server_state.create ~server_name:"first" in
    let first =
      Rpc_effect.Connector.for_test
        ~connection_state:Connection_state.create
        (Rpc.Implementations.create_exn
           ~implementations:
             [ rpc_implementation ~server_state:first_server_state ()
             ; polling_state_rpc_implementation ~server_state:first_server_state ()
             ]
           ~on_unknown_rpc:`Raise
           ~on_exception:Log_on_background_exn)
    in
    let second_server_state = Server_state.create ~server_name:"second" in
    let second =
      Rpc_effect.Connector.for_test
        ~connection_state:Connection_state.create
        (Rpc.Implementations.create_exn
           ~implementations:
             [ rpc_implementation ~server_state:second_server_state ()
             ; polling_state_rpc_implementation ~server_state:second_server_state ()
             ]
           ~on_unknown_rpc:`Raise
           ~on_exception:Log_on_background_exn)
    in
    function
    | Rpc_effect.Where_to_connect.Custom First.T -> first
    | Custom Second.T -> second
    | _ -> Rpc_effect.Connector.test_fallback
  ;;

  module One_shot_result_spec = struct
    type t = int -> Response.t Or_error.t Effect.t
    type incoming = int

    let view _ = ""

    let incoming f query =
      let%map.Effect result = f query in
      let sexp = [%sexp_of: Response.t Or_error.t] result in
      print_sexp ~origin:`Client sexp
    ;;
  end

  module Polling_result_spec = struct
    type t = (int, Response.t) Rpc_effect.Poll_result.Legacy_record.t

    let view t =
      let sexp = [%sexp_of: (int, Response.t) Rpc_effect.Poll_result.Legacy_record.t] t in
      print_sexp' ~origin:`Client sexp
    ;;

    type incoming = Nothing.t

    let incoming _t incoming = Nothing.unreachable_code incoming
  end

  let%expect_test "dispatcher" =
    let where_to_connect = Var.create First.where_to_connect in
    let computation =
      Rpc_effect.Rpc.dispatcher rpc ~where_to_connect:(Var.value where_to_connect)
    in
    let handle =
      Handle.create ~connectors:(connectors ()) (module One_shot_result_spec) computation
    in
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    Handle.do_actions handle [ 1; 2 ];
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    [%expect
      {|
      [server] ("Got request" (query 1) (server_name first))
      [server] ("Got request" (query 2) (server_name first))
      [client] (Ok
       ((server first) (query 1) (num_times_anyone_polled 1)
        (num_times_connection_polled 1) (num_times_client_polled ())))
      [client] (Ok
       ((server first) (query 2) (num_times_anyone_polled 2)
        (num_times_connection_polled 2) (num_times_client_polled ())))
      |}];
    Var.set where_to_connect Second.where_to_connect;
    Handle.recompute_view handle;
    Handle.do_actions handle [ 1; 2 ];
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    [%expect
      {|
      [server] ("Got request" (query 1) (server_name second))
      [server] ("Got request" (query 2) (server_name second))
      [client] (Ok
       ((server second) (query 1) (num_times_anyone_polled 1)
        (num_times_connection_polled 1) (num_times_client_polled ())))
      [client] (Ok
       ((server second) (query 2) (num_times_anyone_polled 2)
        (num_times_connection_polled 2) (num_times_client_polled ())))
      |}];
    Var.set where_to_connect First.where_to_connect;
    Handle.recompute_view handle;
    Handle.do_actions handle [ 1; 2 ];
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    [%expect
      {|
      [server] ("Got request" (query 1) (server_name first))
      [server] ("Got request" (query 2) (server_name first))
      [client] (Ok
       ((server first) (query 1) (num_times_anyone_polled 3)
        (num_times_connection_polled 3) (num_times_client_polled ())))
      [client] (Ok
       ((server first) (query 2) (num_times_anyone_polled 4)
        (num_times_connection_polled 4) (num_times_client_polled ())))
      |}];
    return ()
  ;;

  let poll_handle ?clear_when_deactivated ~where_to_connect () =
    let computation =
      Rpc_effect.Rpc.poll
        ~sexp_of_query:[%sexp_of: Int.t]
        ~sexp_of_response:[%sexp_of: Response.t]
        ~equal_query:[%equal: Int.t]
        ~equal_response:[%equal: Response.t]
        ?clear_when_deactivated
        rpc
        ~where_to_connect:(Var.value where_to_connect)
        ~every:(Bonsai.return (Time_ns.Span.of_sec 1.0))
        ~output_type:Legacy_record
        (Bonsai.return 2)
    in
    Handle.create ~connectors:(connectors ()) (module Polling_result_spec) computation
  ;;

  let send_via_clock ?(print_inflight = false) handle =
    Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
    (* [recompute_view] to send the RPC, another for the response to be evaluated and
       returned, and then a show to process andprint the response. *)
    Handle.recompute_view handle;
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    if print_inflight then print_endline "While inflight:";
    if print_inflight then Handle.show handle else Handle.recompute_view handle;
    if print_inflight then print_endline "";
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    if print_inflight then print_endline "Final:";
    Handle.show handle;
    return ()
  ;;

  let%expect_test "poll" =
    let where_to_connect = Var.create First.where_to_connect in
    let handle = poll_handle ~where_to_connect () in
    let%bind () = send_via_clock handle in
    [%expect
      {|
      [server] ("Got request" (query 2) (server_name first))
      [client] ((last_ok_response
        ((2
          ((server first) (query 2) (num_times_anyone_polled 1)
           (num_times_connection_polled 1) (num_times_client_polled ())))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      |}];
    Var.set where_to_connect Second.where_to_connect;
    Handle.recompute_view handle;
    Handle.show handle;
    (* A new request is scheduled in the lifecycles of the first frame, and sent in the
       second. *)
    [%expect
      {|
      [client] ((last_ok_response
        ((2
          ((server first) (query 2) (num_times_anyone_polled 1)
           (num_times_connection_polled 1) (num_times_client_polled ())))))
       (last_error ()) (inflight_query (2)) (refresh <opaque>))
      |}];
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    Handle.show handle;
    [%expect
      {|
      [server] ("Got request" (query 2) (server_name second))
      [client] ((last_ok_response
        ((2
          ((server second) (query 2) (num_times_anyone_polled 1)
           (num_times_connection_polled 1) (num_times_client_polled ())))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      |}];
    (* Can then poll poll via interval. *)
    let%bind () = send_via_clock handle in
    [%expect
      {|
      [server] ("Got request" (query 2) (server_name second))
      [client] ((last_ok_response
        ((2
          ((server second) (query 2) (num_times_anyone_polled 2)
           (num_times_connection_polled 2) (num_times_client_polled ())))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      |}];
    Var.set where_to_connect First.where_to_connect;
    Handle.recompute_view handle;
    Handle.show handle;
    (* A new request is scheduled in the lifecycles of the first frame, and sent in the
       second. *)
    [%expect
      {|
      [client] ((last_ok_response
        ((2
          ((server second) (query 2) (num_times_anyone_polled 2)
           (num_times_connection_polled 2) (num_times_client_polled ())))))
       (last_error ()) (inflight_query (2)) (refresh <opaque>))
      |}];
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    Handle.show handle;
    [%expect
      {|
      [server] ("Got request" (query 2) (server_name first))
      [client] ((last_ok_response
        ((2
          ((server first) (query 2) (num_times_anyone_polled 2)
           (num_times_connection_polled 2) (num_times_client_polled ())))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      |}];
    (* Can then poll poll via interval. *)
    let%bind () = send_via_clock handle in
    [%expect
      {|
      [server] ("Got request" (query 2) (server_name first))
      [client] ((last_ok_response
        ((2
          ((server first) (query 2) (num_times_anyone_polled 3)
           (num_times_connection_polled 3) (num_times_client_polled ())))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      |}];
    return ()
  ;;

  let%expect_test "PSRPC dispatcher" =
    let where_to_connect = Var.create First.where_to_connect in
    let computation =
      Rpc_effect.Polling_state_rpc.dispatcher
        polling_state_rpc
        ~where_to_connect:(Var.value where_to_connect)
    in
    let handle =
      Handle.create ~connectors:(connectors ()) (module One_shot_result_spec) computation
    in
    Handle.do_actions handle [ 1 ];
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    [%expect
      {|
      [server] ("First request" (query 1) (server_name first))
      [client] (Ok
       ((server first) (query 1) (num_times_anyone_polled 1)
        (num_times_connection_polled 1) (num_times_client_polled (1))))
      |}];
    Handle.do_actions handle [ 1 ];
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    [%expect
      {|
      [server] ("Followup Request" (query 1) (server_name first))
      [client] (Ok
       ((server first) (query 1) (num_times_anyone_polled 2)
        (num_times_connection_polled 2) (num_times_client_polled (2))))
      |}];
    Var.set where_to_connect Second.where_to_connect;
    Handle.recompute_view handle;
    Handle.do_actions handle [ 2 ];
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    [%expect
      {|
      [server] ("First request" (query 2) (server_name second))
      [client] (Ok
       ((server second) (query 2) (num_times_anyone_polled 1)
        (num_times_connection_polled 1) (num_times_client_polled (1))))
      |}];
    Handle.do_actions handle [ 2 ];
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    [%expect
      {|
      [server] ("Followup Request" (query 2) (server_name second))
      [client] (Ok
       ((server second) (query 2) (num_times_anyone_polled 2)
        (num_times_connection_polled 2) (num_times_client_polled (2))))
      |}];
    Handle.do_actions handle [ 3 ];
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    (* PSRPC server treats new query as a "new request". But client state persists!*)
    [%expect
      {|
      [server] ("First request" (query 3) (server_name second))
      [client] (Ok
       ((server second) (query 3) (num_times_anyone_polled 3)
        (num_times_connection_polled 3) (num_times_client_polled (3))))
      |}];
    Handle.do_actions handle [ 2 ];
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    [%expect
      {|
      [server] ("First request" (query 2) (server_name second))
      [client] (Ok
       ((server second) (query 2) (num_times_anyone_polled 4)
        (num_times_connection_polled 4) (num_times_client_polled (4))))
      |}];
    Var.set where_to_connect First.where_to_connect;
    Handle.recompute_view handle;
    Handle.do_actions handle [ 2 ];
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    (* We've switched back, and the connection is the same, but there's a new client
       state. *)
    [%expect
      {|
      [server] ("First request" (query 2) (server_name first))
      [client] (Ok
       ((server first) (query 2) (num_times_anyone_polled 3)
        (num_times_connection_polled 3) (num_times_client_polled (1))))
      |}];
    Handle.do_actions handle [ 1 ];
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    [%expect
      {|
      [server] ("First request" (query 1) (server_name first))
      [client] (Ok
       ((server first) (query 1) (num_times_anyone_polled 4)
        (num_times_connection_polled 4) (num_times_client_polled (2))))
      |}];
    return ()
  ;;

  let psrpc_poll_handle ?clear_when_deactivated ~where_to_connect () =
    let computation =
      Rpc_effect.Polling_state_rpc.poll
        ~sexp_of_query:[%sexp_of: Int.t]
        ~sexp_of_response:[%sexp_of: Response.t]
        ~equal_query:[%equal: Int.t]
        ~equal_response:[%equal: Response.t]
        ?clear_when_deactivated
        polling_state_rpc
        ~where_to_connect:(Var.value where_to_connect)
        ~every:(Bonsai.return (Time_ns.Span.of_sec 1.0))
        ~output_type:Legacy_record
        (Bonsai.return 2)
    in
    Handle.create
      ~connectors:(connectors ())
      (Result_spec.sexp
         (module struct
           type t = (int, Response.t) Rpc_effect.Poll_result.Legacy_record.t
           [@@deriving sexp_of]
         end))
      computation
  ;;

  let%expect_test "Polling_state_rpc.poll" =
    let where_to_connect = Var.create First.where_to_connect in
    let handle = psrpc_poll_handle ~where_to_connect () in
    Handle.show handle;
    [%expect
      {|
      ((last_ok_response ()) (last_error ()) (inflight_query ())
       (refresh <opaque>))
      |}];
    Handle.show handle;
    [%expect
      {|
      ((last_ok_response ()) (last_error ()) (inflight_query (2))
       (refresh <opaque>))
      |}];
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    Handle.show handle;
    [%expect
      {|
      [server] ("First request" (query 2) (server_name first))
      ((last_ok_response
        ((2
          ((server first) (query 2) (num_times_anyone_polled 1)
           (num_times_connection_polled 1) (num_times_client_polled (1))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      |}];
    let%bind () = send_via_clock handle in
    [%expect
      {|
      [server] ("Followup Request" (query 2) (server_name first))
      ((last_ok_response
        ((2
          ((server first) (query 2) (num_times_anyone_polled 2)
           (num_times_connection_polled 2) (num_times_client_polled (2))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      |}];
    let%bind () = send_via_clock handle in
    [%expect
      {|
      [server] ("Followup Request" (query 2) (server_name first))
      ((last_ok_response
        ((2
          ((server first) (query 2) (num_times_anyone_polled 3)
           (num_times_connection_polled 3) (num_times_client_polled (3))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      |}];
    Var.set where_to_connect Second.where_to_connect;
    (* Request is scheduled the next frame, and sent the one after that. *)
    Handle.recompute_view handle;
    Handle.show handle;
    [%expect
      {|
      ((last_ok_response
        ((2
          ((server first) (query 2) (num_times_anyone_polled 3)
           (num_times_connection_polled 3) (num_times_client_polled (3))))))
       (last_error ()) (inflight_query (2)) (refresh <opaque>))
      |}];
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    Handle.show handle;
    [%expect
      {|
      [server] ("First request" (query 2) (server_name second))
      ((last_ok_response
        ((2
          ((server second) (query 2) (num_times_anyone_polled 1)
           (num_times_connection_polled 1) (num_times_client_polled (1))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      |}];
    let%bind () = send_via_clock handle in
    [%expect
      {|
      [server] ("Followup Request" (query 2) (server_name second))
      ((last_ok_response
        ((2
          ((server second) (query 2) (num_times_anyone_polled 2)
           (num_times_connection_polled 2) (num_times_client_polled (2))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      |}];
    let%bind () = send_via_clock handle in
    [%expect
      {|
      [server] ("Followup Request" (query 2) (server_name second))
      ((last_ok_response
        ((2
          ((server second) (query 2) (num_times_anyone_polled 3)
           (num_times_connection_polled 3) (num_times_client_polled (3))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      |}];
    Var.set where_to_connect First.where_to_connect;
    (* Request is scheduled the next frame, and sent the one after that. *)
    Handle.recompute_view handle;
    Handle.show handle;
    [%expect
      {|
      ((last_ok_response
        ((2
          ((server second) (query 2) (num_times_anyone_polled 3)
           (num_times_connection_polled 3) (num_times_client_polled (3))))))
       (last_error ()) (inflight_query (2)) (refresh <opaque>))
      |}];
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    Handle.show handle;
    [%expect
      {|
      [server] ("First request" (query 2) (server_name first))
      ((last_ok_response
        ((2
          ((server first) (query 2) (num_times_anyone_polled 4)
           (num_times_connection_polled 4) (num_times_client_polled (1))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      |}];
    let%bind () = send_via_clock handle in
    [%expect
      {|
      [server] ("Followup Request" (query 2) (server_name first))
      ((last_ok_response
        ((2
          ((server first) (query 2) (num_times_anyone_polled 5)
           (num_times_connection_polled 5) (num_times_client_polled (2))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      |}];
    let%bind () = send_via_clock handle in
    [%expect
      {|
      [server] ("Followup Request" (query 2) (server_name first))
      ((last_ok_response
        ((2
          ((server first) (query 2) (num_times_anyone_polled 6)
           (num_times_connection_polled 6) (num_times_client_polled (3))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      |}];
    return ()
  ;;

  let%expect_test "Polling_state_rpc.poll, switching with an inflight query." =
    let where_to_connect = Var.create First.where_to_connect in
    let handle = psrpc_poll_handle ~where_to_connect () in
    Handle.show handle;
    [%expect
      {|
      ((last_ok_response ()) (last_error ()) (inflight_query ())
       (refresh <opaque>))
      |}];
    Handle.show handle;
    [%expect
      {|
      ((last_ok_response ()) (last_error ()) (inflight_query (2))
       (refresh <opaque>))
      |}];
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    [%expect {| [server] ("First request" (query 2) (server_name first)) |}];
    Var.set where_to_connect Second.where_to_connect;
    (* Request is scheduled the next frame, and sent the one after that. *)
    Handle.recompute_view handle;
    Handle.show handle;
    [%expect
      {|
      ((last_ok_response
        ((2
          ((server first) (query 2) (num_times_anyone_polled 1)
           (num_times_connection_polled 1) (num_times_client_polled (1))))))
       (last_error ()) (inflight_query (2)) (refresh <opaque>))
      |}];
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    Handle.show handle;
    [%expect
      {|
      [server] ("First request" (query 2) (server_name second))
      ((last_ok_response
        ((2
          ((server second) (query 2) (num_times_anyone_polled 1)
           (num_times_connection_polled 1) (num_times_client_polled (1))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      |}];
    let%bind () = send_via_clock handle in
    [%expect
      {|
      [server] ("Followup Request" (query 2) (server_name second))
      ((last_ok_response
        ((2
          ((server second) (query 2) (num_times_anyone_polled 2)
           (num_times_connection_polled 2) (num_times_client_polled (2))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      |}];
    let%bind () = send_via_clock handle in
    [%expect
      {|
      [server] ("Followup Request" (query 2) (server_name second))
      ((last_ok_response
        ((2
          ((server second) (query 2) (num_times_anyone_polled 3)
           (num_times_connection_polled 3) (num_times_client_polled (3))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      |}];
    Var.set where_to_connect First.where_to_connect;
    (* Request is scheduled the next frame, and sent the one after that. *)
    Handle.recompute_view handle;
    Handle.show handle;
    [%expect
      {|
      ((last_ok_response
        ((2
          ((server second) (query 2) (num_times_anyone_polled 3)
           (num_times_connection_polled 3) (num_times_client_polled (3))))))
       (last_error ()) (inflight_query (2)) (refresh <opaque>))
      |}];
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    Handle.show handle;
    [%expect
      {|
      [server] ("First request" (query 2) (server_name first))
      ((last_ok_response
        ((2
          ((server first) (query 2) (num_times_anyone_polled 2)
           (num_times_connection_polled 2) (num_times_client_polled (1))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      |}];
    let%bind () = send_via_clock handle in
    [%expect
      {|
      [server] ("Followup Request" (query 2) (server_name first))
      ((last_ok_response
        ((2
          ((server first) (query 2) (num_times_anyone_polled 3)
           (num_times_connection_polled 3) (num_times_client_polled (2))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      |}];
    let%bind () = send_via_clock handle in
    [%expect
      {|
      [server] ("Followup Request" (query 2) (server_name first))
      ((last_ok_response
        ((2
          ((server first) (query 2) (num_times_anyone_polled 4)
           (num_times_connection_polled 4) (num_times_client_polled (3))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      |}];
    return ()
  ;;

  let%expect_test "Polling_state_rpc.shared_poller" =
    let where_to_connect = Var.create First.where_to_connect in
    let computation graph =
      let open Bonsai.Let_syntax in
      let polling_queries, set_polling_queries = Bonsai.state [] graph in
      let shared_poller =
        Rpc_effect.Polling_state_rpc.shared_poller
          (module Int)
          polling_state_rpc
          ~where_to_connect:(Var.value where_to_connect)
          ~every:(Bonsai.return Time_ns.Span.second)
          graph
      in
      let queries =
        let%arr polling_queries in
        String.Map.of_alist_exn polling_queries
      in
      let results =
        Bonsai.assoc
          (module String)
          queries
          ~f:(fun _key data ->
            Rpc_effect.Shared_poller.lookup shared_poller data ~output_type:Legacy_record)
          graph
      in
      Bonsai.both results set_polling_queries
    in
    let handle =
      Handle.create
        ~connectors:(connectors ())
        (module struct
          type t =
            (int, Response.t) Rpc_effect.Poll_result.Legacy_record.t String.Map.t
            * ((string * int) list -> unit Effect.t)

          type incoming = (string * int) list

          let view (poll_results, _) =
            Map.to_alist poll_results
            |> List.map ~f:(fun (name, data) ->
              let data_str =
                [%sexp_of: (int, Response.t) Rpc_effect.Poll_result.Legacy_record.t] data
                |> Sexp.to_string_hum
              in
              [%string "%{name}: %{data_str}"])
            |> String.concat_lines
          ;;

          let incoming (_, set) t = set t
        end)
        computation
    in
    Handle.do_actions handle [ [ "one", 1; "two", 2; "three", 3 ] ];
    Handle.show handle;
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    [%expect
      {|
      one: ((last_ok_response ()) (last_error ()) (inflight_query ())
       (refresh <opaque>))
      three: ((last_ok_response ()) (last_error ()) (inflight_query ())
       (refresh <opaque>))
      two: ((last_ok_response ()) (last_error ()) (inflight_query ())
       (refresh <opaque>))
      |}];
    Handle.show handle;
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    [%expect
      {|
      one: ((last_ok_response ()) (last_error ()) (inflight_query ())
       (refresh <opaque>))
      three: ((last_ok_response ()) (last_error ()) (inflight_query ())
       (refresh <opaque>))
      two: ((last_ok_response ()) (last_error ()) (inflight_query ())
       (refresh <opaque>))

      [server] ("First request" (query 1) (server_name first))
      [server] ("First request" (query 2) (server_name first))
      [server] ("First request" (query 3) (server_name first))
      |}];
    Handle.show handle;
    [%expect
      {|
      one: ((last_ok_response
        ((1
          ((server first) (query 1) (num_times_anyone_polled 1)
           (num_times_connection_polled 1) (num_times_client_polled (1))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      three: ((last_ok_response
        ((3
          ((server first) (query 3) (num_times_anyone_polled 3)
           (num_times_connection_polled 3) (num_times_client_polled (1))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      two: ((last_ok_response
        ((2
          ((server first) (query 2) (num_times_anyone_polled 2)
           (num_times_connection_polled 2) (num_times_client_polled (1))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      |}];
    Handle.do_actions handle [ [ "one", 1; "two", 2; "one_dup", 1 ] ];
    Handle.show handle;
    (* Nothing is sent to the server, because we are already polling `1`. *)
    [%expect
      {|
      one: ((last_ok_response
        ((1
          ((server first) (query 1) (num_times_anyone_polled 1)
           (num_times_connection_polled 1) (num_times_client_polled (1))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      one_dup: ((last_ok_response
        ((1
          ((server first) (query 1) (num_times_anyone_polled 1)
           (num_times_connection_polled 1) (num_times_client_polled (1))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      two: ((last_ok_response
        ((2
          ((server first) (query 2) (num_times_anyone_polled 2)
           (num_times_connection_polled 2) (num_times_client_polled (1))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      |}];
    Handle.do_actions handle [ [ "one", 1; "two", 2; "three", 3 ] ];
    (* Because we [clear_on_deactivated], we _do_ need to fetch 3 via RPC. *)
    Handle.show handle;
    [%expect
      {|
      one: ((last_ok_response
        ((1
          ((server first) (query 1) (num_times_anyone_polled 1)
           (num_times_connection_polled 1) (num_times_client_polled (1))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      three: ((last_ok_response ()) (last_error ()) (inflight_query ())
       (refresh <opaque>))
      two: ((last_ok_response
        ((2
          ((server first) (query 2) (num_times_anyone_polled 2)
           (num_times_connection_polled 2) (num_times_client_polled (1))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      |}];
    Handle.recompute_view handle;
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    Handle.show handle;
    [%expect
      {|
      [server] ("First request" (query 3) (server_name first))
      one: ((last_ok_response
        ((1
          ((server first) (query 1) (num_times_anyone_polled 1)
           (num_times_connection_polled 1) (num_times_client_polled (1))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      three: ((last_ok_response
        ((3
          ((server first) (query 3) (num_times_anyone_polled 4)
           (num_times_connection_polled 4) (num_times_client_polled (1))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      two: ((last_ok_response
        ((2
          ((server first) (query 2) (num_times_anyone_polled 2)
           (num_times_connection_polled 2) (num_times_client_polled (1))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      |}];
    Var.set where_to_connect Second.where_to_connect;
    Handle.recompute_view handle;
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    Handle.show handle;
    [%expect
      {|
      [server] ("First request" (query 1) (server_name second))
      [server] ("First request" (query 2) (server_name second))
      [server] ("First request" (query 3) (server_name second))
      one: ((last_ok_response
        ((1
          ((server second) (query 1) (num_times_anyone_polled 1)
           (num_times_connection_polled 1) (num_times_client_polled (1))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      three: ((last_ok_response
        ((3
          ((server second) (query 3) (num_times_anyone_polled 3)
           (num_times_connection_polled 3) (num_times_client_polled (1))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      two: ((last_ok_response
        ((2
          ((server second) (query 2) (num_times_anyone_polled 2)
           (num_times_connection_polled 2) (num_times_client_polled (1))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      |}];
    Var.set where_to_connect First.where_to_connect;
    Handle.recompute_view handle;
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    Handle.show handle;
    [%expect
      {|
      [server] ("First request" (query 1) (server_name first))
      [server] ("First request" (query 2) (server_name first))
      [server] ("First request" (query 3) (server_name first))
      one: ((last_ok_response
        ((1
          ((server first) (query 1) (num_times_anyone_polled 5)
           (num_times_connection_polled 5) (num_times_client_polled (1))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      three: ((last_ok_response
        ((3
          ((server first) (query 3) (num_times_anyone_polled 7)
           (num_times_connection_polled 7) (num_times_client_polled (1))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      two: ((last_ok_response
        ((2
          ((server first) (query 2) (num_times_anyone_polled 6)
           (num_times_connection_polled 6) (num_times_client_polled (1))))))
       (last_error ()) (inflight_query ()) (refresh <opaque>))
      |}];
    return ()
  ;;

  let%expect_test "Status.state" =
    let where_to_connect = Var.create First.where_to_connect in
    let handle =
      Handle.create
        ~connectors:(connectors ())
        (Result_spec.sexp (module Rpc_effect.Status))
        (Rpc_effect.Status.state ~where_to_connect:(Var.value where_to_connect))
    in
    Handle.show handle;
    [%expect {| ((state Connecting) (connecting_since ())) |}];
    Handle.show handle;
    [%expect {| ((state Connecting) (connecting_since ("1970-01-01 00:00:00Z"))) |}];
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    Handle.show handle;
    [%expect {| ((state Connected) (connecting_since ())) |}];
    Var.set where_to_connect Second.where_to_connect;
    Handle.show handle;
    [%expect {| ((state Connecting) (connecting_since ())) |}];
    Handle.show handle;
    [%expect {| ((state Connecting) (connecting_since ("1970-01-01 00:00:00Z"))) |}];
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    Handle.show handle;
    [%expect {| ((state Connected) (connecting_since ())) |}];
    Var.set where_to_connect First.where_to_connect;
    (* The connection to the first server is likely still open, but we should confirm that
       just to be sure. *)
    Handle.show handle;
    [%expect {| ((state Connecting) (connecting_since ("1970-01-01 00:00:00Z"))) |}];
    Handle.show handle;
    [%expect {| ((state Connecting) (connecting_since ("1970-01-01 00:00:00Z"))) |}];
    let%bind () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    Handle.show handle;
    [%expect {| ((state Connected) (connecting_since ())) |}];
    return ()
  ;;
end
