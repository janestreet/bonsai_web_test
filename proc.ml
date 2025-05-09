open! Core
open! Import
module Node_helpers = Virtual_dom_test_helpers.Node_helpers
module Linter = Node_helpers.Linter

let test_selector s = Test_selector.For_bonsai_web_test.css_selector s

module Result_spec = struct
  include Bonsai_test.Result_spec

  let vdom
    (type result)
    ?filter_printed_attributes
    ?(censor_paths = true)
    ?(censor_hash = true)
    ?path_censoring_message
    ?hash_censoring_message
    ?(lint_min_severity = Linter.Severity.Only_report_app_crashing_errors)
    ?lint_expected_failures
    ?selector
    get_vdom
    =
    let select_node =
      match selector with
      | None -> Fn.id
      | Some selector -> Node_helpers.select_first_exn ~selector
    in
    (module struct
      type t = result

      include No_incoming

      let view result =
        let node = result |> get_vdom |> Node_helpers.unsafe_convert_exn |> select_node in
        let view =
          Node_helpers.to_string_html
            ?path_censoring_message
            ?hash_censoring_message
            ~filter_printed_attributes:
              (Test_selector.For_bonsai_web_test
               .filter_printed_attributes_with_test_selector_filtering
                 ~filter_printed_attributes)
            ~censor_paths
            ~censor_hash
            node
        in
        match
          Node_helpers.Linter.run
            ?expected_failures:lint_expected_failures
            ~min_severity:lint_min_severity
            node
        with
        | None -> view
        | Some report -> [%string "%{view}\n\n%{report}"]
      ;;
    end : S
      with type t = result
       and type incoming = Nothing.t)
  ;;
end

let add_rpc_implementations_to_computation ~rpc_implementations ~connectors computation =
  match rpc_implementations, connectors with
  | None, None -> computation
  | _ ->
    let rpc_implementations = Option.value rpc_implementations ~default:[] in
    let connectors =
      Option.value connectors ~default:(fun _ ->
        Bonsai_web.Rpc_effect.Connector.test_fallback)
    in
    let test_fallback_connector =
      let open Async_rpc_kernel in
      Rpc_effect.Connector.for_test
        (Rpc.Implementations.create_exn
           ~on_unknown_rpc:`Continue
           ~implementations:rpc_implementations
           ~on_exception:Log_on_background_exn)
        ~connection_state:Fn.id
    in
    let connectors where_to_connect =
      let connector = connectors where_to_connect in
      if Bonsai_web.Rpc_effect.Private.is_test_fallback connector
      then test_fallback_connector
      else connector
    in
    Bonsai_web.Rpc_effect.Private.with_connector connectors computation
;;

module Handle = struct
  include Bonsai_test.Handle

  let create
    result_spec
    ?rpc_implementations
    ?connectors
    ?start_time
    ?optimize
    computation
    =
    computation
    |> add_rpc_implementations_to_computation ~rpc_implementations ~connectors
    |> Bonsai_test.Handle.create result_spec ?start_time ?optimize
  ;;

  let flush_async_and_bonsai
    ?(max_iterations = 100)
    ?(silence_between_frames = false)
    handle
    =
    let open Async_kernel in
    let rec loop i =
      if i = 0
      then
        raise_s [%message [%string "not stable after %{max_iterations#Int} iterations"]];
      if i < max_iterations && not silence_between_frames
      then print_endline "------ between bonsai frame ------";
      let%bind.Eager_deferred () =
        Async_kernel_scheduler.yield_until_no_jobs_remain ~may_return_immediately:true ()
      in
      recompute_view handle;
      if has_after_display_events handle || Async_kernel_scheduler.num_pending_jobs () > 0
      then loop (i - 1)
      else Deferred.unit
    in
    recompute_view handle;
    loop max_iterations
  ;;

  open Virtual_dom_test_helpers

  let get_element handle ~get_vdom ~selector =
    let node = handle |> last_result |> get_vdom |> Node_helpers.unsafe_convert_exn in
    Node_helpers.select_first_exn node ~selector
  ;;

  let lint_vdom
    ?(min_severity = Linter.Severity.Only_report_app_crashing_errors)
    ?expected_failures
    handle
    ~get_vdom
    =
    handle
    |> last_result
    |> get_vdom
    |> Node_helpers.unsafe_convert_exn
    |> Node_helpers.Linter.print_report ?expected_failures ~min_severity
  ;;

  let click_on
    ?extra_event_fields
    ?shift_key_down
    ?alt_key_down
    ?ctrl_key_down
    handle
    ~get_vdom
    ~selector
    =
    let element = get_element handle ~get_vdom ~selector in
    Node_helpers.User_actions.click_on
      element
      ?extra_event_fields
      ?shift_key_down
      ?alt_key_down
      ?ctrl_key_down
  ;;

  let set_checkbox
    ?extra_event_fields
    ?shift_key_down
    ?alt_key_down
    ?ctrl_key_down
    handle
    ~get_vdom
    ~selector
    ~checked
    =
    let element = get_element handle ~get_vdom ~selector in
    Node_helpers.User_actions.set_checkbox
      element
      ~checked
      ?extra_event_fields
      ?shift_key_down
      ?alt_key_down
      ?ctrl_key_down
  ;;

  let submit_form ?extra_event_fields handle ~get_vdom ~selector =
    let element = get_element handle ~get_vdom ~selector in
    Node_helpers.User_actions.submit_form element ?extra_event_fields
  ;;

  let focus ?extra_event_fields handle ~get_vdom ~selector =
    let element = get_element handle ~get_vdom ~selector in
    Node_helpers.User_actions.focus element ?extra_event_fields
  ;;

  let change ?extra_event_fields handle ~get_vdom ~selector ~value =
    let element = get_element handle ~get_vdom ~selector in
    Node_helpers.User_actions.change element ~value ?extra_event_fields
  ;;

  let blur ?extra_event_fields ?related_target handle ~get_vdom ~selector =
    let element = get_element handle ~get_vdom ~selector in
    let related_target =
      match related_target with
      | Some selector -> Some (get_element handle ~get_vdom ~selector)
      | None -> None
    in
    Node_helpers.User_actions.blur ?related_target element ?extra_event_fields
  ;;

  let mousemove ?extra_event_fields handle ~get_vdom ~selector =
    let element = get_element handle ~get_vdom ~selector in
    Node_helpers.User_actions.mousemove element ?extra_event_fields
  ;;

  let mouseenter ?extra_event_fields handle ~get_vdom ~selector =
    let element = get_element handle ~get_vdom ~selector in
    Node_helpers.User_actions.mouseenter element ?extra_event_fields
  ;;

  let wheel ?extra_event_fields handle ~get_vdom ~selector ~delta_y =
    let element = get_element handle ~get_vdom ~selector in
    Node_helpers.User_actions.wheel element ~delta_y ?extra_event_fields
  ;;

  let input_text ?extra_event_fields handle ~get_vdom ~selector ~text =
    let element = get_element handle ~get_vdom ~selector in
    Node_helpers.User_actions.input_text element ~text ?extra_event_fields
  ;;

  let input_files ?extra_event_fields handle ~get_vdom ~selector ~files =
    let element = get_element handle ~get_vdom ~selector in
    Node_helpers.User_actions.input_files element ~files ?extra_event_fields
  ;;

  let keydown
    ?extra_event_fields
    ?shift_key_down
    ?alt_key_down
    ?ctrl_key_down
    handle
    ~get_vdom
    ~selector
    ~key
    =
    let element = get_element handle ~get_vdom ~selector in
    Node_helpers.User_actions.keydown
      ?shift_key_down
      ?alt_key_down
      ?ctrl_key_down
      ?extra_event_fields
      element
      ~key
  ;;

  let trigger_hook handle ~get_vdom ~selector ~name type_id arg =
    get_element handle ~get_vdom ~selector
    |> Node_helpers.trigger_hook ~type_id ~name ~arg ~f:Fn.id
  ;;

  let trigger_hook_via handle ~get_vdom ~selector ~name type_id ~f arg =
    get_element handle ~get_vdom ~selector
    |> Node_helpers.trigger_hook ~type_id ~name ~arg ~f
  ;;

  let get_hook_value handle ~get_vdom ~selector ~name type_id =
    get_element handle ~get_vdom ~selector |> Node_helpers.get_hook_value ~type_id ~name
  ;;
end

module Expect_test_config = Bonsai_test.Expect_test_config
