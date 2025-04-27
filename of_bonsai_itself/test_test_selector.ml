open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open! Virtual_dom
open! Bonsai_web_test

module%test Simple_selector = struct
  let increment_selector = Bonsai.Test_selector.make ()
  let count_selector = Bonsai.Test_selector.make ()
  let decrement_selector = Bonsai.Test_selector.make ()

  let counter (local_ graph) =
    let state, action =
      Bonsai.state_machine
        ~default_model:0
        ~apply_action:(fun _ctx count -> function
          | `Incr -> count + 1
          | `Decr -> count - 1)
        graph
    in
    let%arr state and action in
    {%html|
      <div>
        <button
          on_click=%{fun _ -> action `Decr}
          %{Bonsai.Test_selector.attr decrement_selector}
        >
          -1
        </button>
        <span %{Bonsai.Test_selector.attr count_selector}>%{state#Int}</span>
        <button
          on_click=%{fun _ -> action `Incr}
          %{Bonsai.Test_selector.attr increment_selector}
        >
          +1
        </button>
      </div>
    |}
  ;;

  let%expect_test "basic integration test using the test selector" =
    let handle =
      Handle.create
        (Result_spec.vdom ~selector:(test_selector count_selector) Fn.id)
        counter
    in
    Handle.show handle;
    (* This assertion should show a span with a 0 in it. It is important that:
       1. We do not see any test selector in here - they should get stripped!
       2. We only see the span and not the containing div + the buttons *)
    [%expect {| <span> 0 </span> |}];
    Handle.click_on ~get_vdom:Fn.id ~selector:(test_selector increment_selector) handle;
    Handle.click_on ~get_vdom:Fn.id ~selector:(test_selector increment_selector) handle;
    Handle.show_diff handle;
    (* Should a diff to 2 because the correct increment selector has been hit twice *)
    [%expect
      {|
      -|<span> 0 </span>
      +|<span> 2 </span>
      |}];
    Handle.click_on ~get_vdom:Fn.id ~selector:(test_selector decrement_selector) handle;
    Handle.click_on ~get_vdom:Fn.id ~selector:(test_selector increment_selector) handle;
    Handle.show handle;
    (* Should show 2 because the correct increment and decrement selectors has been both hit *)
    [%expect {| <span> 2 </span> |}]
  ;;
end

module%test Keyed = struct
  let selected_selector = Bonsai.Test_selector.make ()
  let choice_selectors = Bonsai.Test_selector.Keyed.create (module String)

  let chooser ~choices (local_ graph) =
    let state, set_state = Bonsai.state_opt graph in
    let choices =
      Bonsai.assoc_set
        (module String)
        choices
        ~f:(fun c (local_ _graph) ->
          let%arr c and set_state in
          {%html|
            <button
              on_click=%{fun _ -> set_state (Some c)}
              %{Bonsai.Test_selector.attr (Bonsai.Test_selector.Keyed.get choice_selectors c)}
            >
              %{c#String}
            </button>
          |})
        graph
    in
    let selected =
      match%arr state with
      | Some s ->
        {%html|<p %{Bonsai.Test_selector.attr selected_selector}>%{s#String}</p>|}
      | None -> Vdom.Node.none
    in
    let%arr selected and choices in
    {%html|<div>%{selected}*{Map.data choices}</div>|}
  ;;

  let%expect_test "integration test using the test selector bag" =
    let handle =
      Handle.create
        (Result_spec.vdom ~selector:(test_selector selected_selector) Fn.id)
        (chooser ~choices:(Bonsai.return (String.Set.of_list [ "foo"; "bar"; "baz" ])))
    in
    Handle.click_on
      ~get_vdom:Fn.id
      ~selector:(test_selector (Bonsai.Test_selector.Keyed.get choice_selectors "bar"))
      handle;
    Handle.show handle;
    (* Should show a [p] with [bar] in it *)
    [%expect {| <p> bar </p> |}]
  ;;

  let%expect_test "helpful error message if the show selector is not found" =
    let handle =
      Handle.create
        (Result_spec.vdom ~selector:(test_selector selected_selector) Fn.id)
        (chooser ~choices:(Bonsai.return String.Set.empty))
    in
    Handle.show handle
    (* This assertion should show a failure showing the location of the selector
       and the dom where it did not find the selector. *)
  [@@expect.uncaught_exn
    {|
    ("Failed to find element matching selector"
      (selector
        "[data-bonsai-test-selector='((here lib/bonsai/web_test/of_bonsai_itself/test_test_selector.ml:70:26))']")
      (from_node
         "<div>\
        \n  <Vdom.Node.none-widget> </Vdom.Node.none-widget>\
        \n</div>"))
    |}]
  ;;

  let%expect_test "helpful error message if the interaction selector is not found" =
    let handle =
      Handle.create
        (Result_spec.vdom ~selector:(test_selector selected_selector) Fn.id)
        (chooser ~choices:(Bonsai.return String.Set.empty))
    in
    Handle.click_on
      ~selector:(test_selector (Bonsai.Test_selector.Keyed.get choice_selectors "bar"))
      ~get_vdom:Fn.id
      handle
    (* This assertion should show a failure showing the location of the selector,
       the sexp of the instance and the dom where it did not find the selector. *)
  [@@expect.uncaught_exn
    {|
    ("Failed to find element matching selector"
      (selector
        "[data-bonsai-test-selector='((here lib/bonsai/web_test/of_bonsai_itself/test_test_selector.ml:71:25)(bag_inst bar))']")
      (from_node
         "<div>\
        \n  <Vdom.Node.none-widget> </Vdom.Node.none-widget>\
        \n</div>"))
    |}]
  ;;

  let%expect_test "test selector bag for which the instances have special chars that \
                   could break the css selector"
    =
    let tricky_string = "{[('\\\"" in
    let handle =
      Handle.create
        (Result_spec.vdom ~selector:(test_selector selected_selector) Fn.id)
        (chooser ~choices:(Bonsai.return (String.Set.singleton tricky_string)))
    in
    Handle.click_on
      ~get_vdom:Fn.id
      ~selector:
        (test_selector (Bonsai.Test_selector.Keyed.get choice_selectors tricky_string))
      handle;
    Handle.show handle;
    (* Should show a [p] with a weird string in it *)
    [%expect {| <p> {[('\" </p> |}]
  ;;
end
