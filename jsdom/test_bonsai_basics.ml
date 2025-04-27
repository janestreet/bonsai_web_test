open! Core
open! Bonsai_web
open Jsdom
module Handle = Handle_experimental

let hello_world (local_ _graph) = Bonsai.return {%html|Hello!|}

let%expect_test "Constant hello world" =
  let%bind.With handle = Handle.with_ ~get_vdom:Fn.id hello_world in
  Handle.one_frame handle;
  Handle.print_dom handle;
  [%expect
    {|
    <html>
      <head>
        <meta charset="UTF-8"> </meta>
      </head>
      <body> Hello! </body>
    </html>
    |}]
;;

let counter (local_ graph) =
  let open Bonsai.Let_syntax in
  let count, inject =
    Bonsai.state_machine
      ~default_model:0
      ~apply_action:(fun _ctx count -> function
        | `Incr -> count + 1
        | `Decr -> count - 1)
      graph
  in
  let%arr count and inject in
  {%html|
    <div>
      <button id="incr" on_click=%{fun _ -> print_endline "Hello!"; inject `Incr}>
        -
      </button>
      %{count#Int}
      <button id="decr" on_click=%{fun _ -> inject `Incr}>+</button>
    </div>
  |}
;;

let%expect_test "Counter w/ state" =
  let%bind.With handle = Handle.with_ ~get_vdom:Fn.id counter in
  Handle.print_dom handle;
  [%expect
    {|
    <html>
      <head>
        <meta charset="UTF-8"> </meta>
      </head>
      <body>
        <div tabindex="0" style="outline: none;">
          <button id="incr">  -  </button>
          0
          <button id="decr"> + </button>
        </div>
      </body>
    </html>
    |}];
  Handle.click_on handle ~selector:"#incr";
  Handle.one_frame handle;
  Handle.print_dom handle;
  [%expect
    {|
    Hello!
    <html>
      <head>
        <meta charset="UTF-8"> </meta>
      </head>
      <body>
        <div tabindex="0" style="outline: none;">
          <button id="incr">  -  </button>
          1
          <button id="decr"> + </button>
        </div>
      </body>
    </html>
    |}]
;;

let lifecycle_effects (local_ graph) =
  let open Bonsai.Let_syntax in
  let which, cycle =
    Bonsai.state_machine
      ~default_model:`A
      ~apply_action:(fun _ctx model () ->
        match model with
        | `A -> `B
        | `B -> `C
        | `C -> `A)
      graph
  in
  let view =
    match%sub which with
    | `A ->
      (* We don't test after_display because the number of frames that pass is not
         deterministic with jsdom. *)
      Bonsai.Edge.lifecycle
        ~on_activate:(return (Effect.print_s [%message ">> Activating A"]))
        ~on_deactivate:(return (Effect.print_s [%message ">> Deactivating A"]))
        ~after_display:(return (Effect.print_s [%message ">> After Display A"]))
        graph;
      return {%html|A! :)|}
    | `B -> return {%html|B :(|}
    | `C -> return {%html|C!!!!!|}
  in
  Bonsai.Edge.on_change'
    which
    ~callback:
      (return (fun prev new_ ->
         Effect.print_s
           [%message
             ">> Change!" (prev : [ `A | `B | `C ] option) (new_ : [ `A | `B | `C ])]))
    ~equal:[%equal: [ `A | `B | `C ]]
    graph;
  let%arr view and cycle in
  {%html|
    <div>
      %{view}
      <button on_click=%{fun _ -> cycle ()} id="cycle">Cycle</button>
    </div>
  |}
;;

let%expect_test "Lifecycle effects" =
  let%bind.With handle = Handle.with_ ~get_vdom:Fn.id lifecycle_effects in
  Handle.print_dom handle;
  [%expect
    {|
    ">> Activating A"
    ">> After Display A"
    (">> Change!" (prev ()) (new_ A))
    <html>
      <head>
        <meta charset="UTF-8"> </meta>
      </head>
      <body>
        <div tabindex="0" style="outline: none;">
          A! :)
          <button id="cycle"> Cycle </button>
        </div>
      </body>
    </html>
    |}];
  Handle.click_on handle ~selector:"#cycle";
  Handle.one_frame handle;
  Handle.print_dom handle;
  [%expect
    {|
    ">> Deactivating A"
    (">> Change!" (prev (A)) (new_ B))
    <html>
      <head>
        <meta charset="UTF-8"> </meta>
      </head>
      <body>
        <div tabindex="0" style="outline: none;">
          B :(
          <button id="cycle"> Cycle </button>
        </div>
      </body>
    </html>
    |}];
  Handle.click_on handle ~selector:"#cycle";
  Handle.one_frame handle;
  Handle.print_dom handle;
  [%expect
    {|
    (">> Change!" (prev (B)) (new_ C))
    <html>
      <head>
        <meta charset="UTF-8"> </meta>
      </head>
      <body>
        <div tabindex="0" style="outline: none;">
          C!!!!!
          <button id="cycle"> Cycle </button>
        </div>
      </body>
    </html>
    |}];
  Handle.click_on handle ~selector:"#cycle";
  Handle.one_frame handle;
  Handle.print_dom handle;
  [%expect
    {|
    ">> Activating A"
    ">> After Display A"
    (">> Change!" (prev (C)) (new_ A))
    <html>
      <head>
        <meta charset="UTF-8"> </meta>
      </head>
      <body>
        <div tabindex="0" style="outline: none;">
          A! :)
          <button id="cycle"> Cycle </button>
        </div>
      </body>
    </html>
    |}]
;;
