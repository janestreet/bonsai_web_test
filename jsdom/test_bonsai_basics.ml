open! Core
open! Bonsai_web
open Async_kernel
open Jsdom_test

let () = Async_js.init ()
let hello_world _graph = Bonsai.return {%html|Hello!|}

let%expect_test "Constant hello world" =
  Bonsai_web.Start.start hello_world;
  let%bind.Deferred () = run_animation_frame () in
  print_dom ~with_visible_whitespace:false ();
  [%expect
    {|
    | <html>
    |   <head>
    |
    |     <meta charset="UTF-8">
    |
    |     </head>
    |
    |     <body>
    |               Hello!
    |     </body>
    |   </html>
    |}];
  return ()
;;

let counter graph =
  let open Bonsai.Let_syntax in
  let count, inject =
    Bonsai.state_machine0
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
  Bonsai_web.Start.start counter;
  let%bind.Deferred () = run_animation_frame () in
  print_dom ~with_visible_whitespace:false ();
  [%expect
    {|
    | <html>
    |   <head>
    |
    |     <meta charset="UTF-8">
    |
    |     </head>
    |
    |     <body>
    |
    |       <div tabindex="0" class="private-app-root-for-inertness" style="outline: none;">
    |         <button id="incr">
    |            -
    |         </button>
    |         0
    |         <button id="decr">
    |           +
    |         </button>
    |       </div>
    |
    |     </body>
    |   </html>
    |}];
  dispatch_event ~selector:"#incr" ~event_name:"click" ();
  let%bind.Deferred () = run_animation_frame () in
  print_dom ~with_visible_whitespace:false ();
  [%expect
    {|
    Hello!
    | <html>
    |   <head>
    |
    |     <meta charset="UTF-8">
    |
    |     </head>
    |
    |     <body>
    |
    |       <div tabindex="0" class="private-app-root-for-inertness" style="outline: none;">
    |         <button id="incr">
    |            -
    |         </button>
    |         1
    |         <button id="decr">
    |           +
    |         </button>
    |       </div>
    |
    |     </body>
    |   </html>
    |}];
  return ()
;;

let lifecycle_effects graph =
  let open Bonsai.Let_syntax in
  let which, cycle =
    Bonsai.state_machine0
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
  Bonsai_web.Start.start lifecycle_effects;
  let%bind.Deferred () = run_animation_frame () in
  print_dom ~with_visible_whitespace:false ();
  (* The first frame is run synchronously on startup, so the first [run_animation_frame]
     call actually resolves after 2 frames have run. *)
  [%expect
    {|
    ">> Activating A"
    ">> After Display A"
    (">> Change!" (prev ()) (new_ A))
    ">> After Display A"
    | <html>
    |   <head>
    |
    |     <meta charset="UTF-8">
    |
    |     </head>
    |
    |     <body>
    |
    |       <div tabindex="0" class="private-app-root-for-inertness" style="outline: none;">
    |         A! :)
    |         <button id="cycle">
    |           Cycle
    |         </button>
    |       </div>
    |
    |     </body>
    |   </html>
    |}];
  dispatch_event ~selector:"#cycle" ~event_name:"click" ();
  let%bind.Deferred () = run_animation_frame () in
  print_dom ~with_visible_whitespace:false ();
  [%expect
    {|
    ">> Deactivating A"
    (">> Change!" (prev (A)) (new_ B))
    | <html>
    |   <head>
    |
    |     <meta charset="UTF-8">
    |
    |     </head>
    |
    |     <body>
    |
    |       <div tabindex="0" class="private-app-root-for-inertness" style="outline: none;">
    |         B :(
    |         <button id="cycle">
    |           Cycle
    |         </button>
    |       </div>
    |
    |     </body>
    |   </html>
    |}];
  dispatch_event ~selector:"#cycle" ~event_name:"click" ();
  let%bind.Deferred () = run_animation_frame () in
  print_dom ~with_visible_whitespace:false ();
  [%expect
    {|
    (">> Change!" (prev (B)) (new_ C))
    | <html>
    |   <head>
    |
    |     <meta charset="UTF-8">
    |
    |     </head>
    |
    |     <body>
    |
    |       <div tabindex="0" class="private-app-root-for-inertness" style="outline: none;">
    |         C!!!!!
    |         <button id="cycle">
    |           Cycle
    |         </button>
    |       </div>
    |
    |     </body>
    |   </html>
    |}];
  dispatch_event ~selector:"#cycle" ~event_name:"click" ();
  let%bind.Deferred () = run_animation_frame () in
  print_dom ~with_visible_whitespace:false ();
  [%expect
    {|
    ">> Activating A"
    ">> After Display A"
    (">> Change!" (prev (C)) (new_ A))
    | <html>
    |   <head>
    |
    |     <meta charset="UTF-8">
    |
    |     </head>
    |
    |     <body>
    |
    |       <div tabindex="0" class="private-app-root-for-inertness" style="outline: none;">
    |         A! :)
    |         <button id="cycle">
    |           Cycle
    |         </button>
    |       </div>
    |
    |     </body>
    |   </html>
    |}];
  return ()
;;
