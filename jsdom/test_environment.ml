open! Core
open! Bonsai_web
open Js_of_ocaml
open Async_kernel
open Jsdom_test

let () = Async_js.init ()
let hello_world _graph = Bonsai.return {%html|Hello!|}

let%expect_test "JSDom tests aren't in quirks mode" =
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
  Js.Unsafe.get Dom_html.document "compatMode" |> Js.to_string |> print_endline;
  [%expect {| CSS1Compat |}];
  return ()
;;
