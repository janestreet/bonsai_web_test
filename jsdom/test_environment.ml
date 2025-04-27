open! Core
open! Bonsai_web
open Js_of_ocaml
open Jsdom
module Handle = Handle_experimental

let hello_world (local_ _graph) = Bonsai.return {%html|Hello!|}

let%expect_test "JSDom tests aren't in quirks mode" =
  let%bind.With handle = Handle.with_ ~get_vdom:Fn.id hello_world in
  Handle.print_dom handle;
  [%expect
    {|
    <html>
      <head>
        <meta charset="UTF-8"> </meta>
      </head>
      <body> Hello! </body>
    </html>
    |}];
  Js.Unsafe.get Dom_html.document "compatMode" |> Js.to_string |> print_endline;
  [%expect {| CSS1Compat |}]
;;
