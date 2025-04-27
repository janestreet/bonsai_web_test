open! Core
open! Bonsai_web
open Js_of_ocaml
open Jsdom
module Handle = Handle_experimental

let%expect_test "[set_document_title] sets html document title" =
  let%bind.With handle =
    Handle.with_ ~get_vdom:Fn.id (fun _graph ->
      Bonsai.return
        {%html|
          <button on_click=%{fun _ ->
          Effect.set_document_title "Hello World!"}></button>
        |})
  in
  Handle.print_dom handle;
  [%expect
    {|
    <html>
      <head>
        <meta charset="UTF-8"> </meta>
      </head>
      <body>
        <button tabindex="0" style="outline: none;"> </button>
      </body>
    </html>
    |}];
  Js.Unsafe.get Dom_html.document "title" |> Js.to_string |> print_endline;
  [%expect {| |}];
  Handle.click_on handle ~selector:"button";
  Handle.print_dom handle;
  [%expect
    {|
    <html>
      <head>
        <meta charset="UTF-8"> </meta>
        <title> Hello World! </title>
      </head>
      <body>
        <button tabindex="0" style="outline: none;"> </button>
      </body>
    </html>
    |}];
  Js.Unsafe.get Dom_html.document "title" |> Js.to_string |> print_endline;
  [%expect {| Hello World! |}]
;;

let%expect_test "[on_change_set_document_title] sets html document title to provided \
                 [string Bonsai.t] and updates when [string Bonsai.t] change"
  =
  let%bind.With handle =
    Handle.with_ ~get_vdom:Fn.id (fun graph ->
      let title, set_title = Bonsai.state "No title" graph in
      Effect.on_change_set_document_title title graph;
      let%map.Bonsai title and set_title in
      {%html|
        <button
          class="hello"
          on_click=%{fun _ ->
            match title with
            | "Hello World!" -> set_title "Goodbye World!"
            | _ -> set_title "Hello World!"}
        ></button>
      |})
  in
  Handle.print_dom handle;
  [%expect
    {|
    <html>
      <head>
        <meta charset="UTF-8"> </meta>
        <title> No title </title>
      </head>
      <body>
        <button class="hello" tabindex="0" style="outline: none;"> </button>
      </body>
    </html>
    |}];
  let click_and_print () =
    Handle.click_on handle ~selector:"button";
    Handle.one_frame handle;
    Js.Unsafe.get Dom_html.document "title" |> Js.to_string |> print_endline;
    print_string "\n";
    Handle.print_dom handle
  in
  click_and_print ();
  [%expect
    {|
    Hello World!

    <html>
      <head>
        <meta charset="UTF-8"> </meta>
        <title> Hello World! </title>
      </head>
      <body>
        <button class="hello" tabindex="0" style="outline: none;"> </button>
      </body>
    </html>
    |}];
  (* clicking again changes the value of title *)
  click_and_print ();
  [%expect
    {|
    Goodbye World!

    <html>
      <head>
        <meta charset="UTF-8"> </meta>
        <title> Goodbye World! </title>
      </head>
      <body>
        <button class="hello" tabindex="0" style="outline: none;"> </button>
      </body>
    </html>
    |}]
;;
