open! Core
open! Bonsai_web
open Jsdom
module Handle = Handle_experimental

let%expect_test "Bonk does not delay by a frame" =
  let%bind.With handle =
    Handle.with_ ~get_vdom:Fn.id (fun graph ->
      let open Bonsai.Let_syntax in
      let%arr bonk = Bonsai_extra.bonk graph in
      let many_bonked =
        (Fn.apply_n_times ~n:10_000 bonk) (Effect.print_s [%message "many bonked"])
      in
      {%html|
        <button on_click=%{fun _ -> Effect.all_unit [
          Effect.print_s [%message "bonked"] |> bonk;
          many_bonked;
          Effect.print_s [%message "immediate"];
        ]}></button>
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
  Handle.click_on handle ~selector:"button";
  [%expect {| immediate |}];
  Handle.one_frame handle;
  [%expect
    {|
    bonked
    "many bonked"
    |}]
;;
