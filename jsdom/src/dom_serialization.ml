open! Core
open Js_of_ocaml
module Soup = Lambdasoup

let rec of_lambda_soup
  : type a.
    with_visible_whitespace:bool
    -> depth:int option
    -> a Soup.node
    -> Virtual_dom_test_helpers.Node_helpers.t
  =
  fun ~with_visible_whitespace ~depth soup ->
  let clean_whitespace string =
    String.map string ~f:(function
      | ' ' -> if with_visible_whitespace then '_' else ' '
      | c -> c)
    |> String.substr_replace_all
         ~pattern:"\n"
         ~with_:(if with_visible_whitespace then "↵" else "\n")
  in
  match Soup.children soup |> Soup.to_list, Soup.element soup with
  | children, Some element ->
    let convert_children ~depth =
      List.fold children ~init:[] ~f:(fun acc child ->
        let converted = of_lambda_soup ~with_visible_whitespace ~depth child in
        match acc, converted with
        | Virtual_dom_test_helpers.Node_helpers.Text text1 :: rest, Text text2 ->
          Text (text1 ^ text2) :: rest
        | _, Text "" -> acc
        | _ -> converted :: acc)
      |> List.rev
    in
    Element
      { tag_name = Soup.name element
      ; attributes =
          Soup.fold_attributes (fun acc key value -> (key, value) :: acc) [] element
          |> List.rev
      ; children =
          (match depth with
           | Some 0 when List.is_empty children -> []
           | Some 0 -> [ Text "..." ]
           | Some depth -> convert_children ~depth:(Some (depth - 1))
           | None -> convert_children ~depth:None)
      ; string_properties = []
      ; bool_properties = []
      ; handlers = []
      ; styles = []
      ; hooks = []
      ; key = None
      }
  | [], None ->
    (* Soup doesn't expose a text type / coersion, but if it's not an element, and has no
       children, it's gotta be a text node. *)
    let text =
      match Soup.texts soup with
      | [ text ] when (not with_visible_whitespace) && String.equal (String.strip text) ""
        -> ""
      | [ text ] -> clean_whitespace text
      | texts ->
        raise_s
          [%message
            "BUG! Soup text nodes must contain exactly one string" (texts : string list)]
    in
    Text text
  | first_child :: _, None -> of_lambda_soup ~with_visible_whitespace ~depth first_child
;;

let soup_to_string
  ?filter_printed_attributes
  ?censor_paths
  ?censor_hash
  ?path_censoring_message
  ?hash_censoring_message
  ?(with_visible_whitespace = false)
  ?depth
  soup
  =
  let open Virtual_dom_test_helpers.Node_helpers in
  of_lambda_soup ~with_visible_whitespace ~depth soup
  |> to_string_html
       ~filter_printed_attributes:
         (Bonsai_web.Test_selector.For_bonsai_web
          .filter_printed_attributes_with_test_selector_filtering
            ~filter_printed_attributes)
       ?censor_paths
       ?censor_hash
       ?path_censoring_message
       ?hash_censoring_message
;;

let pretty_print_html
  ?filter_printed_attributes
  ?censor_paths
  ?censor_hash
  ?path_censoring_message
  ?hash_censoring_message
  ?with_visible_whitespace
  ?depth
  string
  =
  Lambda_soup_js.parse string
  |> soup_to_string
       ?filter_printed_attributes
       ?censor_paths
       ?censor_hash
       ?path_censoring_message
       ?hash_censoring_message
       ?with_visible_whitespace
       ?depth
  |> print_endline
;;

let dom_to_string
  ?filter_printed_attributes
  ?censor_paths
  ?censor_hash
  ?path_censoring_message
  ?hash_censoring_message
  ?with_visible_whitespace
  ?depth
  ?node
  ()
  =
  (* NOTE: We'd like [node] to accept any subclass of [Dom.node Js.t], but when we give
     [document##.documentElement] as a default, the compiler narrows the type to that of
     the default argument. The [:>] operator doesn't seem to help.

     I haven't found a better option than [Js.Unsafe.coerce].
  *)
  Option.value node ~default:(Dom_html.document##.documentElement |> Js.Unsafe.coerce)
  |> Lambda_soup_js.parse_dom
  |> soup_to_string
       ?filter_printed_attributes
       ?censor_paths
       ?censor_hash
       ?path_censoring_message
       ?hash_censoring_message
       ?with_visible_whitespace
       ?depth
;;

let print_dom
  ?filter_printed_attributes
  ?censor_paths
  ?censor_hash
  ?path_censoring_message
  ?hash_censoring_message
  ?with_visible_whitespace
  ?depth
  ?node
  ()
  =
  dom_to_string
    ?filter_printed_attributes
    ?censor_paths
    ?censor_hash
    ?path_censoring_message
    ?hash_censoring_message
    ?with_visible_whitespace
    ?depth
    ?node
    ()
  |> print_endline
;;
