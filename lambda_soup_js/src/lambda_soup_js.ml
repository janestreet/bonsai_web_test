open! Core
open Js_of_ocaml
module Soup = Lambdasoup

class type dom_parser = object
  method parseFromString :
    Js.js_string Js.t -> Js.js_string Js.t -> Dom_html.document Js.t Js.meth
end

let dom_parser =
  lazy
    (let (domParser : (unit -> dom_parser Js.t) Js.constr) =
       Js.Unsafe.global##._DOMParser
     in
     new%js domParser ())
;;

let get_attributes_raw
  : Dom.element Js.t -> Js.js_string Js.t Js.js_array Js.t Js.js_array Js.t
  =
  fun element ->
  let f =
    Js.Unsafe.pure_js_expr
      {js|
  (function f (element) {
    return Array.from(element.attributes, attr => [attr.name, attr.value])
  })|js}
  in
  Js.Unsafe.fun_call f [| Js.Unsafe.inject element |]
;;

let get_attributes node =
  let attributes = get_attributes_raw node in
  Js.to_array attributes
  |> Array.to_list
  |> List.rev
  |> List.filter_map ~f:(fun attribute ->
    match Js.to_array attribute |> Array.to_list with
    | [ key; value ] -> Some (Js.to_string key, Js.to_string value)
    | _ -> None)
;;

let parse_dom root =
  let rec loop element =
    let tag = element##.tagName |> Js.to_string |> String.lowercase in
    let attributes = get_attributes element in
    let soup = Soup.create_element tag ~attributes in
    Dom.list_of_nodeList element##.childNodes
    |> List.iter ~f:(fun child ->
      match Dom.CoerceTo.element child |> Js.Opt.to_option with
      | Some element -> Soup.append_child soup (loop element)
      | None ->
        (match Dom.CoerceTo.text child |> Js.Opt.to_option with
         | Some text ->
           Soup.append_child soup (Soup.create_text (Js.to_string text##.data))
         | None ->
           (* This should only happen with comments, which Lamba Soup doesn't support. *)
           ()));
    soup
  in
  let root_element =
    (* The root HTML element must be a DOM element. *)
    loop (Dom.CoerceTo.element root |> Js.Opt.to_option |> Option.value_exn)
  in
  let soup = Soup.create_soup () in
  Soup.append_root soup root_element;
  soup
;;

let parse html =
  let root =
    ((force dom_parser)##parseFromString (Js.string html) (Js.string "text/html"))##.documentElement
  in
  parse_dom root
;;
