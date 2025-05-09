open! Core
open Js_of_ocaml
module Soup = Lambdasoup

(** Unfortunately, [Soup.parse] stack overflows on JavaScript:

    https://github.com/aantron/markup.ml/issues/26

    This is an alternative implementation that uses the browser's [DOMParser] API.

    Note that to test this, you'll need to enable JSDom, and only run tests with JS and
    wasm. *)

val parse : string -> Soup.soup Soup.node
val parse_dom : #Dom.node Js.t -> Soup.soup Soup.node
