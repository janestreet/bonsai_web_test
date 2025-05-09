open! Core
open Js_of_ocaml

(** [pretty_print_html string] will pretty-print an HTML string. *)
val pretty_print_html
  :  ?filter_printed_attributes:(key:string -> data:string -> bool)
  -> ?censor_paths:bool
  -> ?censor_hash:bool
  -> ?path_censoring_message:string
  -> ?hash_censoring_message:string
  -> ?with_visible_whitespace:bool
  -> ?depth:int
  -> string
  -> unit

(** [print_dom] pretty-prints [node], or [document.outerHTML] if no [node] is provided. *)
val print_dom
  :  ?filter_printed_attributes:(key:string -> data:string -> bool)
  -> ?censor_paths:bool
  -> ?censor_hash:bool
  -> ?path_censoring_message:string
  -> ?hash_censoring_message:string
  -> ?with_visible_whitespace:bool
  -> ?depth:int
  -> ?node:#Dom.node Js.t
  -> unit
  -> unit

(** [dom_to_string] is like [print_dom], but returns the result as a pretty-printed
    instead of printing it to the DOM. *)
val dom_to_string
  :  ?filter_printed_attributes:(key:string -> data:string -> bool)
  -> ?censor_paths:bool
  -> ?censor_hash:bool
  -> ?path_censoring_message:string
  -> ?hash_censoring_message:string
  -> ?with_visible_whitespace:bool
  -> ?depth:int
  -> ?node:#Dom.node Js.t
  -> unit
  -> string
