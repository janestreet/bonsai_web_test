open! Core
open Js_of_ocaml

let mock_navigate
  ?(download = false)
  ?(ctrl_key_down = false)
  ?(shift_key_down = false)
  ?(alt_key_down = false)
  ~href
  ~target
  ()
  =
  let current_host =
    let%map.Option window_location =
      Js.Optdef.to_option (Js.Optdef.return Dom_html.window##.location)
    in
    window_location##.host |> Js.to_string
  in
  let href_host = Uri.host (Uri.of_string href) in
  let is_internal_link =
    match current_host, href_host with
    | _, None -> true
    | None, Some _ ->
      (* If we don't have access to window.location.origin, then the application being
         tested can't use it to dynamically create URIs, either. We assume that any URI
         with a hostname must be hardcoded to be some (probably external) site. *)
      false
    | Some current_host, Some href_host ->
      (* In this case, the application might be using window.location.origin to construct
         its URIs. *)
      String.equal current_host href_host
  in
  match
    download, ctrl_key_down, shift_key_down, alt_key_down, target, is_internal_link
  with
  | true, _, _, _, _, _ | _, false, false, true, _, _ ->
    (* Download *)
    print_endline [%string "Downloading: %{href}"]
  | _, true, _, _, _, _ | _, _, true, _, _, _ | false, false, false, false, "_blank", _ ->
    (* New tab/window *)
    print_endline [%string "Opening in new tab/window: %{href}"]
  | false, false, false, false, "_self", true ->
    (* Internal, same-page link *)
    let dispatch_navigation_event =
      Js.Unsafe.pure_js_expr
        {js|
          (function(href, print_endline_concat) {
            print_endline_concat ("Opening in same tab: ", href);
            if (globalThis.navigation) {
              let event = {
                canIntercept: true,
                type: "navigate",
                intercept: (obj) => { obj.handler (); },
                destination: {
                  url: href
                },
              };
              globalThis.navigation.dispatchEvent(event);
            }
          })|js}
    in
    let () =
      Js.Unsafe.fun_call
        dispatch_navigation_event
        [| Js.Unsafe.inject (Js.string href)
         ; Js.Unsafe.inject
             (Js.wrap_callback (fun a b ->
                print_endline (Js.to_string a ^ Js.to_string b)))
        |]
    in
    ()
  | false, false, false, false, "_self", false ->
    (* (Probably) external, same-page link. Only internal if the hostname happens to be
       the hostname we're actually on (e.g. if you hardcode links to the prod site) *)
    print_endline [%string "Opening external link in same tab: %{href}"]
  | _, _, _, _, _, _ ->
    (* Some other target *)
    print_endline [%string "Opening with target %{target}: %{href}"]
;;
