open! Core
open Js_of_ocaml

let mock_console_log () =
  let print = Js.wrap_callback (fun s -> print_endline (Js.to_string s)) in
  let f =
    Js.Unsafe.pure_js_expr
      {|(function (print) {
        console.log = (s, ...args) => {
          try {
            print(String(s) + String(args));
          } catch (e) {
           print("console.log ERROR: " + String(e));
           }
        }
        console.warn = (s, ...args) => {
          try {
            print("WARN: " + String(s) + String(args));
          } catch (e) {
           print("console.log ERROR: " + String(e));
           }
        }
      })|}
  in
  let () = Js.Unsafe.fun_call f [| Js.Unsafe.inject print |] in
  ()
;;

(* jsdom's hover simulation doesn't currently work, so we need to do this terrible hack.
     https://github.com/dperini/nwsapi/issues/119 *)
let mock_matches () =
  let open Js_of_ocaml in
  let f =
    Js.Unsafe.pure_js_expr
      {|(function (documentHasFocus) {
            const originalMatches = HTMLElement.prototype.matches;
            HTMLElement.prototype.matches = (function (selectors) {
                const hoveredFromHandle = globalThis.hoveredFromHandle;

                if (!hoveredFromHandle) return false;

                if (selectors === ":hover") {
                  return hoveredFromHandle.contains(this);
                }

                return originalMatches.call(this, selectors);
            });
          })|}
  in
  let () = Js.Unsafe.fun_call f [||] in
  ()
;;

let mock_has_focus ~has_focus =
  let open Js_of_ocaml in
  let document_has_focus = Js.wrap_callback (fun () -> has_focus () |> Js.bool) in
  let f =
    Js.Unsafe.pure_js_expr
      {|(function (documentHasFocus) {
            document.hasFocus = (function() {
              return documentHasFocus();
            })
          })|}
  in
  let () = Js.Unsafe.fun_call f [| Js.Unsafe.inject document_has_focus |] in
  ()
;;

module Animation_frame_tasks = struct
  let global_queue = ref None

  let mock () =
    let queue = Queue.create () in
    global_queue := Some queue;
    let window = Dom_html.window in
    Js.Unsafe.set
      window
      "requestAnimationFrame"
      (Js.wrap_callback (fun (f : (unit, Js.number_t -> unit) Js.meth_callback) ->
         Queue.enqueue queue (fun () ->
           if not (Option.mem !global_queue queue ~equal:phys_equal)
           then
             raise_s
               [%message
                 "BUG: global mocked requestAnimationFrame queue has changed since this \
                  task was scheduled"];
           Js.Unsafe.fun_call f [| Js.Unsafe.inject () |])))
  ;;

  let run_queued () =
    match !global_queue with
    | None ->
      raise_s
        [%message "BUG: cannot run requestAnimationQueue tasks, no mock queue installed!"]
    | Some queue ->
      let all = Queue.to_list queue in
      if List.is_empty all
      then print_endline "WARNING: no requestAnimationFrame tasks queued!";
      Queue.clear queue;
      List.iter all ~f:(fun f -> f ())
  ;;

  let cleanup () =
    match !global_queue with
    | None -> print_endline "WARNING: no mock requestAnimationFrame queue installed"
    | Some queue ->
      if not (Queue.is_empty queue)
      then print_endline "WARNING: requestAnimationFrame queue not empty upon cleanup";
      global_queue := None
  ;;
end
