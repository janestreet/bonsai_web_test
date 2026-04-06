open! Core

(** Mocks the [navigate] event that triggers [~navigation:`Intercept]. This should only be
    used in test frameworks (e.g. jsdom or bonsai_web_test). *)
val mock_navigate
  :  ?download:bool
  -> ?ctrl_key_down:bool
  -> ?shift_key_down:bool
  -> ?alt_key_down:bool
  -> href:string
  -> target:string
  -> unit
  -> unit
