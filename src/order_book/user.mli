(* User module *)

type trade_record = {
  timestamp : float;
  security : string;
  qty : float;
  price : float;
  is_buy : bool;
}

type user = {
  id : int;
  mutable balance : float;
  mutable positions : (string, float) Hashtbl.t;
  mutable trade_history : (string, trade_record list) Hashtbl.t;
}

val create_user : int -> float -> (Postgresql.result, string) result
(** [create_user id initial_balance] creates a new user with the given ID and initial balance *)

val get_balance : int -> float option
(** [get_balance user_id] gets the balance for the given user ID. Returns 0.0 if user doesn't exist *)

val update_balance : int -> float -> (Postgresql.result, string) result
(** [update_balance user_id amount] updates the balance for the given user ID by adding the amount.
    If the user doesn't exist, creates a new user with that amount as balance. *)

val get_position : int -> string -> float option
(** [get_position user_id security] gets the position for the given user ID and security. Returns 0.0 if user or security doesn't exist *)

val update_position : int -> string -> float -> (Postgresql.result, string) result
(** [update_position user_id security qty] updates the position for the given user ID by adding the quantity.
    If the user doesn't exist, creates a new user with that quantity as position. *)