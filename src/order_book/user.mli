type user = {
  id : int;
  mutable balance : float;
  mutable positions : (string, float) Hashtbl.t;
}

val create_user : int -> float -> user
(** [create_user id initial_balance] creates a new user with the given ID and initial balance *)

val get_balance : int -> float
(** [get_balance user_id] gets the balance for the given user ID. Returns 0.0 if user doesn't exist *)

val update_balance : int -> float -> unit
(** [update_balance user_id amount] updates the balance for the given user ID by adding the amount.
    If the user doesn't exist, creates a new user with that amount as balance. *)

val get_position : int -> string -> float
(** [get_position user_id security] gets the position for the given user ID and security. Returns 0.0 if user or security doesn't exist *)

val update_position : int -> string -> float -> unit
(** [update_position user_id security qty] updates the position for the given user ID by adding the quantity.
    If the user doesn't exist, creates a new user with that quantity as position. *)