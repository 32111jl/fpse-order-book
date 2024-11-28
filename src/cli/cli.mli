(** CLI interface for user interaction *)

val run_cli : unit -> unit
(** [run_cli ()] starts the CLI interface for user interaction. *)

val set_user_id : unit -> unit
(** [set_user_id ()] sets the user ID for the current session. *)

val place_order : unit -> unit
(** [place_order ()] submits the order to the matching engine. *)

val cancel_order : unit -> unit
(** [cancel_order ()] cancels the order from the order book. *)

val view_book : unit -> unit
(** [view_book ()] displays the current state of the order book. *)

val view_my_orders : unit -> unit
(** [view_my_orders ()] displays the current orders of the user. *)

val view_bal : unit -> unit
(** [view_bal ()] displays the current balance of the user. *)