(** CLI interface for user interaction *)

val run_cli : unit -> unit
(** [run_cli ()] starts the CLI interface for user interaction. *)

val set_user_id : unit -> unit
(** [set_user_id ()] sets the user ID for the current session. *)

val place_order : string -> Order_book_lib.Order.order_type -> Order_book_lib.Order.buy_sell -> float -> int -> unit
(** [place_order security order_type buy_sell qty user_id] submits the order to the matching engine. *)

val place_order_interactive : unit -> unit
(** [place_order_interactive ()] submits the order to the matching engine interactively. *)

val cancel_order : unit -> unit
(** [cancel_order ()] cancels the order from the order book. *)

val view_book : unit -> unit
(** [view_book ()] displays the current state of the order book. *)

val view_my_orders : unit -> unit
(** [view_my_orders ()] displays the current orders of the user. *)

val view_bal : unit -> unit
(** [view_bal ()] displays the current balance of the user. *)