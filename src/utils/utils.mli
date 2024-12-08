
(* the Order_types module is to export the types used in the order book *)
module Order_types : sig
  include module type of Order_types
end

val round_price : float -> float
(** [round_price price] rounds the price to the nearest cent *)

val round_quantity : float -> float
(** [round_quantity quantity] rounds the quantity to two decimal places *)

val random_float_between : float -> float -> float
(** [random_float_between min max] returns a random float between min and max *)

val random_price : float -> float -> float
(** [random_price base spread] returns a random price between base and base + spread *)

val current_time : unit -> float
(** [current_time ()] returns the current time in seconds since the epoch *)

val is_expired : float option -> bool
(** [is_expired expiration] returns true if the expiration time has passed *)

val string_to_order_type : string -> float -> Order_types.order_type
(** [string_to_order_type order_type_str expiration] converts a string to an order type *)

val order_type_to_string : Order_types.order_type -> string
(** [order_type_to_string order_type] converts an order type to a string *)

val string_to_buy_sell : string -> Order_types.buy_sell
(** [string_to_buy_sell buy_sell_str] converts a string to a buy/sell *)

val buy_sell_to_string : Order_types.buy_sell -> string
(** [buy_sell_to_string buy_sell] converts a buy/sell to a string *)

val unwrap_id : int option -> int
(** [unwrap_id id] returns the id if it exists, otherwise raises an exception *)
