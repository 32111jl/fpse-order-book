
module Order_types : sig
  include module type of Order_types
end

val round_price : float -> float
val round_quantity : float -> float
val random_float_between : float -> float -> float
val random_price : float -> float -> float
val current_time : unit -> float
val is_expired : float option -> bool
val string_to_order_type : string -> float -> Order_types.order_type
val order_type_to_string : Order_types.order_type -> string
val string_to_buy_sell : string -> Order_types.buy_sell
val buy_sell_to_string : Order_types.buy_sell -> string
