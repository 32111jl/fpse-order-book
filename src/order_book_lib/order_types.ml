open Price

type buy_sell = Buy | Sell

type order_type = 
  | Market                 (* market orders will match with the best avail price if that exists *)
  | Limit of { price : price; expiration : float option } (* limit orders allow the user to specify a price and expiration time *)
  | Margin of price        (* margin orders are limit orders with a percentage above the current price *)
(* float equality wishy washy, want a more precise *)

type db_order = {
  id : int option;          (* order ID *)  
  user_id : int;            (* ID of the user who placed the order *)
  security : string;        (* security being traded (ex. "AAPL") *)
  order_type : order_type;  (* type of order (Market, Limit, Margin) *)
  buy_sell : buy_sell;      (* indicates if order is to buy or sell *)
  qty : float;              (* quantity to buy or sell *)
}

type trade = {
  buy_order_id : int;       (* ID of the buy order *)
  sell_order_id : int;      (* ID of the sell order *)
  security : string;        (* security being traded (ex. "AAPL") *)
  qty : float;              (* quantity to buy or sell *)
  price : price;            (* price at which the trade was executed *)
}

type order_book = {
  security : string;
  mutable orders : db_order list;
  mutable best_bid : price option;
  mutable best_ask : price option;
}


type market_conditions = {
  bid_ask_spread : float;
  margin_rate : float;
}

type validation_result =
  | Valid
  | InvalidMarket of string
  | InvalidFunds of price * price
  | InvalidShares of price * price
  | NoPosition of string
  | InvalidUser
  | DatabaseError