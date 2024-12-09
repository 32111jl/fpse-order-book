type buy_sell = Buy | Sell

type order_type = 
  | Market                 (* market orders will match with the best avail price if that exists *)
  | Limit of { price : float; expiration : float option } (* limit orders allow the user to specify a price and expiration time *)
  | Margin of float        (* margin orders are limit orders with a percentage above the current price *)

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
  price : float;            (* price at which the trade was executed *)
}

type order_book = {
  security : string;
  mutable orders : db_order list;
  mutable best_bid : float option;
  mutable best_ask : float option;
}


type market_conditions = {
  bid_ask_spread : float;
  margin_rate : float;
}

type validation_result =
  | Valid
  | InvalidMarket of string
  | InvalidFunds of float * float
  | InvalidShares of float * float
  | NoPosition of string
  | InvalidUser
  | DatabaseError