type buy_sell = Buy | Sell

type order_type = 
  | Market
  | Limit of { price : float; expiration : float option }
  | Margin of float

type order = {
  id : int;
  security : string;
  order_type : order_type;
  buy_sell : buy_sell;
  mutable qty : float;
  timestamp : float;
  user_id : int;
}

type db_order = {
  id : int;
  user_id : int;
  security : string;
  order_type : order_type;
  buy_sell : buy_sell;
  qty : float;
}

type trade = {
  buy_order_id : int;
  sell_order_id : int;
  security : string;
  qty : float;
  price : float;
}