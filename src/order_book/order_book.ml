open Order

module PriceMap = Map.Make(struct
  type t = float
  let compare = Float.compare
end)

type order_book = {
  security : string;
  mutable bids : Order.order Queue.t PriceMap.t;
  mutable asks : Order.order Queue.t PriceMap.t;
  order_ids : (int, Order.order) Hashtbl.t;
  mutable order_counter : int;
  mutable best_bid : float option;
  mutable best_ask : float option;
}

let create_order_book (security : string) = 
  {
    security = security;
    bids = PriceMap.empty;
    asks = PriceMap.empty;
    order_ids = Hashtbl.create 1024;
    order_counter = 0;
    best_bid = None;
    best_ask = None;
  }

let generate_order_id (order_book : order_book) =
  let id = order_book.order_counter in
  order_book.order_counter <- id + 1;
  id

let get_security (order_book : order_book) = order_book.security

let get_price (order : Order.order) : float option = 
  match order.order_type with
  | Market -> None
  | Limit { price; _ } -> Some price
  | Margin price -> Some price

let get_price_helper order =
  match get_price order with
  | None -> failwith "Expected order to have a price."
  | Some price -> price

let get_best_bid (order_book : order_book) = 
  match order_book.best_bid with
  | Some _ as bid -> bid
  | None ->
      let bid = match PriceMap.max_binding_opt order_book.bids with
      | None -> None 
      | Some (price, _) -> Some price
      in
      order_book.best_bid <- bid;
      bid

let get_best_ask (order_book : order_book) = 
  match order_book.best_ask with
  | Some _ as ask -> ask
  | None ->
      let ask = match PriceMap.min_binding_opt order_book.asks with
      | None -> None
      | Some (price, _) -> Some price
      in
      order_book.best_ask <- ask;
      ask

let get_bids (order_book : order_book) = 
  PriceMap.fold (fun _ q acc -> 
    Queue.fold (fun acc order -> order :: acc) acc q
  ) order_book.bids [] |> List.rev

let get_asks (order_book : order_book) = 
  PriceMap.fold (fun _ q acc ->
    Queue.fold (fun acc order -> order :: acc) acc q
  ) order_book.asks [] |> List.rev

let match_market_order order_book order = 
  let (table, set_table, update_best) = match order.buy_sell with
    | Buy -> (order_book.asks, (fun ob -> order_book.asks <- ob), (fun () -> order_book.best_ask <- None))
    | Sell -> (order_book.bids, (fun ob -> order_book.bids <- ob), (fun () -> order_book.best_bid <- None))
  in
  let rec execute_order qty_remaining table = 
    if qty_remaining <= 0.0 then table
    else
      match (if order.buy_sell = Buy then PriceMap.min_binding_opt table else PriceMap.max_binding_opt table) with
      | None -> failwith "Not enough liquidity for market order."
      | Some (price, q) -> 
          let process_q qty_remaining =
            if qty_remaining <= 0.0 || Queue.is_empty q then qty_remaining
            else
              let curr_order = Queue.peek q in
              let trade_qty = Float.min curr_order.qty qty_remaining in
              curr_order.qty <- curr_order.qty -. trade_qty;
              if curr_order.qty <= 0.0 then begin
                ignore (Queue.pop q);
                Hashtbl.remove order_book.order_ids curr_order.id;
              end;
              qty_remaining -. trade_qty
          in
          let remaining_qty = process_q qty_remaining in
          let table = if Queue.is_empty q then PriceMap.remove price table else PriceMap.add price q table in
          update_best ();
          execute_order remaining_qty table
  in
  set_table (execute_order order.qty table)

let add_limit_margin_order order_book order =
  let price = get_price_helper order in
  let (table, set_table, update_best) = match order.buy_sell with
    | Buy -> 
        (order_book.bids, (fun ob -> order_book.bids <- ob), (fun p -> 
          match order_book.best_bid with
          | None -> order_book.best_bid <- Some p
          | Some best when p > best -> order_book.best_bid <- Some p
          | _ -> ()))
    | Sell ->
        (order_book.asks, (fun ob -> order_book.asks <- ob), (fun p ->
          match order_book.best_ask with
          | None -> order_book.best_ask <- Some p  
          | Some best when p < best -> order_book.best_ask <- Some p
          | _ -> ()))
  in
  let q = match PriceMap.find_opt price table with
    | None -> Queue.create ()
    | Some q -> q
  in
  Queue.push order q;
  set_table (PriceMap.add price q table);
  update_best price

let add_order (order_book : order_book) (order : Order.order) = 
  if Hashtbl.mem order_book.order_ids order.id then
    failwith "duplicate order ID";
  Hashtbl.add order_book.order_ids order.id order;
  match order.order_type with
  | Market -> match_market_order order_book order
  | Limit _ | Margin _ -> add_limit_margin_order order_book order

let remove_order (order_book : order_book) (order_id : int) = 
  match Hashtbl.find_opt order_book.order_ids order_id with
  | None -> ()
  | Some order -> 
      let price = get_price_helper order in
      let (table, set_table, clear_best) = match order.buy_sell with
        | Buy -> (order_book.bids, (fun ob -> order_book.bids <- ob), (fun () -> order_book.best_bid <- None))
        | Sell -> (order_book.asks, (fun ob -> order_book.asks <- ob), (fun () -> order_book.best_ask <- None))
      in
      match PriceMap.find_opt price table with
      | None -> ()
      | Some q -> 
          let remaining_orders = Queue.create () in
          Queue.iter (fun o -> if o.id <> order_id then Queue.push o remaining_orders) q;
          if Queue.is_empty remaining_orders then begin
            set_table (PriceMap.remove price table);
            clear_best ()
          end else
            set_table (PriceMap.add price remaining_orders table);
          Hashtbl.remove order_book.order_ids order_id

let match_orders (order_book : order_book) (market_conditions : Market_conditions.t) (curr_time : float) = 
  let bids = List.filter (fun order -> not (is_expired order curr_time)) (get_bids order_book) in
  let asks = List.filter (fun order -> not (is_expired order curr_time)) (get_asks order_book) in
  
  let sorted_bids = List.sort (fun a b -> Float.compare (get_price_helper b) (get_price_helper a)) bids in
  let sorted_asks = List.sort (fun a b -> Float.compare (get_price_helper a) (get_price_helper b)) asks in
  
  let rec match_aux sorted_bids sorted_asks acc = 
    match sorted_bids, sorted_asks with
    | [], _ | _, [] -> acc
    | bid :: rest_bids, ask :: rest_asks -> 
        let bid_price = get_price_helper bid in
        let ask_price = get_price_helper ask in
        if bid_price >= ask_price && Market_conditions.check_spread_conditions market_conditions bid_price ask_price then
          let trade_qty = Float.min bid.qty ask.qty in
          let updated_bid = { bid with qty = bid.qty -. trade_qty } in
          let updated_ask = { ask with qty = ask.qty -. trade_qty } in
          let rest_bids = if updated_bid.qty > 0.0 then updated_bid :: rest_bids else rest_bids in
          let rest_asks = if updated_ask.qty > 0.0 then updated_ask :: rest_asks else rest_asks in
          match_aux rest_bids rest_asks ((bid, ask, trade_qty) :: acc)
        else
          acc
  in
  match_aux sorted_bids sorted_asks []

let remove_expired_orders (order_book : order_book) (curr_time : float) = 
  let remove_expired_from_table table =
    PriceMap.filter_map (fun _ q ->
      let remaining_orders = Queue.create () in
      Queue.iter (fun order ->
        if not (is_expired order curr_time) then Queue.push order remaining_orders
        else Hashtbl.remove order_book.order_ids order.id) q;
      if Queue.is_empty remaining_orders then None
      else Some remaining_orders
    ) table
  in
  order_book.bids <- remove_expired_from_table order_book.bids;
  order_book.asks <- remove_expired_from_table order_book.asks;
  order_book.best_bid <- None;
  order_book.best_ask <- None

let check_order_exists (order_book : order_book) (order_id : int) =
  Hashtbl.mem order_book.order_ids order_id