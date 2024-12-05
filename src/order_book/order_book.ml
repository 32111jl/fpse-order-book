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

let get_qty_at_price (order_book : order_book) (price : float) =
  match PriceMap.find_opt price order_book.bids with
  | Some q -> Queue.fold (fun acc order -> acc +. order.qty) 0.0 q
  | None -> 0.0

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