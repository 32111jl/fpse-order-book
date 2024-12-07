open OUnit2
open Database.Db
open Utils.Order_types
open Order_book_lib.Order_book
open Order_book_lib.Matching_engine
open Order_book_lib.Market_conditions

(* let test_setup () =
  let cmd = "PGPASSWORD=123 psql -U ob1 -d order_book -f tests/test_db.sql" in
  let exit_status = Sys.command cmd in
  if exit_status <> 0 then failwith "Failed to run setup script" *)

module DbTests = struct
  let cleanup () =
    ignore (execute_query "DELETE FROM trades" [||]);
    ignore (execute_query "DELETE FROM positions" [||]);
    ignore (execute_query "DELETE FROM orders" [||]);
    ignore (execute_query "DELETE FROM users" [||]);
    ignore (execute_query "DELETE FROM securities" [||])
  
  let test_create_user_in_db _ =
    cleanup ();
    match create_user_in_db 1 "CreateUser" 5000.0 with
    | Ok _ -> 
      (match get_user_balance 1 with
      | Some balance -> assert_equal balance 5000.00
      | None -> assert_failure "Couldn't get user balance")
    | Error e -> Printf.printf "Error creating user: %s\n" e
  
  let test_create_order_in_db _ =
    cleanup ();
    ignore (create_user_in_db 1 "CreateOrderUser" 1000.0);

    let order_type = Limit { price = 150.0; expiration = None } in
    let buy_sell = Buy in
    match create_order 1000 1 "AAPL" order_type buy_sell 1.0 150.0 with
    | Ok _ -> 
      (match get_order 1000 with
      | Ok order -> 
        assert_equal 1 order#ntuples;
        assert_equal "AAPL" (order#getvalue 0 2);
        assert_equal "LIMIT" (order#getvalue 0 3);
        assert_equal "BUY" (order#getvalue 0 4);
        assert_equal 1.00 (float_of_string (order#getvalue 0 5));
        assert_equal 150.00 (float_of_string (order#getvalue 0 6));
        assert_equal "ACTIVE" (order#getvalue 0 7)
      | Error e -> Printf.printf "Error getting order: %s\n" e)
    | Error e -> Printf.printf "Error creating order: %s\n" e
  
  let test_update_user_balance _ =
    cleanup ();
    ignore (create_user_in_db 2 "UpdateBalanceUser" 1000.0);
    ignore (update_user_balance 2 500.0);
    match get_user_balance 2 with
    | Some balance -> assert_equal 1500.0 balance
    | None -> assert_failure "User not found"

  let test_get_orders_by_user _ =
    cleanup ();
    ignore (create_user_in_db 3 "GetOrdersUser" 1000.0);
    
    let order_type = Limit { price = 150.0; expiration = None } in
    ignore (create_order 1000 3 "AAPL" order_type Buy 1.0 150.0);
    ignore (create_order 1001 3 "GOOGL" order_type Buy 2.0 200.0);
    
    match get_orders_by_user 3 with
    | Ok result -> 
      assert_equal 2 result#ntuples;
      assert_equal "AAPL" (result#getvalue 0 2);
      assert_equal "GOOGL" (result#getvalue 1 2)
    | Error e -> assert_failure ("Failed to get orders: " ^ e)
  
  let test_update_order_status _ =
    cleanup ();
    ignore (create_user_in_db 4 "UpdateOrderStatusUser" 1000.0);
    
    let order_type = Limit { price = 150.0; expiration = None } in
    ignore (create_order 1000 4 "AAPL" order_type Buy 1.0 150.0);
    
    match update_order_status 1000 "FILLED" with
    | Ok _ -> 
      (match get_order 1000 with
      | Ok result -> 
        assert_equal "FILLED" (result#getvalue 0 7)
      | Error e -> assert_failure ("Failed to get order: " ^ e))
    | Error e -> assert_failure ("Failed to update status: " ^ e)
  
  let test_update_position _ =
    cleanup ();
    ignore (create_user_in_db 5 "UpdatePositionUser" 1000.0);
    
    match update_position 5 "AAPL" 10.0 with
    | Ok _ -> 
      (match update_position 5 "AAPL" 5.0 with
      | Ok _ ->
        (match get_positions_by_user 5 with
        | Ok result -> 
          assert_equal 1 result#ntuples;
          Printf.printf "Position: %s\n" (result#getvalue 0 2);
          assert_equal "15.0" (result#getvalue 0 2)
        | Error e -> assert_failure ("Failed to get positions: " ^ e))
      | Error e -> assert_failure ("Failed to update position: " ^ e))
    | Error e -> assert_failure ("Failed to create position: " ^ e)
  
  let test_record_and_get_trade _ =
    cleanup ();
    ignore (create_user_in_db 6 "TradeUser1" 1000.0);
    ignore (create_user_in_db 7 "TradeUser2" 1000.0);

    ignore (create_order 1000 6 "AAPL" (Limit { price = 150.0; expiration = None }) Buy 1.0 150.0);
    ignore (create_order 1001 7 "AAPL" (Limit { price = 150.0; expiration = None }) Sell 1.0 150.0);
    ignore (record_trade ~buy_order_id:1000 ~sell_order_id:1001 ~security:"AAPL" ~qty:1.0 ~price:150.0);
    match get_trade_history 6 with
    | Ok result ->
      assert_equal 1 result#ntuples;
      assert_equal "AAPL" (result#getvalue 0 3);
      assert_equal "1.0" (result#getvalue 0 4)
    | Error e -> assert_failure ("Failed to get trade history: " ^ e)

  let test_create_get_security _ =
    cleanup ();
    ignore (create_security "TEST1" 123.45);
    match get_security_info "TEST1" with
    | Ok result ->
      assert_equal 1 result#ntuples;
      assert_equal "TEST1" (result#getvalue 0 1);
      assert_equal "123.45" (result#getvalue 0 2)
    | Error e -> assert_failure ("Failed to get security info: " ^ e)

  let test_update_security_status _ =
    cleanup ();
    ignore (create_security "TEST2" 200.0);
    ignore (update_security_status "TEST2" "FILLED");
    match get_security_info "TEST2" with
    | Ok result ->
      assert_equal 1 result#ntuples;
      assert_equal "FILLED" (result#getvalue 0 3)
    | Error e -> assert_failure ("Failed to update security status: " ^ e)

  let test_get_all_securities _ =
    cleanup ();
    ignore (create_security "SEC1" 100.0);
    ignore (create_security "SEC2" 200.0);
    match get_all_securities () with
    | Ok result ->
      assert_equal 2 result#ntuples;
      assert_equal "SEC1" (result#getvalue 0 1);
      assert_equal "SEC2" (result#getvalue 1 1)
    | Error e -> assert_failure ("Failed to get all securities: " ^ e)

  let test_get_security_price _ =
    cleanup ();
    ignore (create_security "SEC2" 300.5);
    match get_security_price "SEC2" with
    | Ok result ->
      assert_equal 1 result#ntuples;
      assert_equal "300.5" (result#getvalue 0 0)
    | Error e -> assert_failure ("Failed to get security price: " ^ e)

  let test_get_trades_by_security _ =
    cleanup ();
    ignore (create_user_in_db 8 "GetTradesSecUser1" 1000.0);
    ignore (create_user_in_db 9 "GetTradesSecUser2" 1000.0);
    ignore (create_order 3000 8 "TSLA" (Limit {price=700.0; expiration=None}) Buy 2.0 700.0);
    ignore (create_order 3001 9 "TSLA" (Limit {price=700.0; expiration=None}) Sell 2.0 700.0);
    ignore (record_trade ~buy_order_id:3000 ~sell_order_id:3001 ~security:"TSLA" ~qty:2.0 ~price:700.0);
    match get_trades_by_security "TSLA" with
    | Ok result ->
      assert_equal 1 result#ntuples;
      assert_equal "TSLA" (result#getvalue 0 3);
      assert_equal "2.0" (result#getvalue 0 4)
    | Error e -> assert_failure ("Failed to get trades by security: " ^ e)

  let series = "db_tests" >::: [
    "test_create_user_in_db" >:: test_create_user_in_db;
    "test_create_order_in_db" >:: test_create_order_in_db;
    "test_update_user_balance" >:: test_update_user_balance;
    "test_get_orders_by_user" >:: test_get_orders_by_user;
    "test_update_order_status" >:: test_update_order_status;
    "test_update_position" >:: test_update_position;
    "test_record_and_get_trade" >:: test_record_and_get_trade;
    "test_create_get_security" >:: test_create_get_security;
    "test_update_security_status" >:: test_update_security_status;
    "test_get_all_securities" >:: test_get_all_securities;
    "test_get_security_price" >:: test_get_security_price;
    "test_get_trades_by_security" >:: test_get_trades_by_security;
  ]
end

module OrderBookTests = struct
  let cleanup () =
    ignore (execute_query "DELETE FROM trades" [||]);
    ignore (execute_query "DELETE FROM positions" [||]);
    ignore (execute_query "DELETE FROM orders" [||]);
    ignore (execute_query "DELETE FROM users" [||]);
    ignore (execute_query "DELETE FROM securities" [||])

  let test_create_order_book _ =
    cleanup ();
    let book = create_order_book "AAPL" in
    assert_equal "AAPL" (get_security book);
    let bids = get_bids book in
    let asks = get_asks book in
    assert_equal 0 (List.length bids);
    assert_equal 0 (List.length asks)

  let test_get_best_bid_ask_empty _ =
    cleanup ();
    let book = create_order_book "AAPL" in
    assert_equal None (get_best_bid book);
    assert_equal None (get_best_ask book)

  let test_add_limit_orders_and_check_best_bid_ask _ =
    cleanup ();
    ignore (create_user_in_db 1 "User1" 1000.0);
    ignore (create_user_in_db 2 "User2" 1000.0);

    let book = create_order_book "AAPL" in
    ignore (create_order 1000 1 "AAPL" (Limit { price = 150.0; expiration = None }) Buy 10.0 150.0);
    ignore (create_order 1001 2 "AAPL" (Limit { price = 155.0; expiration = None }) Sell 5.0 155.0);

    assert_equal (Some 150.0) (get_best_bid book);
    assert_equal (Some 155.0) (get_best_ask book)

  let test_add_multiple_sell_orders_check_ask _ =
    cleanup ();
    ignore (create_user_in_db 3 "User3" 1000.0);
    ignore (create_user_in_db 4 "User4" 1000.0);

    let book = create_order_book "AAPL" in
    ignore (create_order 1002 3 "AAPL" (Limit { price = 160.0; expiration = None }) Sell 5.0 160.0);
    ignore (create_order 1003 4 "AAPL" (Limit { price = 155.0; expiration = None }) Sell 5.0 155.0);
    assert_equal (Some 155.0) (get_best_ask book)

  let test_get_bids_and_asks_ordering _ =
    cleanup ();
    ignore (create_user_in_db 5 "User5" 1000.0);
    ignore (create_user_in_db 6 "User6" 1000.0);
    ignore (create_user_in_db 7 "User7" 1000.0);

    let book = create_order_book "AAPL" in
    ignore (create_order 1100 5 "AAPL" (Limit { price = 150.0; expiration = None }) Buy 10.0 150.0);
    ignore (create_order 1101 6 "AAPL" (Limit { price = 155.0; expiration = None }) Buy 5.0 155.0);
    ignore (create_order 1102 7 "AAPL" (Limit { price = 149.0; expiration = None }) Buy 2.0 149.0);

    let bids = get_bids book in
    assert_equal 3 (List.length bids);
    assert_equal 155.0 (match get_price (List.hd bids) with Some p -> p | None -> 0.0);
    assert_equal 150.0 (match get_price (List.nth bids 1) with Some p -> p | None -> 0.0);
    assert_equal 149.0 (match get_price (List.nth bids 2) with Some p -> p | None -> 0.0)

  let test_add_and_remove_order _ =
    cleanup ();
    ignore (create_user_in_db 8 "User8" 1000.0);
    let book = create_order_book "AAPL" in
    ignore (create_order 1200 8 "AAPL" (Limit { price = 150.0; expiration = None }) Buy 10.0 150.0);
    assert_equal true (check_order_exists book 1200);

    ignore (remove_order book 1200);
    assert_equal false (check_order_exists book 1200)

  let test_remove_expired_orders _ =
    cleanup ();
    ignore (create_user_in_db 9 "User9" 1000.0);
    let book = create_order_book "AAPL" in
    ignore (execute_query "
      INSERT INTO orders (id, user_id, security, order_type, buy_sell, quantity, price, status, expiration_time)
      VALUES (1300, 9, 'AAPL', 'LIMIT', 'BUY', 10.0, 100.0, 'ACTIVE', $1)" [| string_of_float 0.0 |]);

    ignore (create_order 1301 9 "AAPL" (Limit { price = 105.0; expiration = None }) Buy 5.0 105.0);

    ignore (remove_expired_orders book 1.0);
    let bids = get_bids book in
    assert_equal 1 (List.length bids);
    assert_equal 105.0 (match get_price (List.hd bids) with Some p -> p | None -> 0.0)

  let series = "order_book_tests" >::: [
    "test_create_order_book" >:: test_create_order_book;
    "test_get_best_bid_ask_empty" >:: test_get_best_bid_ask_empty;
    "test_add_limit_orders_and_check_best_bid_ask" >:: test_add_limit_orders_and_check_best_bid_ask;
    "test_add_multiple_sell_orders_check_ask" >:: test_add_multiple_sell_orders_check_ask;
    "test_get_bids_and_asks_ordering" >:: test_get_bids_and_asks_ordering;
    "test_add_and_remove_order" >:: test_add_and_remove_order;
    "test_remove_expired_orders" >:: test_remove_expired_orders
  ]
end

module MatchingEngineTests = struct
  let cleanup () =
    ignore (execute_query "DELETE FROM trades" [||]);
    ignore (execute_query "DELETE FROM positions" [||]);
    ignore (execute_query "DELETE FROM orders" [||]);
    ignore (execute_query "DELETE FROM users" [||]);
    ignore (execute_query "DELETE FROM securities" [||])

  let create_market_conds ba_spread margin_rate = 
    create_market_conditions ba_spread margin_rate

  let test_get_margin_rate _ =
    cleanup ();
    let market_conds = create_market_conds 3.0 0.5 in
    assert_equal 0.5 (get_margin_rate market_conds);
    let market_conds2 = create_market_conds 2.0 0.75 in
    assert_equal 0.75 (get_margin_rate market_conds2)

  let test_check_spread _ = 
    cleanup ();
    ignore (create_user_in_db 1 "SpreadUser1" 1000.0);
    ignore (create_user_in_db 2 "SpreadUser2" 1000.0);
    
    let book = create_order_book "AAPL" in
    ignore (create_order 1000 1 "AAPL" (Limit { price = 100.0; expiration = None }) Buy 10.0 100.0);
    ignore (create_order 1001 2 "AAPL" (Limit { price = 102.0; expiration = None }) Sell 5.0 102.0);
    
    let market_conds = create_market_conds 3.0 0.5 in
    assert_equal true (check_spread book market_conds);
    let tighter_market_conds = create_market_conds 0.5 0.5 in
    assert_equal false (check_spread book tighter_market_conds)

  let test_check_spread_with_market_orders _ =
    cleanup ();
    ignore (create_user_in_db 13 "MarketUser1" 1000.0);
    ignore (create_user_in_db 14 "MarketUser2" 1000.0);
    
    let book = create_order_book "AAPL" in
    ignore (create_order 7000 13 "AAPL" Market Buy 10.0 0.0);
    ignore (create_order 7001 14 "AAPL" (Limit { price = 105.0; expiration = None }) Sell 5.0 105.0);
    
    let market_conds = create_market_conds 2.0 0.5 in
    assert_equal true (check_spread book market_conds);
    
    cleanup ();
    ignore (create_order 7002 13 "AAPL" (Limit { price = 100.0; expiration = None }) Buy 10.0 100.0);
    ignore (create_order 7003 14 "AAPL" Market Sell 5.0 0.0);
    
    assert_equal true (check_spread book market_conds)

  let test_get_trade_price _ =
    cleanup ();
    ignore (create_user_in_db 15 "PriceUser1" 1000.0);
    ignore (create_user_in_db 16 "PriceUser2" 1000.0);
    
    let buy_order = { 
      id=8000; user_id=15; security="AAPL"; 
      order_type=Limit{price=100.0; expiration=None}; buy_sell=Buy; qty=10.0 
    } in
    let sell_order = { 
      id=8001; user_id=16; security="AAPL"; 
      order_type=Market; buy_sell=Sell; qty=5.0 
    } in
    
    assert_equal 100.0 (get_trade_price buy_order sell_order);
    
    let market_buy = { buy_order with order_type = Market } in
    try
      ignore (get_trade_price market_buy sell_order);
      assert_failure "Expected failure with both market orders"
    with Failure msg ->
      assert_equal "Both orders cannot be market orders" msg

  let test_match_orders_with_margin _ =
    cleanup ();
    ignore (create_user_in_db 17 "MarginUser1" 1000.0);
    ignore (create_user_in_db 18 "MarginUser2" 1000.0);
    
    let book = create_order_book "AAPL" in
    
    ignore (create_order 9000 17 "AAPL" (Margin 100.0) Buy 10.0 100.0);
    ignore (create_order 9001 18 "AAPL" (Limit { price = 98.0; expiration = None }) Sell 5.0 98.0);
    
    let market_conds = create_market_conds 5.0 0.5 in
    let trades1 = match_orders book market_conds in
    assert_equal 1 (List.length trades1);
    
    cleanup ();
    ignore (create_order 9002 17 "AAPL" (Limit { price = 100.0; expiration = None }) Buy 10.0 100.0);
    ignore (create_order 9003 18 "AAPL" (Margin 98.0) Sell 5.0 98.0);
    
    let trades2 = match_orders book market_conds in
    assert_equal 1 (List.length trades2);
    
    cleanup ();
    ignore (create_order 9004 17 "AAPL" (Margin 100.0) Buy 10.0 100.0);
    ignore (create_order 9005 18 "AAPL" (Margin 98.0) Sell 5.0 98.0);
    
    let trades3 = match_orders book market_conds in
    assert_equal 1 (List.length trades3)

  let test_execute_trade _ =
    cleanup ();
    ignore (create_user_in_db 3 "ExecTradeBuyUser" 2000.0);
    ignore (create_user_in_db 4 "ExecTradeSellUser" 2000.0);

    ignore (create_order 2000 3 "AAPL" (Limit { price = 100.0; expiration = None }) Buy 10.0 100.0);
    ignore (create_order 2001 4 "AAPL" (Limit { price = 100.0; expiration = None }) Sell 5.0 100.0);

    let buy_order = { id=2000; user_id=3; security="AAPL"; order_type=Limit{price=100.0; expiration=None}; buy_sell=Buy; qty=10.0 } in
    let sell_order = { id=2001; user_id=4; security="AAPL"; order_type=Limit{price=100.0; expiration=None}; buy_sell=Sell; qty=5.0 } in
    
    match execute_trade buy_order sell_order with
    | Ok (trade_qty, trade_price) ->
      assert_equal 5.0 trade_qty;
      assert_equal 100.0 trade_price;
      (match get_order 2000 with
      | Ok res ->
        let new_qty_buy = float_of_string (res#getvalue 0 5) in
        assert_equal 5.0 new_qty_buy
      | Error e -> assert_failure e);
      (match get_order 2001 with
      | Ok res ->
        let new_qty_sell = float_of_string (res#getvalue 0 5) in
        assert_equal 0.0 new_qty_sell
      | Error e -> assert_failure e)
    | Error e -> assert_failure e

  let test_match_orders _ = 
    cleanup ();
    ignore (create_user_in_db 5 "MatchUser1" 1000.0);
    ignore (create_user_in_db 6 "MatchUser2" 1000.0);
    
    let book = create_order_book "AAPL" in
    ignore (create_order 3000 5 "AAPL" (Limit { price = 100.0; expiration = None }) Buy 10.0 100.0);
    ignore (create_order 3001 6 "AAPL" (Limit { price = 102.0; expiration = None }) Sell 5.0 102.0);
    
    let market_conds = create_market_conds 5.0 0.5 in
    let trades = match_orders book market_conds in
    assert_equal 1 (List.length trades);

    let trade = List.hd trades in
    assert_equal 5.0 trade.qty;
    assert_equal 102.0 trade.price

  let test_match_multiple_books _ =
    cleanup ();
    ignore (create_user_in_db 7 "MultiBookUser1" 1000.0);
    ignore (create_user_in_db 8 "MultiBookUser2" 1000.0);
    ignore (create_user_in_db 9 "MultiBookUser3" 2000.0);
    ignore (create_user_in_db 10 "MultiBookUser4" 2000.0);

    let book1 = create_order_book "AAPL" in
    let book2 = create_order_book "TSLA" in
    
    ignore (create_order 4000 7 "AAPL" (Limit { price = 100.0; expiration = None }) Buy 10.0 100.0);
    ignore (create_order 4001 8 "AAPL" (Limit { price = 102.0; expiration = None }) Sell 5.0 102.0);
    ignore (create_order 5000 9 "TSLA" (Limit { price = 200.0; expiration = None }) Buy 15.0 200.0);
    ignore (create_order 5001 10 "TSLA" (Limit { price = 198.0; expiration = None }) Sell 10.0 198.0);

    let market_conds = create_market_conds 10.0 0.5 in
    let trades = match_all_books [book1; book2] market_conds in
    assert_equal 2 (List.length trades)

  let test_no_matching_orders _ =
    cleanup ();
    ignore (create_user_in_db 11 "NoMatchUser1" 1000.0);
    ignore (create_user_in_db 12 "NoMatchUser2" 1000.0);
    
    let book = create_order_book "AAPL" in
    ignore (create_order 6000 11 "AAPL" (Limit { price = 100.0; expiration = None }) Buy 10.0 100.0);
    ignore (create_order 6001 12 "AAPL" (Limit { price = 105.0; expiration = None }) Sell 5.0 105.0);
    
    let market_conds = create_market_conds 2.0 0.5 in
    let trades = match_orders book market_conds in
    assert_equal 0 (List.length trades)

  let series = "matching_engine_tests" >::: [
    "test_get_margin_rate" >:: test_get_margin_rate;
    "test_check_spread" >:: test_check_spread;
    "test_check_spread_with_market_orders" >:: test_check_spread_with_market_orders;
    "test_get_trade_price" >:: test_get_trade_price;
    "test_match_orders_with_margin" >:: test_match_orders_with_margin;
    "test_execute_trade" >:: test_execute_trade;
    "test_match_orders" >:: test_match_orders;
    "test_match_multiple_books" >:: test_match_multiple_books;
    "test_no_matching_orders" >:: test_no_matching_orders;
  ]
end


let test_suite = "Tests" >::: [
  DbTests.series;
  OrderBookTests.series;
  MatchingEngineTests.series
]

let () =
  (* test_setup (); *)
  run_test_tt_main test_suite
