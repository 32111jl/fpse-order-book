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
  let cleanup _ =
    Unix.sleepf 2.0; (* sleep so that tests can finish before we actually cleanup *)
    ignore (execute_query "TRUNCATE TABLE trades, positions, orders, users CASCADE;" [||]);
    Unix.sleepf 2.0
  
  let test_create_user_in_db _ =
    match create_user_in_db 1 "CreateUser" 5000.0 with
    | Ok _ -> 
      (match get_user_balance 1 with
      | Some balance -> assert_equal balance 5000.00
      | None -> assert_failure "Couldn't get user balance")
    | Error e -> Printf.printf "Error creating user: %s\n" e
  
  let test_create_order_in_db _ =
    ignore (create_user_in_db 2 "CreateOrderUser" 1000.0);

    let order_type = Limit { price = 150.0; expiration = None } in
    let buy_sell = Buy in
    match create_order 2000 2 "AAPL" order_type buy_sell 1.0 150.0 with
    | Ok _ -> 
      (match get_order 2000 with
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
    ignore (create_user_in_db 3 "UpdateBalanceUser" 1000.0);
    ignore (update_user_balance 3 500.0);
    match get_user_balance 3 with
    | Some balance -> assert_equal 1500.0 balance
    | None -> assert_failure "User not found"

  let test_get_orders_by_user _ =
    ignore (create_user_in_db 4 "GetOrdersUser" 1000.0);
    
    let order_type = Limit { price = 150.0; expiration = None } in
    ignore (create_order 4000 4 "AAPL" order_type Buy 1.0 150.0);
    ignore (create_order 4001 4 "GOOGL" order_type Buy 2.0 200.0);
    
    match get_orders_by_user 4 with
    | Ok result -> 
      assert_equal 2 result#ntuples;
      assert_equal "AAPL" (result#getvalue 0 2);
      assert_equal "GOOGL" (result#getvalue 1 2)
    | Error e -> assert_failure ("Failed to get orders: " ^ e)
  
  let test_update_order_status _ =
    ignore (create_user_in_db 5 "UpdateOrderStatusUser" 1000.0);
    
    let order_type = Limit { price = 150.0; expiration = None } in
    ignore (create_order 5000 5 "AAPL" order_type Buy 1.0 150.0);
    
    match update_order_status 5000 "FILLED" with
    | Ok _ -> 
      (match get_order 5000 with
      | Ok result -> 
        assert_equal "FILLED" (result#getvalue 0 7)
      | Error e -> assert_failure ("Failed to get order: " ^ e))
    | Error e -> assert_failure ("Failed to update status: " ^ e)
  
  let test_update_position _ =
    ignore (create_user_in_db 6 "UpdatePositionUser" 1000.0);
    ignore (update_position 6 "AAPL" 10.0);
    ignore (update_position 6 "AAPL" 5.0);
    match get_positions_by_user 6 with
    | Ok result -> 
      assert_equal 1 result#ntuples; (* why 0? *)
      assert_equal 15.0 (float_of_string (result#getvalue 0 2))
    | Error e -> assert_failure ("Failed to create position: " ^ e)
  
  let test_record_and_get_trade _ =
    ignore (create_user_in_db 7 "TradeUser1" 1000.0);
    ignore (create_user_in_db 8 "TradeUser2" 1000.0);

    ignore (create_order 7000 7 "AAPL" (Limit { price = 150.0; expiration = None }) Buy 1.0 150.0);
    ignore (create_order 8000 8 "AAPL" (Limit { price = 150.0; expiration = None }) Sell 1.0 150.0);
    ignore (record_trade ~buy_order_id:7000 ~sell_order_id:8000 ~security:"AAPL" ~qty:1.0 ~price:150.0);
    match get_trade_history 7 with
    | Ok result ->
      assert_equal 1 result#ntuples;
      assert_equal "AAPL" (result#getvalue 0 3);
      assert_equal 1.0 (float_of_string (result#getvalue 0 4))
    | Error e -> assert_failure ("Failed to get trade history: " ^ e)

  let test_create_get_security _ =
    ignore (create_security "TEST1" 123.45);
    match get_security_info "TEST1" with
    | Ok result ->
      assert_equal 1 result#ntuples;
      assert_equal "TEST1" (result#getvalue 0 0);
      assert_equal "123.45" (result#getvalue 0 1)
    | Error e -> assert_failure ("Failed to get security info: " ^ e)

  let test_update_security_status _ =
    ignore (create_security "TEST2" 200.0);
    ignore (update_security_status "TEST2" "FILLED");
    match get_security_info "TEST2" with
    | Ok result ->
      assert_equal 1 result#ntuples;
      assert_equal "FILLED" (result#getvalue 0 2)
    | Error e -> assert_failure ("Failed to update security status: " ^ e)

  let test_get_security_price _ =
    ignore (create_security "SEC3" 300.5);
    match get_security_price "SEC3" with
    | Ok result ->
      assert_equal 1 result#ntuples;
      assert_equal 300.5 (float_of_string (result#getvalue 0 0))
    | Error e -> assert_failure ("Failed to get security price: " ^ e)

  let test_get_trades_by_security _ =
    ignore (create_user_in_db 9 "GetTradesSecUser1" 1000.0);
    ignore (create_user_in_db 10 "GetTradesSecUser2" 1000.0);
    ignore (create_order 9000 9 "TSLA" (Limit { price = 700.0; expiration = None }) Buy 1.0 700.0);
    ignore (create_order 10000 10 "TSLA" (Limit { price = 700.0; expiration = None }) Sell 1.0 700.0);
    ignore (record_trade ~buy_order_id:9000 ~sell_order_id:10000 ~security:"TSLA" ~qty:1.0 ~price:700.0);
    match get_trades_by_security "TSLA" with
    | Ok result ->
      assert_equal 1 result#ntuples;
      assert_equal "TSLA" (result#getvalue 0 3);
      assert_equal 1.0 (float_of_string (result#getvalue 0 4))
    | Error e -> assert_failure ("Failed to get trades by security: " ^ e)

  let test_get_all_securities _ =
    ignore (create_security "SEC1" 100.0);
    ignore (create_security "SEC2" 200.0);
    match get_all_securities () with
    | Ok result ->
      assert_equal 15 result#ntuples; (* 15 because we had 10 initially, added "TEST1/TEST2", added "SEC1/SEC2" and after this test there's 1 more *)
      (* commenting these out because order of execution for tests is not always the same *)
      (* assert_equal "SEC1" (result#getvalue 13 0);
      assert_equal "SEC2" (result#getvalue 14 0) *)
    | Error e -> assert_failure ("Failed to get all securities: " ^ e)

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
    "test_get_security_price" >:: test_get_security_price;
    "test_get_trades_by_security" >:: test_get_trades_by_security;
    "test_get_all_securities" >:: test_get_all_securities;
    "cleanup" >:: cleanup
  ]
end

module OrderBookTests = struct
  let cleanup _ =
    Unix.sleepf 2.0;
    ignore (execute_query "TRUNCATE TABLE trades, positions, orders, users, securities CASCADE;" [||])

  let test_create_order_book _ =
    ignore (create_security "OB1" 100.0);
    let book = create_order_book "OB1" in
    assert_equal "OB1" (get_security book);
    let bids = get_bids book in
    let asks = get_asks book in
    assert_equal 0 (List.length bids);
    assert_equal 0 (List.length asks)

  let test_get_best_bid_ask_empty _ =
    ignore (create_security "OB2" 100.0);
    let book = create_order_book "OB2" in
    assert_equal None (get_best_bid book);
    assert_equal None (get_best_ask book)

  let test_add_limit_orders_and_check_best_bid_ask _ =
    ignore (create_security "OB3" 100.0);
    let book = create_order_book "OB3" in
    ignore (create_user_in_db 11 "UserOB1" 1000.0);
    ignore (create_user_in_db 12 "UserOB2" 1000.0);

    ignore (create_order 1101 11 "OB3" (Limit { price = 101.0; expiration = None }) Buy 2.0 101.0);
    ignore (create_order 1102 12 "OB3" (Limit { price = 102.0; expiration = None }) Sell 1.0 102.0);
    assert_equal (Some 101.0) (get_best_bid book);
    assert_equal (Some 102.0) (get_best_ask book)

  let test_add_multiple_buy_orders_check_bid _ =
    ignore (create_security "OB4" 100.0);
    let book = create_order_book "OB4" in
    ignore (create_user_in_db 13 "UserOB3" 1000.0);
    ignore (create_user_in_db 14 "UserOB4" 1000.0);

    ignore (create_order 1103 13 "OB4" (Limit { price = 101.0; expiration = None }) Buy 2.0 102.0);
    ignore (create_order 1104 14 "OB4" (Limit { price = 102.0; expiration = None }) Buy 1.0 101.0);
    assert_equal (Some 102.0) (get_best_bid book) (* highest price = best bid *)

  let test_add_multiple_sell_orders_check_ask _ =
    ignore (create_security "OB5" 100.0);
    let book = create_order_book "OB5" in
    ignore (create_user_in_db 15 "UserOB5" 1000.0);
    ignore (create_user_in_db 16 "UserOB6" 1000.0);

    ignore (create_order 1105 15 "OB5" (Limit { price = 101.0; expiration = None }) Sell 1.0 101.0);
    ignore (create_order 1106 16 "OB5" (Limit { price = 102.0; expiration = None }) Sell 2.0 102.0);
    assert_equal (Some 101.0) (get_best_ask book) (* lowest price = best ask *)

  let test_get_bids_and_asks_ordering _ =
    ignore (create_security "OB6" 100.0);
    let book = create_order_book "OB6" in
    ignore (create_user_in_db 17 "UserOB7" 1000.0);
    ignore (create_user_in_db 18 "UserOB8" 1000.0);
    ignore (create_user_in_db 19 "UserOB9" 1000.0);

    ignore (create_order 1107 17 "OB6" (Limit { price = 100.0; expiration = None }) Buy 1.0 100.0);
    ignore (create_order 1108 18 "OB6" (Limit { price = 105.0; expiration = None }) Buy 1.0 105.0);
    ignore (create_order 1109 19 "OB6" (Limit { price = 101.0; expiration = None }) Buy 1.0 101.0);

    let bids = get_bids book in
    assert_equal 3 (List.length bids);
    assert_equal 105.0 (match get_price (List.hd bids) with Some p -> p | None -> 0.0);
    assert_equal 101.0 (match get_price (List.nth bids 1) with Some p -> p | None -> 0.0);
    assert_equal 100.0 (match get_price (List.nth bids 2) with Some p -> p | None -> 0.0)

  let test_add_and_remove_order _ =
    ignore (create_security "OB7" 100.0);
    ignore (create_user_in_db 20 "UserOB10" 1000.0);
    let book = create_order_book "OB7" in
    ignore (create_order 1110 20 "OB7" (Limit { price = 100.0; expiration = None }) Buy 1.0 100.0);
    assert_equal true (check_order_exists book 1110);

    ignore (remove_order book 1110); (* order status should be CANCELLED *)
    assert_equal false (check_order_exists book 1110)

  let test_remove_expired_orders _ =
    ignore (create_security "OB8" 100.0);
    ignore (create_user_in_db 21 "UserOB11" 1000.0);
    let book = create_order_book "OB8" in
    ignore (create_order 1111 21 "OB8" (Limit { price = 105.0; expiration = Some 2.0 }) Buy 5.0 105.0);

    ignore (remove_expired_orders book 5.0); (* should be expired since 5 > 2 *)
    match get_order 1111 with
    | Ok result ->
      assert_equal 1 result#ntuples;
      assert_equal "EXPIRED" (result#getvalue 0 7);
      let bids = get_bids book in
      assert_equal 0 (List.length bids) (* 0 since expired orders aren't active *)
    | Error e -> assert_failure ("Failed to get order: " ^ e)

  let test_get_best_bid_ask_cached _ =
    ignore (create_security "OB9" 100.0);
    let book = create_order_book "OB9" in
    ignore (create_user_in_db 22 "UserOB12" 1000.0);
    let buy_order = { id = 1112; user_id = 22; security = "OB9"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Buy; qty = 1.0 } in
    let sell_order = { id = 1113; user_id = 22; security = "OB9"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Sell; qty = 1.0 } in
    ignore (add_order book buy_order);
    ignore (add_order book sell_order);
    assert_equal (Some 100.0) (get_best_bid book);
    assert_equal (Some 100.0) (get_best_ask book);

    let better_buy_order = { id = 1114; user_id = 22; security = "OB9"; order_type = Limit { price = 101.0; expiration = None }; buy_sell = Buy; qty = 1.0 } in
    let better_sell_order = { id = 1115; user_id = 22; security = "OB9"; order_type = Limit { price = 99.0; expiration = None }; buy_sell = Sell; qty = 1.0 } in
    ignore (add_order book better_buy_order);
    ignore (add_order book better_sell_order);
    assert_equal (Some 101.0) (get_best_bid book);
    assert_equal (Some 99.0) (get_best_ask book)

  let series = "order_book_tests" >::: [
    "test_create_order_book" >:: test_create_order_book;
    "test_get_best_bid_ask_empty" >:: test_get_best_bid_ask_empty;
    "test_add_limit_orders_and_check_best_bid_ask" >:: test_add_limit_orders_and_check_best_bid_ask;
    "test_add_multiple_buy_orders_check_bid" >:: test_add_multiple_buy_orders_check_bid;
    "test_add_multiple_sell_orders_check_ask" >:: test_add_multiple_sell_orders_check_ask;
    "test_get_bids_and_asks_ordering" >:: test_get_bids_and_asks_ordering;
    "test_add_and_remove_order" >:: test_add_and_remove_order;
    "test_remove_expired_orders" >:: test_remove_expired_orders;
    "test_get_best_bid_ask_cached" >:: test_get_best_bid_ask_cached;
    "cleanup" >:: cleanup
  ]
end

module MatchingEngineTests = struct
  let cleanup _ =
    Unix.sleepf 2.0;
    ignore (execute_query "TRUNCATE TABLE trades, positions, orders, users, securities CASCADE;" [||])

  let test_get_margin_rate _ =
    let market_conds = create_market_conditions 3.0 0.5 in
    assert_equal 0.5 (get_margin_rate market_conds);
    let market_conds2 = create_market_conditions 2.0 0.75 in
    assert_equal 0.75 (get_margin_rate market_conds2)

  let test_check_spread _ = 
    ignore (create_security "ME1" 100.0);
    let book = create_order_book "ME1" in
    ignore (create_user_in_db 31 "UserME1" 1000.0);
    ignore (create_user_in_db 32 "UserME2" 1000.0);
    
    let order1 = { id = 1301; user_id = 31; security = "ME1"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Buy; qty = 1.0 } in
    let order2 = { id = 1302; user_id = 32; security = "ME1"; order_type = Limit { price = 102.0; expiration = None }; buy_sell = Sell; qty = 1.0 } in
    ignore (add_order book order1);
    ignore (add_order book order2);

    (* book should be updated with best bid/ask *)
    let market_conds = create_market_conditions 2.0 0.5 in
    assert_equal true (check_spread book market_conds);
    let tighter_market_conds = create_market_conditions 0.5 0.5 in
    assert_equal false (check_spread book tighter_market_conds)


  let test_check_spread_with_market_orders _ =
    ignore (create_security "ME3" 100.0);
    let book = create_order_book "ME3" in
    ignore (create_user_in_db 33 "UserME3" 1000.0);
    ignore (create_user_in_db 34 "UserME4" 1000.0);
    
    let order3 = { id = 1303; user_id = 33; security = "ME3"; order_type = Limit { price = 105.0; expiration = None }; buy_sell = Sell; qty = 1.0 } in
    let order4 = { id = 1304; user_id = 34; security = "ME3"; order_type = Market; buy_sell = Buy; qty = 1.0 } in
    ignore (add_order book order3);
    ignore (add_order book order4);

    let market_conds = create_market_conditions 2.0 0.5 in
    assert_equal true (check_spread book market_conds);
    let market_conds2 = create_market_conditions 0.5 0.5 in
    assert_equal true (check_spread book market_conds2) (* this shouldn't change outcome since it's a market order *)

  let test_get_trade_price _ =
    ignore (create_security "ME5" 100.0);
    ignore (create_user_in_db 35 "UserME5" 1000.0);
    ignore (create_user_in_db 36 "UserME6" 1000.0);
    
    let buy_order = { id = 1305; user_id = 35; security = "ME5"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Buy; qty = 10.0 } in
    let sell_order = { id = 1306; user_id = 36; security = "ME5"; order_type = Market; buy_sell = Sell; qty = 5.0 } in
    
    assert_equal 100.0 (get_trade_price buy_order sell_order);
    
    let market_buy = { buy_order with order_type = Market } in
    try
      ignore (get_trade_price market_buy sell_order);
      assert_failure "Expected failure with both market orders"
    with Failure msg ->
      assert_equal "Both orders cannot be market orders" msg

  let test_match_orders_with_margin _ =
    (* margin buy, limit sell *)
    ignore (create_security "ME7" 100.0);
    let book1 = create_order_book "ME7" in
    ignore (create_user_in_db 37 "UserME7" 1000.0);
    ignore (create_user_in_db 38 "UserME8" 1000.0);
    let margin_order1 = { id = 1307; user_id = 37; security = "ME7"; order_type = Margin 100.0; buy_sell = Buy; qty = 2.0 } in
    let limit_order1 = { id = 1308; user_id = 38; security = "ME7"; order_type = Limit { price = 98.0; expiration = None }; buy_sell = Sell; qty = 1.0 } in
    ignore (add_order book1 margin_order1);
    ignore (add_order book1 limit_order1);

    let market_conds = create_market_conditions 5.0 0.5 in
    let trades1 = match_orders book1 market_conds in
    assert_equal 1 (List.length trades1);
    
    (* limit buy, margin sell *)
    ignore (create_security "ME9" 100.0);
    let book2 = create_order_book "ME9" in
    ignore (create_user_in_db 39 "UserME9" 1000.0);
    ignore (create_user_in_db 40 "UserME10" 1000.0);
    let limit_order2 = { id = 1309; user_id = 39; security = "ME9"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Buy; qty = 2.0 } in
    let margin_order2 = { id = 1310; user_id = 40; security = "ME9"; order_type = Margin 98.0; buy_sell = Sell; qty = 1.0 } in
    ignore (add_order book2 limit_order2);
    ignore (add_order book2 margin_order2);
    let trades2 = match_orders book2 market_conds in
    assert_equal 1 (List.length trades2);
    
    (* margin buy, margin sell *)
    ignore (create_security "ME11" 100.0);
    let book3 = create_order_book "ME11" in
    ignore (create_user_in_db 41 "UserME11" 1000.0);
    ignore (create_user_in_db 42 "UserME12" 1000.0);
    let margin_order3 = { id = 1311; user_id = 41; security = "ME11"; order_type = Margin 100.0; buy_sell = Buy; qty = 2.0 } in
    let margin_order4 = { id = 1312; user_id = 42; security = "ME11"; order_type = Margin 98.0; buy_sell = Sell; qty = 1.0 } in
    ignore (add_order book3 margin_order3);
    ignore (add_order book3 margin_order4);
    let trades3 = match_orders book3 market_conds in
    assert_equal 1 (List.length trades3)

  let test_execute_trade _ =
    ignore (create_security "ME13" 100.0);
    let book = create_order_book "ME13" in
    ignore (create_user_in_db 43 "UserME13" 1000.0);
    ignore (create_user_in_db 44 "UserME14" 1000.0);

    (let buy_order = { id = 1313; user_id = 43; security = "ME13"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Buy; qty = 2.0 } in
    let sell_order = { id = 1314; user_id = 44; security = "ME13"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Sell; qty = 1.0 } in
    ignore (add_order book buy_order);
    ignore (add_order book sell_order);
    
    match execute_trade buy_order sell_order with
    | Ok (trade_qty, trade_price) ->
      assert_equal 1.0 trade_qty;
      assert_equal 100.0 trade_price;
      (match get_order 1313 with
      | Ok res ->
        let new_qty_buy = float_of_string (res#getvalue 0 5) in
        assert_equal 1.0 new_qty_buy
      | Error e -> assert_failure e);
      (match get_order 1314 with
      | Ok res ->
        let new_qty_sell = float_of_string (res#getvalue 0 5) in
        assert_equal 0.0 new_qty_sell
      | Error e -> assert_failure e)
    | Error e -> assert_failure e);

    (let buy_order = { id = 1353; user_id = 43; security = "ME13"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Buy; qty = 1.0 } in
    let sell_order = { id = 1354; user_id = 44; security = "ME13"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Sell; qty = 2.0 } in
    ignore (add_order book buy_order);
    ignore (add_order book sell_order);
    
    match execute_trade buy_order sell_order with
    | Ok (trade_qty, trade_price) ->
      assert_equal 1.0 trade_qty;
      assert_equal 100.0 trade_price;
      (match get_order 1353 with
      | Ok res ->
        let new_qty_buy = float_of_string (res#getvalue 0 5) in
        assert_equal 0.0 new_qty_buy
      | Error e -> assert_failure e);
      (match get_order 1354 with
      | Ok res ->
        let new_qty_sell = float_of_string (res#getvalue 0 5) in
        assert_equal 1.0 new_qty_sell
      | Error e -> assert_failure e)
    | Error e -> assert_failure e)

  let test_match_orders _ = 
    ignore (create_security "ME15" 100.0);
    let book = create_order_book "ME15" in
    let market_conds = create_market_conditions 5.0 0.5 in
    ignore (create_user_in_db 45 "UserME15" 1000.0);
    ignore (create_user_in_db 46 "UserME16" 1000.0);

    let trades_none = match_orders book market_conds in
    assert_equal 0 (List.length trades_none);
    
    let buy_order = { id = 1315; user_id = 45; security = "ME15"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Buy; qty = 2.0 } in
    let sell_order = { id = 1316; user_id = 46; security = "ME15"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Sell; qty = 1.0 } in
    ignore (add_order book buy_order);
    ignore (add_order book sell_order);
    
    let trades_actual = match_orders book market_conds in
    assert_equal 1 (List.length trades_actual);

    let trade = List.hd trades_actual in
    assert_equal 1.0 trade.qty;
    assert_equal 100.0 trade.price;

    let sell_market = { id = 1356; user_id = 46; security = "ME15"; order_type = Market; buy_sell = Sell; qty = 1.0 } in
    ignore (add_order book sell_market);

    let trades_market = match_orders book market_conds in
    assert_equal 1 (List.length trades_market)

  let test_match_multiple_books _ =
    ignore (create_security "ME17" 100.0);
    ignore (create_security "ME18" 150.0);
    let book1 = create_order_book "ME17" in
    let book2 = create_order_book "ME18" in

    ignore (create_user_in_db 47 "UserME17" 10000.0);
    ignore (create_user_in_db 48 "UserME18" 10000.0);
    
    let order1 = { id = 1317; user_id = 47; security = "ME17"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Buy; qty = 2.0 } in
    let order2 = { id = 1318; user_id = 48; security = "ME17"; order_type = Limit { price = 102.0; expiration = None }; buy_sell = Sell; qty = 1.0 } in
    let order3 = { id = 1319; user_id = 47; security = "ME18"; order_type = Limit { price = 155.0; expiration = None }; buy_sell = Buy; qty = 1.0 } in
    let order4 = { id = 1320; user_id = 48; security = "ME18"; order_type = Limit { price = 145.0; expiration = None }; buy_sell = Sell; qty = 0.5 } in
    let order5 = { id = 1321; user_id = 47; security = "ME18"; order_type = Limit { price = 161.0; expiration = None }; buy_sell = Buy; qty = 1.0 } in
    let order6 = { id = 1322; user_id = 48; security = "ME18"; order_type = Limit { price = 159.0; expiration = None }; buy_sell = Sell; qty = 1.0 } in
    ignore (add_order book1 order1);
    ignore (add_order book1 order2);
    ignore (add_order book2 order3);
    ignore (add_order book2 order4);
    ignore (add_order book1 order5);
    ignore (add_order book1 order6);
    let market_conds = create_market_conditions 5.0 0.5 in
    let trades = match_all_books [book1; book2] market_conds in
    assert_equal 2 (List.length trades) (* 2 trades: (order1, order2) and (order5, order6); (order3, order4) is out of spread *)

  let test_no_matching_orders _ =
    ignore (create_security "ME19" 100.0);
    let book = create_order_book "ME19" in
    let market_conds = create_market_conditions 2.0 0.5 in
    ignore (create_user_in_db 49 "UserME19" 1000.0);
    ignore (create_user_in_db 50 "UserME20" 1000.0);
    
    let order1 = { id = 1323; user_id = 49; security = "ME19"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Buy; qty = 10.0 } in
    let order2 = { id = 1324; user_id = 50; security = "ME19"; order_type = Limit { price = 105.0; expiration = None }; buy_sell = Sell; qty = 5.0 } in
    ignore (add_order book order1);
    ignore (add_order book order2);
    
    let trades = match_orders book market_conds in
    assert_equal 0 (List.length trades);

    let order3 = { id = 1359; user_id = 49; security = "ME19"; order_type = Market; buy_sell = Buy; qty = 5.0 } in
    let order4 = { id = 1360; user_id = 50; security = "ME19"; order_type = Market; buy_sell = Sell; qty = 5.0 } in
    ignore (add_order book order3);
    ignore (add_order book order4);

    try
      let trades = match_orders book market_conds in
      assert_equal 0 (List.length trades);
      assert_failure "Expected failure with both market orders"
    with Failure msg ->
      assert_equal "Both orders cannot be market orders" msg

  let test_check_spread_edge_cases _ =
    ignore (create_security "ME21" 100.0);
    let book = create_order_book "ME21" in
    let market_conds = create_market_conditions 2.0 0.5 in
    ignore (create_user_in_db 51 "UserME21" 1000.0);

    assert_equal false (check_spread book market_conds);

    let market_order = { id = 1325; user_id = 51; security = "ME21"; order_type = Market; buy_sell = Buy; qty = 1.0 } in
    ignore (add_order book market_order);
    assert_equal false (check_spread book market_conds); (* market order should not change spread *)

    let limit_order = { id = 1326; user_id = 51; security = "ME21"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Buy; qty = 1.0 } in
    ignore (add_order book limit_order);
    assert_equal false (check_spread book market_conds) (* one best bid, but not best ask *)

    (* no best bid in book *)

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
    "test_check_spread_edge_cases" >:: test_check_spread_edge_cases;
    "cleanup" >:: cleanup
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
