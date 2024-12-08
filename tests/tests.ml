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
  
  let test_create_user_in_db _ =
    let user = create_user_in_db "CreateUser" 5000.0 in
    match get_user_balance user with
    | Some balance -> assert_equal balance 5000.00
    | None -> assert_failure "Couldn't get user balance"
  
  let test_create_order_in_db _ =
    let user = create_user_in_db "CreateOrderUser" 1000.0 in
    let order_type = Limit { price = 150.0; expiration = None } in
    let buy_sell = Buy in
    match create_order user "AAPL" order_type buy_sell 1.0 150.0 with
    | Ok result -> 
      let order_id = int_of_string (result#getvalue 0 0) in
      (match get_order order_id with
      | Ok order -> 
        assert_equal 1 order#ntuples;
        assert_equal "AAPL" (order#getvalue 0 2);
        assert_equal "LIMIT" (order#getvalue 0 3);
        assert_equal "BUY" (order#getvalue 0 4);
        assert_equal 1.00 (float_of_string (order#getvalue 0 5));
        assert_equal 150.00 (float_of_string (order#getvalue 0 6));
        assert_equal "ACTIVE" (order#getvalue 0 7)
      | Error e -> assert_failure ("Failed to get order: " ^ e))
    | Error e -> assert_failure ("Failed to create order: " ^ e)
  
  let test_update_user_balance _ =
    let user = create_user_in_db "UpdateBalanceUser" 1000.0 in
    ignore (update_user_balance user 500.0);
    match get_user_balance user with
    | Some balance -> assert_equal 1500.0 balance
    | None -> assert_failure "User not found"

  let test_get_orders_by_user _ =
    let user = create_user_in_db "GetOrdersUser" 1000.0 in
    
    let order_type = Limit { price = 150.0; expiration = None } in
    ignore (create_order user "AAPL" order_type Buy 1.0 150.0);
    ignore (create_order user "GOOGL" order_type Buy 2.0 200.0);
    
    (* order id's don't matter here, we're just checking that the function returns the correct number of orders *)
    match get_orders_by_user user with
    | Ok result -> 
      assert_equal 2 result#ntuples;
      assert_equal "AAPL" (result#getvalue 0 2);
      assert_equal "GOOGL" (result#getvalue 1 2)
    | Error e -> assert_failure ("Failed to get orders: " ^ e)
  
  let test_update_order_status _ =
    let user = create_user_in_db "UpdateOrderStatusUser" 1000.0 in
    let order_type = Limit { price = 150.0; expiration = None } in
    match create_order user "AAPL" order_type Buy 1.0 150.0 with
    | Ok result ->
      let order_id = int_of_string (result#getvalue 0 0) in
      (match update_order_status order_id "FILLED" with
      | Ok _ -> 
        (match get_order order_id with
        | Ok result -> 
          assert_equal "FILLED" (result#getvalue 0 7)
        | Error e -> assert_failure ("Failed to get order: " ^ e))
      | Error e -> assert_failure ("Failed to update status: " ^ e))
    | Error e -> assert_failure ("Failed to create order: " ^ e)
  
  let test_update_position _ =
    let user = create_user_in_db "UpdatePositionUser" 1000.0 in
    ignore (update_position user "AAPL" 10.0);
    ignore (update_position user "AAPL" 5.0);
    match get_positions_by_user user with
    | Ok result -> 
      assert_equal 1 result#ntuples;
      assert_equal 15.0 (float_of_string (result#getvalue 0 2))
    | Error e -> assert_failure ("Failed to create position: " ^ e)
  
  let test_record_and_get_trade _ =
    let user1 = create_user_in_db "TradeUser1" 1000.0 in
    let user2 = create_user_in_db "TradeUser2" 1000.0 in

    match create_order user1 "AAPL" (Limit { price = 150.0; expiration = None }) Buy 1.0 150.0 with
    | Ok result ->
      let buy_order_id = int_of_string (result#getvalue 0 0) in
      (match create_order user2 "AAPL" (Limit { price = 150.0; expiration = None }) Sell 1.0 150.0 with
      | Ok result ->
        let sell_order_id = int_of_string (result#getvalue 0 0) in
        ignore (record_trade ~buy_order_id ~sell_order_id ~security:"AAPL" ~qty:1.0 ~price:150.0);
        (match get_trade_history user1 with
        | Ok result ->
          assert_equal 1 result#ntuples;
          assert_equal "AAPL" (result#getvalue 0 3);
          assert_equal 1.0 (float_of_string (result#getvalue 0 4))
        | Error e -> assert_failure ("Failed to get trade history: " ^ e))
      | Error e -> assert_failure ("Failed to create sell order: " ^ e))
    | Error e -> assert_failure ("Failed to create buy order: " ^ e)

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
    let user1 = create_user_in_db "GetTradesSecUser1" 1000.0 in
    let user2 = create_user_in_db "GetTradesSecUser2" 1000.0 in
    match create_order user1 "TSLA" (Limit { price = 700.0; expiration = None }) Buy 1.0 700.0 with
    | Ok result ->
      let buy_order_id = int_of_string (result#getvalue 0 0) in
      (match create_order user2 "TSLA" (Limit { price = 700.0; expiration = None }) Sell 1.0 700.0 with
      | Ok result ->
        let sell_order_id = int_of_string (result#getvalue 0 0) in
        ignore (record_trade ~buy_order_id ~sell_order_id ~security:"TSLA" ~qty:1.0 ~price:700.0);
        (match get_trades_by_security "TSLA" with
        | Ok result ->
          assert_equal 1 result#ntuples;
          assert_equal "TSLA" (result#getvalue 0 3);
          assert_equal 1.0 (float_of_string (result#getvalue 0 4))
        | Error e -> assert_failure ("Failed to get trades by security: " ^ e))
      | Error e -> assert_failure ("Failed to create sell order: " ^ e))
    | Error e -> assert_failure ("Failed to create buy order: " ^ e)

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
  ]
end

module OrderBookTests = struct

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
    let user1 = create_user_in_db "UserOB1" 1000.0 in
    let user2 = create_user_in_db "UserOB2" 1000.0 in

    let buy_order = { id = None; user_id = user1; security = "OB3"; order_type = Limit { price = 101.0; expiration = None }; buy_sell = Buy; qty = 2.0 } in
    let sell_order = { id = None; user_id = user2; security = "OB3"; order_type = Limit { price = 102.0; expiration = None }; buy_sell = Sell; qty = 1.0 } in
    match add_order book buy_order with
    | Ok _ ->
      (match add_order book sell_order with
      | Ok _ ->
        assert_equal (Some 101.0) (get_best_bid book);
        assert_equal (Some 102.0) (get_best_ask book)
      | Error e -> assert_failure ("Failed to add sell order: " ^ e))
    | Error e -> assert_failure ("Failed to add buy order: " ^ e)

  let test_add_multiple_buy_orders_check_bid _ =
    ignore (create_security "OB4" 100.0);
    let book = create_order_book "OB4" in
    let user1 = create_user_in_db "UserOB3" 1000.0 in
    let user2 = create_user_in_db "UserOB4" 1000.0 in

    let buy_order1 = { id = None; user_id = user1; security = "OB4"; order_type = Limit { price = 101.0; expiration = None }; buy_sell = Buy; qty = 2.0 } in
    let buy_order2 = { id = None; user_id = user2; security = "OB4"; order_type = Limit { price = 102.0; expiration = None }; buy_sell = Buy; qty = 1.0 } in
    match add_order book buy_order1 with
    | Ok _ ->
      (match add_order book buy_order2 with
      | Ok _ -> assert_equal (Some 102.0) (get_best_bid book) (* highest price = best bid *)
      | Error e -> assert_failure ("Failed to add buy order: " ^ e))
    | Error e -> assert_failure ("Failed to add buy order: " ^ e)

  let test_add_multiple_sell_orders_check_ask _ =
    ignore (create_security "OB5" 100.0);
    let book = create_order_book "OB5" in
    let user1 = create_user_in_db "UserOB5" 1000.0 in
    let user2 = create_user_in_db "UserOB6" 1000.0 in

    let sell_order1 = { id = None; user_id = user1; security = "OB5"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Sell; qty = 1.0 } in
    let sell_order2 = { id = None; user_id = user2; security = "OB5"; order_type = Limit { price = 102.0; expiration = None }; buy_sell = Sell; qty = 2.0 } in
    match add_order book sell_order2 with
    | Ok _ -> 
      assert_equal (Some 102.0) (get_best_ask book);
      (match add_order book sell_order1 with
      | Ok _ -> assert_equal (Some 100.0) (get_best_ask book) (* lowest price = best ask *)
      | Error e -> assert_failure ("Failed to add buy order: " ^ e))
    | Error e -> assert_failure ("Failed to add sell order: " ^ e)

  let test_get_bids_and_asks_ordering _ =
    ignore (create_security "OB6" 100.0);
    let book = create_order_book "OB6" in
    let user1 = create_user_in_db "UserOB7" 1000.0 in
    let user2 = create_user_in_db "UserOB8" 1000.0 in
    let user3 = create_user_in_db "UserOB9" 1000.0 in

    let buy_order1 = { id = None; user_id = user1; security = "OB6"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Buy; qty = 1.0 } in
    let buy_order2 = { id = None; user_id = user2; security = "OB6"; order_type = Limit { price = 105.0; expiration = None }; buy_sell = Buy; qty = 1.0 } in
    let buy_order3 = { id = None; user_id = user3; security = "OB6"; order_type = Limit { price = 101.0; expiration = None }; buy_sell = Buy; qty = 1.0 } in
    ignore (add_order book buy_order1);
    ignore (add_order book buy_order2);
    ignore (add_order book buy_order3);

    let bids = get_bids book in
    assert_equal 3 (List.length bids);
    assert_equal 105.0 (match get_price (List.hd bids) with Some p -> p | None -> 0.0);
    assert_equal 101.0 (match get_price (List.nth bids 1) with Some p -> p | None -> 0.0);
    assert_equal 100.0 (match get_price (List.nth bids 2) with Some p -> p | None -> 0.0)

  let test_add_and_remove_order _ =
    ignore (create_security "OB7" 100.0);
    let user1 = create_user_in_db "UserOB10" 1000.0 in
    let book = create_order_book "OB7" in
    let buy_order = { id = None; user_id = user1; security = "OB7"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Buy; qty = 1.0 } in
    match add_order book buy_order with
    | Ok result ->
      let order_id = int_of_string (result#getvalue 0 0) in
      assert_equal true (check_order_exists book order_id);

      ignore (remove_order book order_id); (* order status should be CANCELLED *)
      assert_equal false (check_order_exists book order_id)
    | Error e -> assert_failure ("Failed to add buy order: " ^ e)

  let test_remove_expired_orders _ =
    ignore (create_security "OB8" 100.0);
    let user1 =  create_user_in_db "UserOB11" 1000.0 in
    let book = create_order_book "OB8" in
    let buy_order = { id = None; user_id = user1; security = "OB8"; order_type = Limit { price = 105.0; expiration = Some 2.0 }; buy_sell = Buy; qty = 5.0 } in
    match add_order book buy_order with
    | Ok result ->
      let order_id = int_of_string (result#getvalue 0 0) in
      ignore (remove_expired_orders book 5.0); (* should be expired since 5 > 2 *)
      (match get_order order_id with
      | Ok result ->
        assert_equal 1 result#ntuples;
        assert_equal "EXPIRED" (result#getvalue 0 7);
        let bids = get_bids book in
        assert_equal 0 (List.length bids) (* 0 since expired orders aren't active *)
      | Error e -> assert_failure ("Failed to get order: " ^ e))
    | Error e -> assert_failure ("Failed to create order: " ^ e)

  let test_get_best_bid_ask_cached _ =
    ignore (create_security "OB9" 100.0);
    let book = create_order_book "OB9" in
    let user1 = create_user_in_db "UserOB12" 1000.0 in
    
    let buy_order = { id = None; user_id = user1; security = "OB9"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Buy; qty = 1.0 } in
    let sell_order = { id = None; user_id = user1; security = "OB9"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Sell; qty = 1.0 } in
    (match add_order book buy_order with
    | Ok _ ->
      assert_equal (Some 100.0) (get_best_bid book);
      (match add_order book sell_order with
      | Ok _ -> assert_equal (Some 100.0) (get_best_ask book)
      | Error e -> assert_failure ("Failed to add sell order: " ^ e))
    | Error e -> assert_failure ("Failed to add buy order: " ^ e));

    let better_buy_order = { id = None; user_id = user1; security = "OB9"; order_type = Limit { price = 101.0; expiration = None }; buy_sell = Buy; qty = 1.0 } in
    let better_sell_order = { id = None; user_id = user1; security = "OB9"; order_type = Limit { price = 99.0; expiration = None }; buy_sell = Sell; qty = 1.0 } in
    (match add_order book better_buy_order with
    | Ok _ ->
      assert_equal (Some 101.0) (get_best_bid book);
      (match add_order book better_sell_order with
      | Ok _ -> assert_equal (Some 99.0) (get_best_ask book)
      | Error e -> assert_failure ("Failed to add sell order: " ^ e))
    | Error e -> assert_failure ("Failed to add buy order: " ^ e))

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
  ]
end

module MatchingEngineTests = struct

  let test_get_margin_rate _ =
    let market_conds = create_market_conditions 3.0 0.5 in
    assert_equal 0.5 (get_margin_rate market_conds);
    let market_conds2 = create_market_conditions 2.0 0.75 in
    assert_equal 0.75 (get_margin_rate market_conds2)

  let test_check_spread _ = 
    ignore (create_security "ME1" 100.0);
    let book = create_order_book "ME1" in
    let user1 = create_user_in_db "UserME1" 1000.0 in
    let user2 = create_user_in_db "UserME2" 1000.0 in
    
    let order1 = { id = None; user_id = user1; security = "ME1"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Buy; qty = 1.0 } in
    let order2 = { id = None; user_id = user2; security = "ME1"; order_type = Limit { price = 102.0; expiration = None }; buy_sell = Sell; qty = 1.0 } in
    match add_order book order1 with
    | Ok _ ->
      (match add_order book order2 with
      | Ok _ ->
        (* book should be updated with best bid/ask *)
        let market_conds = create_market_conditions 2.0 0.5 in
        assert_equal true (check_spread book market_conds);
        let tighter_market_conds = create_market_conditions 0.5 0.5 in
        assert_equal false (check_spread book tighter_market_conds)
      | Error e -> assert_failure ("Failed to add sell order: " ^ e))
    | Error e -> assert_failure ("Failed to add buy order: " ^ e)

  let test_check_spread_with_market_orders _ =
    ignore (create_security "ME3" 100.0);
    let book = create_order_book "ME3" in
    let user1 = create_user_in_db "UserME3" 1000.0 in
    let user2 = create_user_in_db "UserME4" 1000.0 in
    
    let order3 = { id = None; user_id = user1; security = "ME3"; order_type = Limit { price = 105.0; expiration = None }; buy_sell = Sell; qty = 1.0 } in
    let order4 = { id = None; user_id = user2; security = "ME3"; order_type = Market; buy_sell = Buy; qty = 1.0 } in
    match add_order book order3 with
    | Ok _ ->
      (match add_order book order4 with
      | Ok _ ->
        let market_conds = create_market_conditions 2.0 0.5 in
        assert_equal true (check_spread book market_conds);
        let market_conds2 = create_market_conditions 0.5 0.5 in
        assert_equal true (check_spread book market_conds2) (* this shouldn't change outcome since it's a market order *)
      | Error e -> assert_failure ("Failed to add market order: " ^ e))
    | Error e -> assert_failure ("Failed to add limit order: " ^ e)

  let test_get_trade_price _ =
    ignore (create_security "ME5" 100.0);
    let user1 = create_user_in_db "UserME5" 1000.0 in
    let user2 = create_user_in_db "UserME6" 1000.0 in
    
    let buy_order = { id = None; user_id = user1; security = "ME5"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Buy; qty = 10.0 } in
    let sell_order = { id = None; user_id = user2; security = "ME5"; order_type = Market; buy_sell = Sell; qty = 5.0 } in
    
    assert_equal 100.0 (get_trade_price buy_order sell_order);
    
    let market_buy = { buy_order with order_type = Market } in
    try
      ignore (get_trade_price market_buy sell_order);
      assert_failure "Expected failure with both market orders"
    with Failure msg -> assert_equal "Both orders cannot be market orders" msg

  let test_match_orders_with_margin _ =
    (* margin buy, limit sell *)
    ignore (create_security "ME7" 100.0);
    let book1 = create_order_book "ME7" in
    let user1 = create_user_in_db "UserME7" 1000.0 in
    let user2 = create_user_in_db "UserME8" 1000.0 in
    let margin_order1 = { id = None; user_id = user1; security = "ME7"; order_type = Margin 100.0; buy_sell = Buy; qty = 2.0 } in
    let limit_order1 = { id = None; user_id = user2; security = "ME7"; order_type = Limit { price = 98.0; expiration = None }; buy_sell = Sell; qty = 1.0 } in
    (match add_order book1 margin_order1 with
    | Ok _ ->
      (match add_order book1 limit_order1 with
      | Ok _ ->
        let market_conds = create_market_conditions 5.0 0.5 in
        let trades1 = match_orders book1 market_conds in
        assert_equal 1 (List.length trades1);
      | Error e -> assert_failure ("Failed to add limit order: " ^ e))
    | Error e -> assert_failure ("Failed to add margin order: " ^ e));
    
    (* limit buy, margin sell *)
    ignore (create_security "ME9" 100.0);
    let book2 = create_order_book "ME9" in
    let user1 = create_user_in_db "UserME9" 1000.0 in
    let user2 = create_user_in_db "UserME10" 1000.0 in
    let limit_order2 = { id = None; user_id = user1; security = "ME9"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Buy; qty = 2.0 } in
    let margin_order2 = { id = None; user_id = user2; security = "ME9"; order_type = Margin 98.0; buy_sell = Sell; qty = 1.0 } in
    (match add_order book2 limit_order2 with
    | Ok _ ->
      (match add_order book2 margin_order2 with
      | Ok _ ->
        let market_conds = create_market_conditions 5.0 0.5 in
        let trades2 = match_orders book2 market_conds in
        assert_equal 1 (List.length trades2);
      | Error e -> assert_failure ("Failed to add margin order: " ^ e))
    | Error e -> assert_failure ("Failed to add limit order: " ^ e));
    
    (* margin buy, margin sell *)
    ignore (create_security "ME11" 100.0);
    let book3 = create_order_book "ME11" in
    let user1 = create_user_in_db "UserME11" 1000.0 in
    let user2 = create_user_in_db "UserME12" 1000.0 in
    let margin_order3 = { id = None; user_id = user1; security = "ME11"; order_type = Margin 100.0; buy_sell = Buy; qty = 2.0 } in
    let margin_order4 = { id = None; user_id = user2; security = "ME11"; order_type = Margin 98.0; buy_sell = Sell; qty = 1.0 } in
    (match add_order book3 margin_order3 with
    | Ok _ ->
      (match add_order book3 margin_order4 with
      | Ok _ ->
        let market_conds = create_market_conditions 5.0 0.5 in
        let trades3 = match_orders book3 market_conds in
        assert_equal 1 (List.length trades3)
      | Error e -> assert_failure ("Failed to add margin order: " ^ e))
    | Error e -> assert_failure ("Failed to add margin order: " ^ e))

  let test_execute_trade _ =
    ignore (create_security "ME13" 100.0);
    let book = create_order_book "ME13" in
    let user1 = create_user_in_db "UserME13" 1000.0 in
    let user2 = create_user_in_db "UserME14" 1000.0 in

    let buy_order = { id = None; user_id = user1; security = "ME13"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Buy; qty = 2.0 } in
    let sell_order = { id = None; user_id = user2; security = "ME13"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Sell; qty = 1.0 } in
    ignore (add_order book buy_order);
    ignore (add_order book sell_order);
    
    (match add_order book buy_order with
    | Ok buy_result ->
      let buy_id = int_of_string (buy_result#getvalue 0 0) in
      let buy_order = { buy_order with id = Some buy_id } in
      (match add_order book sell_order with
      | Ok sell_result ->
        let sell_id = int_of_string (sell_result#getvalue 0 0) in
        let sell_order = { sell_order with id = Some sell_id } in
        
        (match execute_trade buy_order sell_order with
        | Ok (trade_qty, trade_price) ->
          assert_equal 1.0 trade_qty;
          assert_equal 100.0 trade_price;
          (match get_order buy_id with
          | Ok res ->
            let new_qty_buy = float_of_string (res#getvalue 0 5) in
            assert_equal 1.0 new_qty_buy
          | Error e -> assert_failure e);
          (match get_order sell_id with
          | Ok res ->
            let new_qty_sell = float_of_string (res#getvalue 0 5) in
            assert_equal 0.0 new_qty_sell
          | Error e -> assert_failure e)
        | Error e -> assert_failure e)
      | Error e -> assert_failure e)
    | Error e -> assert_failure e);

    let buy_order = { id = None; user_id = user1; security = "ME13"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Buy; qty = 1.0 } in
    let sell_order = { id = None; user_id = user2; security = "ME13"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Sell; qty = 2.0 } in
    match add_order book buy_order with
    | Ok buy_result ->
      let buy_id = int_of_string (buy_result#getvalue 0 0) in
      let buy_order = { buy_order with id = Some buy_id } in
      (match add_order book sell_order with
      | Ok sell_result ->
        let sell_id = int_of_string (sell_result#getvalue 0 0) in
        let sell_order = { sell_order with id = Some sell_id } in
        
        (match execute_trade buy_order sell_order with
        | Ok (trade_qty, trade_price) ->
          assert_equal 1.0 trade_qty;
          assert_equal 100.0 trade_price;
          (match get_order buy_id with
          | Ok res ->
            let new_qty_buy = float_of_string (res#getvalue 0 5) in
            assert_equal 0.0 new_qty_buy
          | Error e -> assert_failure e);
          (match get_order sell_id with
          | Ok res ->
            let new_qty_sell = float_of_string (res#getvalue 0 5) in
            assert_equal 1.0 new_qty_sell
          | Error e -> assert_failure e)
        | Error e -> assert_failure e)
      | Error e -> assert_failure e)
    | Error e -> assert_failure e

  let test_match_orders _ = 
    ignore (create_security "ME15" 100.0);
    let book = create_order_book "ME15" in
    let market_conds = create_market_conditions 5.0 0.5 in
    let user1 = create_user_in_db "UserME15" 1000.0 in
    let user2 = create_user_in_db "UserME16" 1000.0 in

    let trades_none = match_orders book market_conds in
    assert_equal 0 (List.length trades_none);
    
    let buy_order = { id = None; user_id = user1; security = "ME15"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Buy; qty = 2.0 } in
    let sell_order = { id = None; user_id = user2; security = "ME15"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Sell; qty = 1.0 } in
    match add_order book buy_order with
    | Ok buy_result ->
      let _buy_id = int_of_string (buy_result#getvalue 0 0) in
      (match add_order book sell_order with
      | Ok sell_result ->
        let _sell_id = int_of_string (sell_result#getvalue 0 0) in
        
        let trades_actual = match_orders book market_conds in
        assert_equal 1 (List.length trades_actual);

        let trade = List.hd trades_actual in
        assert_equal 1.0 trade.qty;
        assert_equal 100.0 trade.price;

        let sell_market = { id = None; user_id = user2; security = "ME15"; order_type = Market; buy_sell = Sell; qty = 1.0 } in
        (match add_order book sell_market with
        | Ok _sell_market_result ->
          let trades_market = match_orders book market_conds in
          assert_equal 1 (List.length trades_market)
        | Error e -> assert_failure ("Failed to add market order: " ^ e))
      | Error e -> assert_failure ("Failed to add sell order: " ^ e))
    | Error e -> assert_failure ("Failed to add buy order: " ^ e)

  let test_match_multiple_books _ =
    ignore (create_security "ME17" 100.0);
    ignore (create_security "ME18" 150.0);
    let book1 = create_order_book "ME17" in
    let book2 = create_order_book "ME18" in

    let user1 = create_user_in_db "UserME17" 10000.0 in
    let user2 = create_user_in_db "UserME18" 10000.0 in

    let safe_add_order book order =
      match add_order book order with
      | Ok _ -> ()
      | Error e -> assert_failure e
    in
    
    let order1 = { id = None; user_id = user1; security = "ME17"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Buy; qty = 2.0 } in
    let order2 = { id = None; user_id = user2; security = "ME17"; order_type = Limit { price = 102.0; expiration = None }; buy_sell = Sell; qty = 1.0 } in
    let order3 = { id = None; user_id = user1; security = "ME18"; order_type = Limit { price = 155.0; expiration = None }; buy_sell = Buy; qty = 1.0 } in
    let order4 = { id = None; user_id = user2; security = "ME18"; order_type = Limit { price = 145.0; expiration = None }; buy_sell = Sell; qty = 0.5 } in
    let order5 = { id = None; user_id = user1; security = "ME18"; order_type = Limit { price = 161.0; expiration = None }; buy_sell = Buy; qty = 1.0 } in
    let order6 = { id = None; user_id = user2; security = "ME18"; order_type = Limit { price = 159.0; expiration = None }; buy_sell = Sell; qty = 1.0 } in
    
    safe_add_order book1 order1;
    safe_add_order book1 order2;
    safe_add_order book2 order3;
    safe_add_order book2 order4;
    safe_add_order book1 order5;
    safe_add_order book1 order6;
    
    let market_conds = create_market_conditions 5.0 0.5 in
    let trades = match_all_books [book1; book2] market_conds in
    assert_equal 2 (List.length trades) (* 2 trades: (order1, order2) and (order5, order6); (order3, order4) is out of spread *)

  let test_no_matching_orders _ =
    ignore (create_security "ME19" 100.0);
    let book = create_order_book "ME19" in
    let market_conds = create_market_conditions 2.0 0.5 in
    let user1 = create_user_in_db "UserME19" 1000.0 in
    let user2 = create_user_in_db "UserME20" 1000.0 in
    
    let order1 = { id = None; user_id = user1; security = "ME19"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Buy; qty = 10.0 } in
    let order2 = { id = None; user_id = user2; security = "ME19"; order_type = Limit { price = 105.0; expiration = None }; buy_sell = Sell; qty = 5.0 } in
    
    match add_order book order1 with
    | Ok _ ->
      (match add_order book order2 with
      | Ok _ -> 
        let trades = match_orders book market_conds in
        assert_equal 0 (List.length trades);

        let order3 = { id = None; user_id = user1; security = "ME19"; order_type = Market; buy_sell = Buy; qty = 5.0 } in
        let order4 = { id = None; user_id = user2; security = "ME19"; order_type = Market; buy_sell = Sell; qty = 5.0 } in
        (match add_order book order3 with
        | Ok _ ->
          (match add_order book order4 with
          | Ok _ -> 
            (try
              let trades = match_orders book market_conds in
              assert_equal 0 (List.length trades);
              assert_failure "Expected failure with both market orders"
            with Failure msg -> assert_equal "Both orders cannot be market orders" msg)
          | Error e -> assert_failure ("Failed to add order4: " ^ e))
        | Error e -> assert_failure ("Failed to add order3: " ^ e))
      | Error e -> assert_failure ("Failed to add order2: " ^ e))
    | Error e -> assert_failure ("Failed to add order1: " ^ e)

  let test_check_spread_edge_cases _ =
    ignore (create_security "ME21" 100.0);
    let book = create_order_book "ME21" in
    let market_conds = create_market_conditions 2.0 0.5 in
    let user1 = create_user_in_db "UserME21" 1000.0 in

    assert_equal false (check_spread book market_conds);

    let market_order = { id = None; user_id = user1; security = "ME21"; order_type = Market; buy_sell = Buy; qty = 1.0 } in
    match add_order book market_order with
    | Ok _ ->
      assert_equal false (check_spread book market_conds); (* market order should not change spread *)

      let limit_order = { id = None; user_id = user1; security = "ME21"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Buy; qty = 1.0 } in
      (match add_order book limit_order with
      | Ok _ -> assert_equal false (check_spread book market_conds) (* one best bid, but not best ask *)
      | Error e -> assert_failure ("Failed to add limit order: " ^ e))
    | Error e -> assert_failure ("Failed to add market order: " ^ e)

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
