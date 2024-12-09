open OUnit2
open Database.Db
open Order_book_lib.Order_book
open Order_book_lib.Order_types
open Order_book_lib.Matching_engine
open Order_book_lib.Market_conditions
open Order_book_lib.Utils

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
    match create_order_in_db user "AAPL" order_type buy_sell 1.0 150.0 with
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
    ignore (create_order_in_db user "AAPL" order_type Buy 1.0 150.0);
    ignore (create_order_in_db user "GOOGL" order_type Buy 2.0 200.0);
    
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
    match create_order_in_db user "AAPL" order_type Buy 1.0 150.0 with
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

    match create_order_in_db user1 "AAPL" (Limit { price = 150.0; expiration = None }) Buy 1.0 150.0 with
    | Ok result ->
      let buy_order_id = int_of_string (result#getvalue 0 0) in
      (match create_order_in_db user2 "AAPL" (Limit { price = 150.0; expiration = None }) Sell 1.0 150.0 with
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
    match create_order_in_db user1 "SEC3" (Limit { price = 300.0; expiration = None }) Buy 1.0 300.0 with
    | Ok result ->
      let buy_order_id = int_of_string (result#getvalue 0 0) in
      (match create_order_in_db user2 "SEC3" (Limit { price = 300.0; expiration = None }) Sell 1.0 300.0 with
      | Ok result ->
        let sell_order_id = int_of_string (result#getvalue 0 0) in
        ignore (record_trade ~buy_order_id ~sell_order_id ~security:"SEC3" ~qty:1.0 ~price:300.0);
        (match get_trades_by_security "SEC3" with
        | Ok result ->
          assert_equal 1 result#ntuples;
          assert_equal "SEC3" (result#getvalue 0 3);
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
    assert_equal "OB1" book.security;
    let bids = get_bids book in
    let asks = get_asks book in
    assert_equal 0 (List.length bids);
    assert_equal 0 (List.length asks)

  let test_get_best_bid_ask_empty _ =
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
    add_order_to_memory book buy_order;
    add_order_to_memory book sell_order;
    assert_equal (Some 101.0) (get_best_bid book);
    assert_equal (Some 102.0) (get_best_ask book)

  let test_add_multiple_buy_orders_check_bid _ =
    let book = create_order_book "OB4" in
    let user1 = create_user_in_db "UserOB3" 1000.0 in
    let user2 = create_user_in_db "UserOB4" 1000.0 in

    let buy_order1 = { id = None; user_id = user1; security = "OB4"; order_type = Limit { price = 101.0; expiration = None }; buy_sell = Buy; qty = 2.0 } in
    let buy_order2 = { id = None; user_id = user2; security = "OB4"; order_type = Limit { price = 102.0; expiration = None }; buy_sell = Buy; qty = 1.0 } in
    
    add_order_to_memory book buy_order1;
    assert_equal (Some 101.0) (get_best_bid book);
    add_order_to_memory book buy_order2;
    assert_equal (Some 102.0) (get_best_bid book)

  let test_add_multiple_sell_orders_check_ask _ =
    let book = create_order_book "OB5" in
    let user1 = create_user_in_db "UserOB5" 1000.0 in
    let user2 = create_user_in_db "UserOB6" 1000.0 in

    let sell_order1 = { id = None; user_id = user1; security = "OB5"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Sell; qty = 1.0 } in
    let sell_order2 = { id = None; user_id = user2; security = "OB5"; order_type = Limit { price = 102.0; expiration = None }; buy_sell = Sell; qty = 2.0 } in
    
    add_order_to_memory book sell_order2;
    assert_equal (Some 102.0) (get_best_ask book);
    add_order_to_memory book sell_order1;
    assert_equal (Some 100.0) (get_best_ask book)

  let test_get_bids_and_asks_ordering _ =
    ignore (create_security "OB6" 100.0);
    let book = create_order_book "OB6" in
    let user1 = create_user_in_db "UserOB7" 1000.0 in
    let user2 = create_user_in_db "UserOB8" 1000.0 in
    let user3 = create_user_in_db "UserOB9" 1000.0 in

    let buy_order1 = { id = None; user_id = user1; security = "OB6"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Buy; qty = 1.0 } in
    let buy_order2 = { id = None; user_id = user2; security = "OB6"; order_type = Limit { price = 105.0; expiration = None }; buy_sell = Buy; qty = 1.0 } in
    let buy_order3 = { id = None; user_id = user3; security = "OB6"; order_type = Limit { price = 101.0; expiration = None }; buy_sell = Buy; qty = 1.0 } in
    add_order_to_memory book buy_order1;
    add_order_to_memory book buy_order2;
    add_order_to_memory book buy_order3;

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
    add_order_to_memory book buy_order;
    assert_equal (Some 100.0) (get_best_bid book);
    let order = List.hd (get_bids book) in
    try
      remove_order_from_memory book (unwrap_id order.id);
      assert_equal None (get_best_bid book)
    with Failure msg -> assert_equal "Order has no ID." msg

  let test_get_best_bid_ask_cached _ =
    ignore (create_security "OB9" 100.0);
    let book = create_order_book "OB9" in
    let user1 = create_user_in_db "UserOB12" 1000.0 in
    
    let buy_order = { id = None; user_id = user1; security = "OB9"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Buy; qty = 1.0 } in
    let sell_order = { id = None; user_id = user1; security = "OB9"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Sell; qty = 1.0 } in
    add_order_to_memory book buy_order;
    assert_equal (Some 100.0) (get_best_bid book);
    assert_equal None (get_best_ask book);
    add_order_to_memory book sell_order;
    assert_equal (Some 100.0) (get_best_ask book)

  let test_print_orders _ =
    let book = create_order_book "TEST" in
    let order1 = { id = Some 1; user_id = 100; security = "TEST"; order_type = Limit { price = 101.0; expiration = None }; buy_sell = Buy; qty = 2.0 } in
    let order2 = { id = Some 2; user_id = 200; security = "TEST"; order_type = Limit { price = 99.0; expiration = None }; buy_sell = Sell; qty = 1.0 } in
    let order3 = { id = Some 3; user_id = 300; security = "TEST"; order_type = Market; buy_sell = Buy; qty = 3.0 } in
    let order4 = { id = Some 4; user_id = 400; security = "TEST"; order_type = Market; buy_sell = Sell; qty = 4.0 } in
    let order5 = { id = Some 5; user_id = 500; security = "TEST"; order_type = Margin 100.0; buy_sell = Buy; qty = 5.0 } in
    let order6 = { id = Some 6; user_id = 600; security = "TEST"; order_type = Margin 100.0; buy_sell = Sell; qty = 6.0 } in
    
    add_order_to_memory book order1;
    add_order_to_memory book order2;
    add_order_to_memory book order3;
    add_order_to_memory book order4;
    add_order_to_memory book order5;
    add_order_to_memory book order6;
    
    Printf.printf "Test print orders (Limit/Market/Margin):\n";
    print_orders book;
    assert_bool "print_orders executed." true

  let test_print_trades _ = 
    let trade = { buy_order_id = 1; sell_order_id = 2; security = "TEST"; qty = 5.0; price = 100.0 } in
    Printf.printf "Test print trades:\n";
    print_trade trade "TEST";
    assert_bool "print_trade executed" true

  let series = "order_book_tests" >::: [
    "test_create_order_book" >:: test_create_order_book;
    "test_get_best_bid_ask_empty" >:: test_get_best_bid_ask_empty;
    "test_add_limit_orders_and_check_best_bid_ask" >:: test_add_limit_orders_and_check_best_bid_ask;
    "test_add_multiple_buy_orders_check_bid" >:: test_add_multiple_buy_orders_check_bid;
    "test_add_multiple_sell_orders_check_ask" >:: test_add_multiple_sell_orders_check_ask;
    "test_get_bids_and_asks_ordering" >:: test_get_bids_and_asks_ordering;
    "test_add_and_remove_order" >:: test_add_and_remove_order;
    "test_get_best_bid_ask_cached" >:: test_get_best_bid_ask_cached;
    "test_print_orders" >:: test_print_orders;
    "test_print_trades" >:: test_print_trades;
  ]
end

module MatchingEngineTests = struct

  let test_get_margin_rate _ =
    let market_conds = create_market_conditions 3.0 0.5 in
    assert_equal 0.5 (get_margin_rate market_conds);
    let market_conds2 = create_market_conditions 2.0 0.75 in
    assert_equal 0.75 (get_margin_rate market_conds2)

  let test_get_trade_price _ =
    ignore (create_security "ME5" 100.0);
    let user1 = create_user_in_db "UserME5" 1000.0 in
    let user2 = create_user_in_db "UserME6" 1000.0 in
    
    let buy_order = { id = None; user_id = user1; security = "ME5"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Buy; qty = 10.0 } in
    let sell_order = { id = None; user_id = user2; security = "ME5"; order_type = Market; buy_sell = Sell; qty = 5.0 } in
    
    assert_equal 100.0 (get_trade_price buy_order sell_order);
    
    let market_buy = { id = None; user_id = user1; security = "ME5"; order_type = Market; buy_sell = Buy; qty = 10.0 } in
    try
      ignore (get_trade_price market_buy sell_order);
      assert_failure "Expected failure with both market orders"
    with Failure msg -> assert_equal "Both cannot be market orders." msg

  let test_match_orders_with_margin _ =
    (* margin buy, limit sell *)
    ignore (create_security "ME7" 100.0);
    let market_conds = create_market_conditions 5.0 0.5 in
    let book1 = create_order_book "ME7" in
    let user1 = create_user_in_db "UserME7" 1000.0 in
    let user2 = create_user_in_db "UserME8" 1000.0 in
    let margin_order1 = { id = Some 1; user_id = user1; security = "ME7"; order_type = Margin 100.0; buy_sell = Buy; qty = 2.0 } in
    let limit_order1 = { id = Some 2; user_id = user2; security = "ME7"; order_type = Limit { price = 98.0; expiration = None }; buy_sell = Sell; qty = 1.0 } in
    add_order_to_memory book1 margin_order1;
    add_order_to_memory book1 limit_order1;
    let trades1 = match_orders book1 market_conds in
    assert_equal 1 (List.length trades1);
    
    (* limit buy, margin sell *)
    ignore (create_security "ME9" 100.0);
    let book2 = create_order_book "ME9" in
    let user1 = create_user_in_db "UserME9" 1000.0 in
    let user2 = create_user_in_db "UserME10" 1000.0 in
    let limit_order2 = { id = Some 3; user_id = user1; security = "ME9"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Buy; qty = 2.0 } in
    let margin_order2 = { id = Some 4; user_id = user2; security = "ME9"; order_type = Margin 98.0; buy_sell = Sell; qty = 1.0 } in
    add_order_to_memory book2 limit_order2;
    add_order_to_memory book2 margin_order2;
    let trades2 = match_orders book2 market_conds in
    assert_equal 1 (List.length trades2);
    
    (* margin buy, margin sell *)
    ignore (create_security "ME11" 100.0);
    let book3 = create_order_book "ME11" in
    let user1 = create_user_in_db "UserME11" 1000.0 in
    let user2 = create_user_in_db "UserME12" 1000.0 in
    let margin_order3 = { id = Some 5; user_id = user1; security = "ME11"; order_type = Margin 100.0; buy_sell = Buy; qty = 2.0 } in
    let margin_order4 = { id = Some 6; user_id = user2; security = "ME11"; order_type = Margin 98.0; buy_sell = Sell; qty = 1.0 } in
    add_order_to_memory book3 margin_order3;
    add_order_to_memory book3 margin_order4;
    let trades3 = match_orders book3 market_conds in
    assert_equal 1 (List.length trades3)

  (* let test_execute_trade _ =
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
    | Error e -> assert_failure e *)

  let test_match_orders _ = 
    ignore (create_security "ME15" 100.0);
    let book = create_order_book "ME15" in
    let market_conds = create_market_conditions 5.0 0.5 in
    let user1 = create_user_in_db "UserME15" 1000.0 in
    let user2 = create_user_in_db "UserME16" 1000.0 in

    let trades_none = match_orders book market_conds in
    assert_equal 0 (List.length trades_none);
    
    let buy_limit = { id = Some 1; user_id = user1; security = "ME15"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Buy; qty = 2.0 } in
    let sell_limit = { id = Some 2; user_id = user2; security = "ME15"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Sell; qty = 1.0 } in
    add_order_to_memory book buy_limit;
    add_order_to_memory book sell_limit;
    let trades_actual = match_orders book market_conds in
    assert_equal 1 (List.length trades_actual);

    let trade = List.hd trades_actual in
    assert_equal 1.0 trade.qty;
    assert_equal 100.0 trade.price;

    let sell_market = { id = Some 3; user_id = user2; security = "ME15"; order_type = Market; buy_sell = Sell; qty = 1.0 } in
    add_order_to_memory book sell_market;
    let trades_market = match_orders book market_conds in
    assert_equal 1 (List.length trades_market)

  let test_match_multiple_books _ =
    ignore (create_security "ME17" 100.0);
    ignore (create_security "ME18" 150.0);
    let book1 = create_order_book "ME17" in
    let book2 = create_order_book "ME18" in

    let user1 = create_user_in_db "UserME17" 10000.0 in
    let user2 = create_user_in_db "UserME18" 10000.0 in
    
    let order1 = { id = Some 1; user_id = user1; security = "ME17"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Buy; qty = 2.0 } in
    let order2 = { id = Some 2; user_id = user2; security = "ME17"; order_type = Limit { price = 102.0; expiration = None }; buy_sell = Sell; qty = 1.0 } in
    let order3 = { id = Some 3; user_id = user1; security = "ME18"; order_type = Limit { price = 155.0; expiration = None }; buy_sell = Buy; qty = 1.0 } in
    let order4 = { id = Some 4; user_id = user2; security = "ME18"; order_type = Limit { price = 145.0; expiration = None }; buy_sell = Sell; qty = 0.5 } in
    let order5 = { id = Some 5; user_id = user1; security = "ME18"; order_type = Limit { price = 161.0; expiration = None }; buy_sell = Buy; qty = 1.0 } in
    let order6 = { id = Some 6; user_id = user2; security = "ME18"; order_type = Limit { price = 159.0; expiration = None }; buy_sell = Sell; qty = 1.0 } in
    
    add_order_to_memory book1 order1;
    add_order_to_memory book1 order2;
    add_order_to_memory book2 order3;
    add_order_to_memory book2 order4;
    add_order_to_memory book1 order5;
    add_order_to_memory book1 order6;
    
    let market_conds = create_market_conditions 5.0 0.5 in
    let trades = match_all_books [book1; book2] market_conds in
    assert_equal 2 (List.length trades) (* 2 trades: (order1, order2) and (order5, order6); (order3, order4) is out of spread *)

  let test_no_matching_orders _ =
    ignore (create_security "ME19" 100.0);
    let book = create_order_book "ME19" in
    let market_conds = create_market_conditions 2.0 0.5 in
    let user1 = create_user_in_db "UserME19" 1000.0 in
    let user2 = create_user_in_db "UserME20" 1000.0 in
    
    let order1 = { id = Some 1; user_id = user1; security = "ME19"; order_type = Limit { price = 100.0; expiration = None }; buy_sell = Buy; qty = 10.0 } in
    let order2 = { id = Some 2; user_id = user2; security = "ME19"; order_type = Limit { price = 105.0; expiration = None }; buy_sell = Sell; qty = 5.0 } in
    
    add_order_to_memory book order1;
    add_order_to_memory book order2;
    
    (* no trades since outside of spread *)
    let trades1 = match_orders book market_conds in
    assert_equal 0 (List.length trades1);

    book.orders <- [];
    let order3 = { id = Some 3; user_id = user1; security = "ME19"; order_type = Market; buy_sell = Buy; qty = 5.0 } in
    let order4 = { id = Some 4; user_id = user2; security = "ME19"; order_type = Market; buy_sell = Sell; qty = 5.0 } in
    add_order_to_memory book order3;
    add_order_to_memory book order4;
    let trades2 = 
      try match_orders book market_conds 
      with Failure msg ->
        assert_equal "Both cannot be market orders." msg;
        [] (* no trades *)
    in
    assert_equal 0 (List.length trades2)

  let series = "matching_engine_tests" >::: [
    "test_get_margin_rate" >:: test_get_margin_rate;
    "test_get_trade_price" >:: test_get_trade_price;
    "test_match_orders_with_margin" >:: test_match_orders_with_margin;
    "test_match_orders" >:: test_match_orders;
    "test_match_multiple_books" >:: test_match_multiple_books;
    "test_no_matching_orders" >:: test_no_matching_orders;
  ]
end

module UtilsTests = struct
  let test_round_price _ =
    assert_equal 123.45 (round_price 123.446);
    assert_equal 123.45 (round_price 123.454);
    assert_equal 100.0 (round_price 100.00049)
  
  let test_round_quantity _ =
    assert_equal 10.0 (round_quantity 10.0);
    assert_equal 10.12 (round_quantity 10.1234)
  
  let test_random_float_between _ =
    let v = random_float_between 5.0 10.0 in
    assert_bool "v between 5 and 10" (v >= 5.0 && v < 10.0)

  let test_random_price _ =
    let v = random_price 100.0 5.0 in
    assert_bool "v between 95. and 105." (v >= 95.0 && v < 105.0)

  let test_curr_time _ =
    let v = current_time () in
    assert_bool "v is a float" (v >= 0.0)

  let test_is_expired _ =
    let curr = current_time () in
    assert_bool "None not expired" (not (is_expired None));
    assert_bool "Future not expired" (not (is_expired (Some (curr +. 1000.0))));
    assert_bool "Past expired" (is_expired (Some (curr -. 1000.0)))

  let test_string_to_order_type _ =
    assert_equal Market (string_to_order_type "market" 0.0);
    assert_equal (Limit { price=150.0; expiration=None }) (string_to_order_type "limit" 150.0);
    assert_equal (Margin 200.0) (string_to_order_type "margin" 200.0);
    assert_raises (Failure "Invalid order type: BLAH") (fun () -> string_to_order_type "BLAH" 0.0)

  let test_order_type_to_string _ =
    assert_equal "MARKET" (order_type_to_string Market);
    assert_equal "LIMIT" (order_type_to_string (Limit { price=100.0; expiration=None }));
    assert_equal "MARGIN" (order_type_to_string (Margin 50.0))

  let test_string_to_buy_sell _ =
    assert_equal Buy (string_to_buy_sell "BUY");
    assert_equal Sell (string_to_buy_sell "sell");
    assert_raises (Failure "Invalid buy/sell: HOLD") (fun () -> string_to_buy_sell "HOLD")

  let test_buy_sell_to_string _ =
    assert_equal "BUY" (buy_sell_to_string Buy);
    assert_equal "SELL" (buy_sell_to_string Sell)

  let test_unwrap_id _ =
    assert_equal 123 (unwrap_id (Some 123));
    assert_raises (Failure "Order has no ID.") (fun () -> unwrap_id None)
  
  let test_compare_price_options _ =
    assert_equal 0 (compare_price_options (Some 100.0) (Some 100.0));
    assert_equal 1 (compare_price_options (Some 101.0) None);
    assert_equal (-1) (compare_price_options None (Some 101.0));
    assert_equal 0 (compare_price_options None None);
    assert_equal (-1) (compare_price_options (Some 100.0) (Some 101.0))

  let series = "utils_tests" >::: [
    "test_round_price" >:: test_round_price;
    "test_round_quantity" >:: test_round_quantity;
    "test_random_float_between" >:: test_random_float_between;
    "test_random_price" >:: test_random_price;
    "test_curr_time" >:: test_curr_time;
    "test_is_expired" >:: test_is_expired;
    "test_string_to_order_type" >:: test_string_to_order_type;
    "test_order_type_to_string" >:: test_order_type_to_string;
    "test_string_to_buy_sell" >:: test_string_to_buy_sell;
    "test_buy_sell_to_string" >:: test_buy_sell_to_string;
    "test_unwrap_id" >:: test_unwrap_id;
    "test_compare_price_options" >:: test_compare_price_options
  ]
end


let test_suite = "Tests" >::: [
  DbTests.series;
  OrderBookTests.series;
  MatchingEngineTests.series;
  UtilsTests.series
]

let () =
  (* test_setup (); *)
  run_test_tt_main test_suite
