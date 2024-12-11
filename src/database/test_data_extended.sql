-- Clear existing data (if any)
TRUNCATE TABLE trades CASCADE;
TRUNCATE TABLE positions CASCADE;
TRUNCATE TABLE orders CASCADE;
TRUNCATE TABLE users CASCADE;
TRUNCATE TABLE securities CASCADE;

-- add some randoms (negative id = computer-generated)
INSERT INTO users (id, name, balance) VALUES 
-- (1, 'Computer1', 10000000.0),
-- (2, 'Computer2', 10000000.0),
-- (3, 'Computer3', 10000000.0),
-- (4, 'Computer4', 10000000.0),
-- (5, 'Computer5', 10000000.0),
(6, 'Alice', 10000.00),
(7, 'Bob', 15000.00),
(8, 'Charlie', 20000.00),
(9, 'Diana', 30000.00),
(10, 'Eve', 80000.00)
ON CONFLICT (id) DO NOTHING;

-- add some securities with random prices
INSERT INTO securities (symbol, price, status) VALUES 
('AAPL', 180.50, 'ACTIVE'),
('GOOGL', 140.25, 'ACTIVE'),
('MSFT', 330.75, 'ACTIVE'),
('TSLA', 250.10, 'ACTIVE'),
('AMZN', 125.75, 'ACTIVE'),
('RKLB', 25.00, 'ACTIVE'),
('META', 300.20, 'ACTIVE');

-- add some active orders
INSERT INTO orders (id, user_id, security, order_type, buy_sell, quantity, price, status) VALUES 
-- Alice's orders
(1, 6, 'AAPL', 'LIMIT', 'BUY', 10.0, 179.50, 'ACTIVE'), -- 1
(2, 6, 'GOOGL', 'LIMIT', 'SELL', 5.0, 141.00, 'ACTIVE'), -- 2

-- Bob's orders
(3, 7, 'AAPL', 'LIMIT', 'SELL', 8.0, 181.00, 'ACTIVE'), -- 3
(4, 7, 'MSFT', 'LIMIT', 'BUY', 12.0, 329.50, 'ACTIVE'), -- 4

-- Charlie's orders
(5, 8, 'MSFT', 'LIMIT', 'SELL', 15.0, 331.00, 'ACTIVE'), -- 5
(6, 8, 'GOOGL', 'LIMIT', 'BUY', 7.0, 140.00, 'ACTIVE'), -- 6

-- Diana's orders
(7, 9, 'TSLA', 'LIMIT', 'BUY', 5.0, 249.00, 'ACTIVE'), -- 7
(8, 9, 'META', 'LIMIT', 'SELL', 3.0, 305.00, 'ACTIVE'), -- 8

-- Eve's orders
(9, 10, 'AMZN', 'LIMIT', 'SELL', 4.0, 126.00, 'ACTIVE'), -- 9
(10, 10, 'RKLB', 'LIMIT', 'BUY', 6.0, 25.50, 'ACTIVE'); -- 10

-- add some initial positions
INSERT INTO positions (user_id, security, quantity) VALUES 
(6, 'AAPL', 20.0),
(6, 'GOOGL', 15.0),
(7, 'MSFT', 25.0),
(7, 'AAPL', 30.0),
(8, 'MSFT', 40.0),
(8, 'GOOGL', 10.0),
(9, 'TSLA', 10.0),
(9, 'META', 5.0),
(10, 'AMZN', 8.0),
(10, 'RKLB', 12.0);

-- add some sample trades
INSERT INTO trades (id, buy_order_id, sell_order_id, security, quantity, price) VALUES 
(1, 1, 3, 'AAPL', 5.0, 180.25),
(2, 4, 5, 'MSFT', 8.0, 330.50),
(3, 2, 6,'GOOGL', 2.0, 140.50);