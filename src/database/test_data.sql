-- Clear existing data (if any)
TRUNCATE TABLE trades CASCADE;
TRUNCATE TABLE positions CASCADE;
TRUNCATE TABLE orders CASCADE;
TRUNCATE TABLE users CASCADE;
TRUNCATE TABLE securities CASCADE;

-- add some randoms (negative id = computer-generated)
INSERT INTO users (id, name, balance) VALUES 
(-1000001, 'Computer_1', 10000000.0),
(-1000002, 'Computer_2', 10000000.0),
(-1000003, 'Computer_3', 10000000.0),
(-1000004, 'Computer_4', 10000000.0),
(-1000005, 'Computer_5', 10000000.0),
(1, 'Alice', 10000.00),
(2, 'Bob', 15000.00),
(3, 'Charlie', 20000.00)
ON CONFLICT (id) DO NOTHING;

-- add some securities with random prices
INSERT INTO securities (symbol, price, status) VALUES 
('AAPL', 180.50, 'ACTIVE'),
('GOOGL', 140.25, 'ACTIVE'),
('MSFT', 330.75, 'ACTIVE');

-- add some active orders
INSERT INTO orders (id, user_id, security, order_type, buy_sell, quantity, price, status) VALUES 
-- Alice's orders
(1, 1, 'AAPL', 'LIMIT', 'BUY', 10.0, 179.50, 'ACTIVE'),
(2, 1, 'GOOGL', 'LIMIT', 'SELL', 5.0, 141.00, 'ACTIVE'),

-- Bob's orders
(3, 2, 'AAPL', 'LIMIT', 'SELL', 8.0, 181.00, 'ACTIVE'),
(4, 2, 'MSFT', 'LIMIT', 'BUY', 12.0, 329.50, 'ACTIVE'),

-- Charlie's orders
(5, 3, 'MSFT', 'LIMIT', 'SELL', 15.0, 331.00, 'ACTIVE'),
(6, 3, 'GOOGL', 'MARKET', 'BUY', 7.0, 0.0, 'ACTIVE');

-- add some initial positions
INSERT INTO positions (user_id, security, quantity) VALUES 
(1, 'AAPL', 20.0),
(1, 'GOOGL', 15.0),
(2, 'MSFT', 25.0),
(2, 'AAPL', 30.0),
(3, 'MSFT', 40.0),
(3, 'GOOGL', 10.0);

-- add some sample trades
INSERT INTO trades (id, buy_order_id, sell_order_id, security, quantity, price) VALUES 
(1, 1, 3, 'AAPL', 5.0, 180.25),
(2, 4, 5, 'MSFT', 8.0, 330.50);