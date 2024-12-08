-- DROP TABLE IF EXISTS trades CASCADE;
-- DROP TABLE IF EXISTS orders CASCADE;
-- DROP TABLE IF EXISTS positions CASCADE;
-- DROP TABLE IF EXISTS users CASCADE;

-- users table
CREATE TABLE IF NOT EXISTS users (
  id SERIAL PRIMARY KEY,
  name VARCHAR(255) NOT NULL,
  balance FLOAT NOT NULL
);

-- securities table
CREATE TABLE IF NOT EXISTS securities (
  symbol TEXT PRIMARY KEY,
  price FLOAT NOT NULL,
  status VARCHAR(10) NOT NULL
);

-- positions table (user holdings)
CREATE TABLE IF NOT EXISTS positions (
  user_id INTEGER REFERENCES users(id),
  security VARCHAR(10) REFERENCES securities(symbol),
  quantity FLOAT NOT NULL,
  PRIMARY KEY (user_id, security)
);

-- orders table
CREATE TABLE IF NOT EXISTS orders (
  id INTEGER PRIMARY KEY, -- SERIAL instead?
  user_id INTEGER REFERENCES users(id),
  security VARCHAR(10) REFERENCES securities(symbol),
  order_type VARCHAR(10) NOT NULL, -- 'MARKET', 'LIMIT', 'MARGIN'
  buy_sell VARCHAR(4) NOT NULL, -- 'BUY' or 'SELL'
  quantity FLOAT NOT NULL,
  price FLOAT NOT NULL,
  status VARCHAR(10) DEFAULT 'ACTIVE', -- 'ACTIVE', 'FILLED', 'PARTIAL', 'EXPIRED', 'CANCELLED'
  expiration_time FLOAT
);

-- trades table
CREATE TABLE IF NOT EXISTS trades (
  id SERIAL PRIMARY KEY, -- SERIAL auto-increments for us
  buy_order_id INTEGER REFERENCES orders(id), -- buyer's id
  sell_order_id INTEGER REFERENCES orders(id), -- seller's id
  security VARCHAR(10) REFERENCES securities(symbol),
  quantity FLOAT NOT NULL,
  price FLOAT NOT NULL
);

-- add indexes for better performance
CREATE INDEX idx_orders_user_id ON orders(user_id);
CREATE INDEX idx_orders_security ON orders(security);
CREATE INDEX idx_positions_user_id ON positions(user_id);
