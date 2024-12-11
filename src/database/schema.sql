-- DROP TABLE IF EXISTS trades CASCADE;
-- DROP TABLE IF EXISTS orders CASCADE;
-- DROP TABLE IF EXISTS positions CASCADE;
-- DROP TABLE IF EXISTS users CASCADE;

-- users table
CREATE TABLE IF NOT EXISTS users (
  id BIGINT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
  name VARCHAR(255) NOT NULL,
  balance DECIMAL(15,2) NOT NULL DEFAULT 0,
  created_at TIMESTAMPTZ DEFAULT NOW()
);

-- securities table
CREATE TABLE IF NOT EXISTS securities (
  symbol TEXT PRIMARY KEY,
  price DECIMAL(15,2) NOT NULL,
  status VARCHAR(10) NOT NULL DEFAULT 'ACTIVE',
  created_at TIMESTAMPTZ DEFAULT NOW()
);

-- positions table (user holdings)
CREATE TABLE IF NOT EXISTS positions (
  user_id BIGINT REFERENCES users(id),
  security TEXT REFERENCES securities(symbol),
  quantity DECIMAL(15,2) NOT NULL DEFAULT 0,
  created_at TIMESTAMPTZ DEFAULT NOW(),
  PRIMARY KEY (user_id, security)
);

-- orders table
CREATE TABLE IF NOT EXISTS orders (
  id BIGINT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
  user_id BIGINT REFERENCES users(id),
  security TEXT REFERENCES securities(symbol),
  order_type VARCHAR(10) NOT NULL,
  buy_sell VARCHAR(4) NOT NULL,
  quantity DECIMAL(15,2) NOT NULL,
  price DECIMAL(15,2) NOT NULL,
  status VARCHAR(10) DEFAULT 'ACTIVE',
  expiration_time DECIMAL(15,2),
  created_at TIMESTAMPTZ DEFAULT NOW()
);

-- trades table
CREATE TABLE IF NOT EXISTS trades (
  id BIGINT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
  buy_order_id BIGINT REFERENCES orders(id),
  sell_order_id BIGINT REFERENCES orders(id),
  security TEXT REFERENCES securities(symbol),
  quantity DECIMAL(15,2) NOT NULL,
  price DECIMAL(15,2) NOT NULL,
  created_at TIMESTAMPTZ DEFAULT NOW()
);

-- add indexes for better performance
CREATE INDEX IF NOT EXISTS idx_orders_user_id ON orders(user_id);
CREATE INDEX IF NOT EXISTS idx_orders_security ON orders(security);
CREATE INDEX IF NOT EXISTS idx_positions_user_id ON positions(user_id);