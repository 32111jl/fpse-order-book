DROP TABLE IF EXISTS trades CASCADE;
DROP TABLE IF EXISTS positions CASCADE;
DROP TABLE IF EXISTS orders CASCADE;
DROP TABLE IF EXISTS users CASCADE;
DROP TABLE IF EXISTS securities CASCADE;

-- \i src/database/schema.sql

CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  name VARCHAR(100) NOT NULL,
  balance FLOAT NOT NULL
);

CREATE TABLE securities (
  symbol VARCHAR(10) PRIMARY KEY,
  price FLOAT NOT NULL,
  status VARCHAR(10) DEFAULT 'ACTIVE'
);

CREATE TABLE orders (
  id SERIAL PRIMARY KEY,
  user_id INTEGER REFERENCES users(id),
  security VARCHAR(10) REFERENCES securities(symbol),
  order_type VARCHAR(10) NOT NULL,
  buy_sell VARCHAR(4) NOT NULL,
  quantity FLOAT NOT NULL,
  price FLOAT NOT NULL,
  status VARCHAR(10) DEFAULT 'ACTIVE',
  expiration_time FLOAT
);

CREATE TABLE positions (
  user_id INTEGER REFERENCES users(id),
  security VARCHAR(10) REFERENCES securities(symbol),
  quantity FLOAT NOT NULL,
  PRIMARY KEY (user_id, security)
);

CREATE TABLE trades (
  id SERIAL PRIMARY KEY,
  buy_order_id INTEGER REFERENCES orders(id),
  sell_order_id INTEGER REFERENCES orders(id),
  security VARCHAR(10) NOT NULL,
  quantity FLOAT NOT NULL,
  price FLOAT NOT NULL,
  timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

INSERT INTO securities (symbol, price, status) VALUES
  ('AAPL', 150.00, 'ACTIVE'),
  ('MSFT', 330.00, 'ACTIVE'),
  ('GOOGL', 140.00, 'ACTIVE'),
  ('AMZN', 180.00, 'ACTIVE'),
  ('TSLA', 300.00, 'ACTIVE'),
  ('META', 300.00, 'ACTIVE'),
  ('NVDA', 400.00, 'ACTIVE'),
  ('RKLB', 20.00, 'ACTIVE'),
  ('RIVN', 15.00, 'ACTIVE'),
  ('PLTR', 53.00, 'ACTIVE')
ON CONFLICT (symbol) DO NOTHING;
