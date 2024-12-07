# FPSE Order Book


### About
This project is a high-performance order book application developed in OCaml. It simulates the management and matching of buy/sell orders for a given financial asset using a matching engine that efficiently pairs millions of concurrent buy/sell orders based on price and time priority. The application supports market, limit, and margin orders, along with partially-filled orders and a bid-ask spread threshold to regulate trade execution. Most user interaction will occur through a command-line interface.


### Libraries
- **Core** - for basic data structures and utilities
- **PostgreSQL** - for persistent data storage of users, orders, securities, and trades
- **Threads** - for concurrent execution of the matching engine

### Installation and Setup
##### Pre-requisites
- OCaml, Core, PostgreSQL
- Bisect_ppx (for testing)

##### Building, Running the Application
- clone the repo - `git clone https://github.com/32111jl/fpse-order-book.git`
- install dependencies - `opam install . --deps-only`
- compile the project - `dune build`
- create the database - `psql -U ob1 -d order_book -f database/schema.sql` then `psql -U ob1 -d order_book -f database/test_data.sql`
  - password is `123` if prompted
- run the application - `dune exec _build/install/default/bin/order_book_app`

Note that to test, the database command becomes `psql -U ob1 -d order_book -f tests/test_db.sql`.

### Project Timeline
11/13 - Submit project design proposal  
11/18 - Finalize `mli`'s with Professor Smith and teaching team  
12/1 - Complete implementation (finish matching engine, determine user features, have most/all of order types and related logic complete)  
12/6 - Code checkpoint (debug and have initial testing to ensure basic functionality)  
(week of) 12/9 - Final project submission (complete testing, documentation, etc. as outlined by project guidelines)  

### Current Implementation Status
##### Completed Features
- Raw CLI interface with functional user interaction (for options 1-6 at least)
- Initial in-memory implementation of entire order book (see commit `40dd3cf35c196355ec2023e124a56ff3eb0b3c58` for a working version)
- (Successful?) Database integration with PostgreSQL
  - Was a little of a rush because Prof. Smith only mentioned it 2 days before the 12/6 deadline and it was our first time integrating a database into any of our projects, past and present.
  - We would love for you to carefully review the related code and provide feedback on this as we're not sure on best practices for this.
- Comprehensive test suite for in-memory implementation (see `tests/ob_tests.ml` in previous commits, it passed 100% of the tests ... we are working on getting the database tests running in `tests/tests.ml`!)
- Basic matching engine integrated with the database
- Basic order book functionality offering market, limit, and margin orders, partial fills, a bid-ask spread threshold, and 10 stocks to trade from

##### In Progress
- More testing on database and related logic checks (ex. negative shares, etc.)
- Fleshing out other CLI commands (ex. `view_security_info`, `view_recent_trades`) and/or deciding what to remove/add
  - Adding functionality to parse CLI input
- Improving performance of matching engine
  - We tried using threads but are not sure if it's working correctly. It's included in the current code.
  - We also tried using an in-memory hashtable to store order books for faster lookup (potentially).
- Code optimization / refactoring
  - Consolidating utility functions
  - Streamlining order book operations and user-CLI interaction
  - Cleaning up / Deleting legacy in-memory implementation code (ex. `user.ml`, `order.ml`)

### Testing
Current `bisect-ppx` coverage:  
`src/order_book/market_conditions.ml` - 100%  
`src/order_book/matching_engine.ml` - 80%  
`src/order_book/order_book.ml` - 86%  
(With the in-memory implementation, it was 92.16%.)