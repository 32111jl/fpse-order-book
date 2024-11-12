# fpse-order-book


#### About
This project is a high-performance order book application developed in OCaml. It simulates the management and matching of buy/sell orders for a given financial asset using a matching engine that efficiently pairs millions of concurrent buy/sell orders based on price and time priority. The application supports market, limit, and margin orders, along with partially-filled orders and a bid-ask spread threshold to regulate trade execution. Most user interaction will occur through a command-line interface.


#### Libraries
Core - for basic data structures and utilities
Lwt - for asynchronous I/O
Dream (not yet determined) - for web interface/server

#### Installation and Setup
##### Pre-requisites
- OCaml, Core, Lwt

##### Building, Running the Application
- clone the repo
- compile the project - `dune build`
- run the application - `dune exec TBD`

#### Implementation Plan
11/13 - submit project design proposal
11/18 - finalize `mli`'s with Professor Smith and teaching team
12/1 - complete implementation (finish matching engine, determine account features, have most/all of order types and related logic complete)
12/4 - code checkpoint (debug and have initial testing to ensure basic functionality)
(week of) 12/9 - final project submission (complete testing, documentation, etc. as outlined by project guidelines)

#### What's Working, and What's Not?
- Working: N/A
- Not Working: N/A