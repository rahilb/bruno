include Digestif
include Base64
include Yojson

open Lwt
open Cohttp
open Cohttp_lwt_unix
open Sexplib.Std

type auth_params = {
  key : string ;
  passphrase : string [@opaque] ;
  secret : string ;
  api_host : string ;
} [@@deriving sexp]

type account = {
  id : string; 
  currency : string; 
  balance : string; 
  hold : string; 
  profile_id: string; 
  available : string; 
  trading_enabled : bool; 
} [@@deriving show, yojson]

type tx_type =
| Transfer   [@name "transfer"]
| Match [@name "match"]
| Fee [@name "fee"]
| Rebate [@name "rebate"]
| Conversion [@name "conversion"]
[@@deriving show, yojson]

type account_history = {
  id: string; 
  created_at: string;  
  amount: string; 
  balance: string; 
  typ: string [@key "type"]; 
} [@@deriving show, yojson {strict = false}]

type order_side =
| Buy   [@name "buy"]
| Sell [@name "sell"]
[@@deriving show, yojson]

type order = {
  id: string;
  price: string;
  size: string;
  product_id: string;
  side: string;
  stp: string;
  order_type: string [@key "type"];
  time_in_force: string;
  post_only: bool;
  created_at: string;
  fill_fees: string;
  filled_size: string;
  executed_value: string;
  status: string;
  settled: bool;
} [@@deriving show, yojson]

type product = {
  id: string;
  display_name: string;
  base_currency: string;
  quote_currency: string;
  base_increment: string;
  quote_increment: string;
  base_min_size: string;
  base_max_size: string;
  min_market_funds: string;
  max_market_funds: string;
  status: string;
  status_message: string;
  cancel_only: bool;
  limit_only: bool;
  post_only: bool;
  trading_disabled: bool;
} [@@deriving show, yojson {strict = false}]

type order_req = {
  side: string;
  product_id: string;
  price: string;
  size: string;
} [@@deriving show, yojson {strict = false}]

type account_list = account list [@@deriving show, yojson]
type product_list = product list [@@deriving show, yojson]
type account_history_list = account_history list [@@deriving show, yojson]

let host_from_env = match Sys.getenv_opt ("COINBASE_ENV") with 
  | Some "PROD" -> ""
  | _ -> "https://api-public.sandbox.pro.coinbase.com"

let api_params_from_env = 
  let get_env = fun k -> Sys.getenv ("COINBASE_" ^ k) in
  let (access_key, passphrase, secret) = ((get_env "API_KEY"), (get_env "PASSPHRASE"), (get_env "SECRET")) in 
  let secret = Base64.decode_exn secret in
  {key = access_key; passphrase = passphrase; secret = secret; api_host = host_from_env}  

let hmac_sha256 ~key v =
  Base64.encode_exn Digestif.SHA256.(hmac_string ~key v |> to_raw_string)

type http_method = 
  | GET
  | POST
  | DELETE
  [@@deriving show { with_path = false } ]

let http http_method path body unmarshaller =
  let body = match body with 
  | Some b -> b
  | None -> "" in
  let timestamp = int_of_float (Unix.time ()) |> string_of_int in 
  let pre_hash = timestamp ^ (show_http_method http_method) ^ path ^ body in
  let hmac_digest = hmac_sha256 ~key:api_params_from_env.secret pre_hash in 
  let headers = Header.init () in 
  let headers = Header.add headers "CB-ACCESS-KEY" api_params_from_env.key in
  let headers = Header.add headers "CB-ACCESS-PASSPHRASE" api_params_from_env.passphrase in
  let headers = Header.add headers "CB-ACCESS-TIMESTAMP" timestamp in
  let headers = Header.add headers "CB-ACCESS-SIGN" hmac_digest in
  let uri = Uri.of_string (api_params_from_env.api_host ^ path) in
  let result = match http_method with 
  | POST -> Client.post ~body:(Cohttp_lwt.Body.of_string body) ~headers uri
  | DELETE -> Client.delete ~headers uri
  | GET -> Client.get ~headers uri in  
  result >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  let json = Yojson.Safe.from_string body in 
  match code with 
  | 200 -> unmarshaller json
  | _ -> Result.Error (Printf.sprintf "Unexpected response code: %d" code)

let http_get path unmarshaller = 
  http GET path None unmarshaller

let http_del path = 
  let unit_unmarshall (t: Safe.t) = match t with 
  | _ -> Result.Ok () in
  http DELETE path None unit_unmarshall

let http_post ~path ~body ~unmarshaller = 
  http POST path body unmarshaller

let get_products = 
  http_get "/products" product_list_of_yojson

let get_account_history id = 
  http_get ("/accounts/" ^ id ^ "/ledger") account_history_list_of_yojson

let get_accounts = 
  http_get "/accounts" account_list_of_yojson

let get_account id = 
  http_get ("/account/" ^ id) account_of_yojson

let get_product id = 
  http_get ("/products/" ^ id) product_of_yojson

let create_order order_req = 
  http_post ~path:"/orders" ~body:(order_req_to_yojson order_req |> Yojson.Safe.to_string |> Some) ~unmarshaller:order_of_yojson

let cancel_order id = 
  http_del ("/orders/" ^ id)