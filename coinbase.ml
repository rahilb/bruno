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

type account_list = account list [@@deriving show, yojson]

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

let get_accounts =
  let path = "/accounts" in
  let req_body = "" in
  let _method = "GET" in
  let timestamp = int_of_float (Unix.time ()) |> string_of_int in 
  let pre_hash = timestamp ^ _method ^ path ^ req_body in
  let hmac_digest = hmac_sha256 ~key:api_params_from_env.secret pre_hash in 
  let headers = Header.init () in 
  let headers = Header.add headers "CB-ACCESS-KEY" api_params_from_env.key in
  let headers = Header.add headers "CB-ACCESS-PASSPHRASE" api_params_from_env.passphrase in
  let headers = Header.add headers "CB-ACCESS-TIMESTAMP" timestamp in
  let headers = Header.add headers "CB-ACCESS-SIGN" hmac_digest in
  let uri = Uri.of_string (api_params_from_env.api_host ^ path) in
  Client.get ~headers:headers uri >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  let json = Yojson.Safe.from_string body in 
  match code with 
  | 200 -> account_list_of_yojson json
  | _ -> Result.Error (Printf.sprintf "Unexpected response code: %d" code)
  
