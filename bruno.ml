open Coinbase

let () =
  let accounts = match (Lwt_main.run get_accounts) with
  | Result.Ok l -> l
  | Result.Error e -> print_endline e; [] in
  List.iter (fun (account: account) -> 
    let () = print_endline (show_account account) in
    let result = Lwt_main.run (get_account_history account.id) in 
    match result with 
    | Result.Ok l -> print_endline (show_account_history_list l)
    | Result.Error e -> print_endline ("ERROR: " ^ e)
  ) accounts

let () =
  let products = Lwt_main.run get_products in
  match products with 
  | Result.Ok p -> print_endline (show_product_list p)
  | Result.Error e -> print_endline ("ERROR: " ^ e)

let () =
  let btc_gbp = Lwt_main.run (get_product "BTC-GBP") in
  match btc_gbp with 
  | Result.Ok p -> print_endline (show_product p)
  | Result.Error e -> print_endline ("ERROR: " ^ e)

let () = 
  print_endline "Goodbye, World!"
