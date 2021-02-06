include Coinbase

let () =
  let accounts = Result.get_ok (Lwt_main.run Coinbase.get_accounts) in
  print_endline (show_account_list accounts)

let () = 
  print_endline "Goodbye, World!"
