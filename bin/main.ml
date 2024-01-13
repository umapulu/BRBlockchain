open Brb

let next ready =
  print_string
    "> What would you like to do? \n\
    \ 0: Generate hash\n\
    \ 1: Mine block \n\
    \ 2: Make blockchain \n\
    \ 3: Open wallet\n\
    \ 4: New wallet\n\
    \ 5: View ledger\n\
    \ 6: Exit\n\n\
    \ > ";
  read_line ()

let wallets = ref (WalletList.read_from_json_file "resources/wallets.json")
let chain = ref (Blockchain.read_from_json_file "resources/chain.json")

type ledger = { mutable ledger_list : Wallet.transaction list }

let ledger = { ledger_list = [] }

let rec wallet_action (w : Wallet.t) =
  print_endline
    ("> What would you like to do with " ^ w.name
   ^ "'s wallet? \n\
     \ 0: Make a deposit\n\
     \ 1: Make a withdrawal \n\
     \ 2: Make a transfer\n\
     \ 3: Exit wallet\n");
  print_string "> ";
  let action = read_line () in
  print_endline "";
  match action with
  | "0" -> wallet_action_deposit w
  | "1" -> wallet_action_withdrawl w
  | "2" -> wallet_action_transfer w
  | _ -> ()

and wallet_action_deposit w =
  print_endline "How much would you like to deposit?";
  print_string "> ";
  let amount = float_of_string (read_line ()) in
  Wallet.deposit w amount;
  print_endline
    ("The balance of " ^ w.name ^ "'s wallet is now "
   ^ string_of_float w.balance);
  print_endline "";
  ledger.ledger_list <-
    ledger.ledger_list @ Wallet.get_last_wallet_transaction w;
  wallet_action w

and wallet_action_withdrawl w =
  print_endline "How much would you like to withdrawl?";
  print_string "> ";
  let amount = float_of_string (read_line ()) in
  try
    Wallet.withdraw w amount;
    print_endline
      ("The balance of " ^ w.name ^ "'s wallet is now "
     ^ string_of_float w.balance);
    print_endline "";
    ledger.ledger_list <-
      ledger.ledger_list @ Wallet.get_last_wallet_transaction w;
    wallet_action w
  with Wallet.InsufficientBalance s ->
    print_endline s;
    wallet_action w

and wallet_action_transfer w =
  print_endline "Who would you like to transfer money to?";
  print_string "> ";
  let n = read_line () in
  let transfer = WalletList.find_wallet n !wallets in
  let () =
    match transfer with
    | None -> print_endline (n ^ "'s wallet does not exist\n")
    | Some x -> (
        print_endline "How much would you like to transfer?";
        print_string "> ";
        let amount = float_of_string (read_line ()) in
        try Wallet.transfer w x amount
        with Wallet.InsufficientBalance s ->
          print_endline s;
          wallet_action w)
  in
  print_endline
    ("The balance of " ^ w.name ^ "'s wallet is now "
   ^ string_of_float w.balance);
  print_endline "";
  ledger.ledger_list <-
    ledger.ledger_list @ Wallet.get_last_wallet_transaction w;
  wallet_action w

let continue x =
  print_endline "> Would you like to continue? (y/n)";
  print_string "> ";
  let c = read_line () in
  if c = "y" then (
    print_endline "";
    next true)
  else if c = "n" then begin
    WalletList.send_to_file "resources/wallets.json"
      (WalletList.to_json !wallets);
    Blockchain.send_to_file "resources/chain.json" (Blockchain.to_json !chain);
    let ledger_chain =
      Blockchain.add_block "ledger" ledger.ledger_list
        (Blockchain.read_from_json_file "resources/ledger.json")
    in
    Blockchain.send_to_file "resources/ledger.json"
      (Blockchain.to_json ledger_chain);
    "6"
  end
  else c

let make_blockchain n =
  let rec iterate num acc =
    if num = 0 then acc
    else (
      print_endline
        ("> what would you like the data of block #"
        ^ string_of_int (Blockchain.get_i acc)
        ^ " to be?");
      print_string "> ";
      let data = read_line () in
      print_endline "";
      iterate (num - 1) (Blockchain.add_block data [] acc))
  in
  print_endline "";
  let current = iterate n !chain in
  chain := current;
  Blockchain.send_to_file "resources/chain.json" (Blockchain.to_json current);

  print_endline (Blockchain.to_string current)

let rec select command =
  print_endline "       ,,    , ";
  print_endline "      || |  | | ";
  print_endline "      || '--' |";
  print_endline "  ,   || .----'";
  print_endline " | |  || |";
  print_endline " | '--'| |";
  print_endline " '----.| |                             _____";
  print_endline "      || |                            / /|\\ \\";
  print_endline "      || |     Authorizing request   | | | | |";
  print_endline "______||_|____________________________\\_\\|/_/__";
  print_endline "";
  match command with
  | "0" -> command_generate_hash ()
  | "1" -> command_mine_block ()
  | "2" -> command_make_blockchain ()
  | "3" -> command_open_wallet ()
  | "4" -> command_new_wallet ()
  | "5" -> command_view_ledger ()
  | "6" -> print_endline "> exiting...\n"
  | "" -> print_endline "> exiting...\n"
  | _ ->
      print_endline "> invalid action try again\n";
      select (continue true)

and command_generate_hash () =
  print_endline "> please enter data";
  print_string "> ";
  let data = read_line () in
  let hash = Block.generate_hash data in
  print_endline ("> " ^ hash);
  print_endline "";
  select (continue true)

and command_mine_block () =
  print_endline "> please enter data";
  print_string "> ";
  let data = read_line () in
  print_endline "> mining block...";
  print_endline "";
  let block = Block.make_block data [] in
  print_endline ("> Hash Value: " ^ block.hash);
  print_endline ("> Nonce: " ^ string_of_int block.nonce);
  print_endline "";
  select (continue true)

and command_make_blockchain () =
  print_endline "> how many blocks do you want to add to the chain?";
  print_string "> ";
  let number = int_of_string (read_line ()) in
  make_blockchain number;
  print_endline "";
  select (continue true)

and command_open_wallet () =
  print_endline "> who's wallet?";
  print_string "> ";
  let name = read_line () in
  print_endline ("> opening " ^ name ^ "'s wallet\n");
  let () =
    match WalletList.find_wallet name !wallets with
    | None ->
        print_endline "> Wallet with that name was not found";
        print_endline ""
    | Some x ->
        print_endline ("> Sucessfully opened " ^ x.name ^ "'s wallet");
        print_endline
          ("> " ^ x.name ^ "'s wallet has a balance of "
         ^ string_of_float x.balance);
        print_endline "";
        wallet_action x
  in
  select (continue true)

and command_new_wallet () =
  print_endline "> name of wallet?";
  print_string "> ";
  let name = read_line () in
  print_endline ("> creating " ^ name ^ "'s wallet\n");
  wallets := WalletList.new_wallet name !wallets;
  select (continue true)

and command_view_ledger () =
  print_endline "> viewing ledger...\n";
  print_endline (Block.transactions_to_string ledger.ledger_list);
  select (continue true)

(*********** command line interface ***********)
let () =
  print_endline "\nWelcome to Big Red Blockchain";
  print_endline "         _";
  print_endline "     .--' |";
  print_endline "     /___^ |     .--.";
  print_endline "        ) |    /     \\";
  print_endline "       /  |  /`      '.";
  print_endline "      |   '-'    /     \\";
  print_endline "      \\          |     |";
  print_endline "      \\    /          /";
  print_endline "       \\   /'----`   /";
  print_endline "        |||       \\ |";
  print_endline "        ((|       ((|";
  print_endline "        |||       |||";
  print_endline "_______//_(______//_(_______________";
  print_endline "";
  select (next true)
