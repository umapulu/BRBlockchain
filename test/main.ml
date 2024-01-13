open OUnit2
open Brb

(* Testing Plan: For testing, we utilized our terminal interface, utop, and the
   main.ml file within the testing folder to test the functionality of our
   modules. In main.ml, we designed approximately two OUnit tests for each
   function in each module. . Test cases were developed with a combination of
   black box and glass box testing. We started with black box testing to account
   for general cases, and then utilized glass box testing to test edge cases and
   cover execution paths. We also manually tested some of the functionality
   using utop and our terminal interface. The majority of the manual testing was
   a result of testing out the functions via our terminal interface. To verify
   that all the functionality was present, we selected each possible path and
   ensured that all outputs were correct in the terminal. Our testing approach
   demonstrates correctness as we look at the functionality from a client facing
   side, ensuring that the actions they take and their results reflect in the
   outputs of the functions. *)

(******************************************************************************)
(*blockchain.ml tests*)
(******************************************************************************)
let one = Blockchain.(add_block "deepa" [] empty)

let multi =
  Blockchain.(
    empty |> add_block "shuyu" [] |> add_block "deepa" [] |> add_block "dee" []
    |> add_block "kate" [] |> add_block "annie" [])

let blockchain_tests =
  [
    ( "empty blockchain (size 0)" >:: fun _ ->
      assert_equal 0 (Blockchain.get_i Blockchain.empty) );
    ( "empty blockchain (empty list)" >:: fun _ ->
      assert_equal [] (Blockchain.get_chain Blockchain.empty) );
    ( "blockchain with one element (size 1)" >:: fun _ ->
      assert_equal 1 (Blockchain.get_i one) );
    ( "blockchain with one element (list)" >:: fun _ ->
      assert_equal
        [
          {
            Brb.Block.data = "deepa";
            hash = "000051577eec2509de77568195349d57";
            nonce = 26909;
            index = 0;
            previous_hash = "";
            timestamp = 0;
            transactions = [];
          };
        ]
        (Blockchain.get_chain one) );
    ( "blockchain with one element (last element)" >:: fun _ ->
      assert_equal
        {
          Brb.Block.data = "deepa";
          hash = "000051577eec2509de77568195349d57";
          nonce = 26909;
          index = 0;
          previous_hash = "";
          timestamp = 0;
          transactions = [];
        }
        (Blockchain.get_last_block one) );
    ( "blockchain with two elements (size 2)" >:: fun _ ->
      assert_equal 5 (Blockchain.get_i multi) );
    ( "blockchain with multiple elements (list)" >:: fun _ ->
      assert_equal
        [
          {
            Brb.Block.data = "shuyu";
            hash = "00009cd8cf3be54475059b12f836c8fc";
            nonce = 57474;
            index = 0;
            previous_hash = "";
            timestamp = 0;
            transactions = [];
          };
          {
            Brb.Block.data = "deepa";
            hash = "0000b73d3a491a4ed1af03df0c2c6959";
            nonce = 130740;
            index = 1;
            previous_hash = "00009cd8cf3be54475059b12f836c8fc";
            timestamp = 0;
            transactions = [];
          };
          {
            Brb.Block.data = "dee";
            hash = "0000b9e73dd01cd069de490ce58fa749";
            nonce = 46030;
            index = 2;
            previous_hash = "0000b73d3a491a4ed1af03df0c2c6959";
            timestamp = 0;
            transactions = [];
          };
          {
            Brb.Block.data = "kate";
            hash = "0000042ce1b418e0d4f9e98574aa629a";
            nonce = 46168;
            index = 3;
            previous_hash = "0000b9e73dd01cd069de490ce58fa749";
            timestamp = 0;
            transactions = [];
          };
          {
            Brb.Block.data = "annie";
            hash = "000081c59d624572149db851e9355c3b";
            nonce = 72872;
            index = 4;
            previous_hash = "0000042ce1b418e0d4f9e98574aa629a";
            timestamp = 0;
            transactions = [];
          };
        ]
        (Blockchain.get_chain multi) );
    ( "blockchain with multiple elements (last element)" >:: fun _ ->
      assert_equal
        {
          Brb.Block.data = "annie";
          hash = "000081c59d624572149db851e9355c3b";
          nonce = 72872;
          index = 4;
          previous_hash = "0000042ce1b418e0d4f9e98574aa629a";
          timestamp = 0;
          transactions = [];
        }
        (Blockchain.get_last_block multi) );
  ]

(******************************************************************************)
(*block.ml tests*)
(******************************************************************************)
let block_tests =
  [
    ( "generate_hash (test)" >:: fun _ ->
      assert_equal "098f6bcd4621d373cade4e832627b4f6"
        (Block.generate_hash "test") );
    ( "generate_hash (3.1415)" >:: fun _ ->
      assert_equal "63e1f04640e83605c1d177544a5a0488"
        (Block.generate_hash "3.1415") );
    ( "generate_hash (MS #2 Rubric: Milestone)" >:: fun _ ->
      assert_equal "a34ea5370b29aa7ee665f76f524d1ffd"
        (Block.generate_hash
           "4 points: demo. If at least one team member is present, the system \
            is running, and new functionality of clear value to users is being \
            demoed, you get all 4 points. If the functionality is visibly \
            buggy (as opposed to incomplete) or there is no clear value, your \
            PG could deduct 1 to 3 points. If no one is present, or if you \
            don't have the demo within the required time, you won't get the \
            points.") );
    ( "generate_hash (42)" >:: fun _ ->
      assert_equal "a1d0c6e83f027327d8461063f4ac58a6" (Block.generate_hash "42")
    );
    ( "generate_hash (empty string)" >:: fun _ ->
      assert_equal "d41d8cd98f00b204e9800998ecf8427e" (Block.generate_hash "")
    );
    ( "make_block ()" >:: fun _ ->
      assert_equal
        {
          Block.data = "";
          hash = "00003e3b9e5336685200ae85d21b4f5e";
          nonce = 5329;
          index = 0;
          previous_hash = "";
          timestamp = 0;
          transactions = [];
        }
        (Block.make_block "" []) );
    ( "make_block (3.1415)" >:: fun _ ->
      assert_equal
        {
          Block.data = "3.1415";
          hash = "0000cde29f2938208b7378e81332d39a";
          nonce = 104529;
          index = 0;
          previous_hash = "";
          timestamp = 0;
          transactions = [];
        }
        (Block.make_block "3.1415" []) );
    ( "make_block (annie)" >:: fun _ ->
      assert_equal
        {
          Block.data = "annie";
          hash = "0000177f8ebd36039404beafa0fdf421";
          nonce = 66986;
          index = 0;
          previous_hash = "";
          timestamp = 0;
          transactions = [];
        }
        (Block.make_block "annie" []) );
    ( "make_block (shuyu)" >:: fun _ ->
      assert_equal
        {
          Block.data = "shuyu";
          hash = "00009cd8cf3be54475059b12f836c8fc";
          nonce = 57474;
          index = 0;
          previous_hash = "";
          timestamp = 0;
          transactions = [];
        }
        (Block.make_block "shuyu" []) );
    ( "make_block (club goin up on a tuesday)" >:: fun _ ->
      assert_equal
        {
          Block.data = "club goin up on a tuesday";
          hash = "0000d5446d7e2008060ba5125a6eaec1";
          nonce = 78198;
          index = 0;
          previous_hash = "";
          timestamp = 0;
          transactions = [];
        }
        (Block.make_block "club goin up on a tuesday" []) );
    ( "count_zeros (singleton zero hash, 0 diff)" >:: fun _ ->
      assert_equal true (Block.count_zeros "0" 0) );
    ( "count_zeros (\"annie\" hash, 1 diff)" >:: fun _ ->
      assert_equal false
        (let hash = Block.generate_hash "annie" in
         Block.count_zeros hash 1) );
    ( "count_zeros (hash of all zeros, 3 diff)" >:: fun _ ->
      assert_equal true (Block.count_zeros "00000" 3) );
  ]

(******************************************************************************)
(*wallet.ml tests*)
(******************************************************************************)
let wallet_tests =
  [
    ( "create (new wallet)" >:: fun _ ->
      assert_equal
        {
          Wallet.name = "dee";
          balance = 0.;
          tokens = Token.create_token "CamelCoin";
          transactions = [];
        }
        (Wallet.create "dee") );
    ( "create (no name wallet)" >:: fun _ ->
      assert_equal
        {
          Wallet.name = "";
          balance = 0.;
          tokens = Token.create_token "CamelCoin";
          transactions = [];
        }
        (Wallet.create "") );
    ( "deposit (1 unit into wallet)" >:: fun _ ->
      assert_equal 1.0
        (let w = Wallet.create "" in
         Wallet.deposit w 1.;
         w.balance) );
    ( "deposit (0 unit into wallet)" >:: fun _ ->
      assert_equal 0.
        (let w = Wallet.create "" in
         Wallet.deposit w 0.;
         w.balance) );
    ( "withdraw (1 unit from wallet w/ 1 unit yields empty wallet)" >:: fun _ ->
      assert_equal 0.
        (let w = Wallet.create "" in
         Wallet.deposit w 1.;
         Wallet.withdraw w 1.;
         w.balance) );
    ( "withdraw (cannot withdrawl more than balance)" >:: fun _ ->
      assert_raises
        (Wallet.InsufficientBalance "\n Insufficient balance in account \n")
        (fun () ->
          let w = Wallet.create "" in
          Wallet.deposit w 2.;
          Wallet.withdraw w 3.) );
    ( "withdraw (new wallet balance is old balance minus withdraw)" >:: fun _ ->
      assert_equal 1.
        (let w = Wallet.create "" in
         Wallet.deposit w 3.;
         Wallet.withdraw w 2.;
         w.balance) );
    ( "transfer (empty transfer)" >:: fun _ ->
      assert_equal (3., 3.)
        (let x, y = (Wallet.create "x", Wallet.create "y") in
         Wallet.deposit x 3.;
         Wallet.deposit y 3.;
         Wallet.transfer x y 0.;
         (x.balance, y.balance)) );
    ( "transfer (transfer between two wallets affects bal of both)" >:: fun _ ->
      assert_equal (1., 5.)
        (let x, y = (Wallet.create "x", Wallet.create "y") in
         Wallet.deposit x 3.;
         Wallet.deposit y 3.;
         Wallet.transfer x y 2.;
         (x.balance, y.balance)) );
    ( "transfer (transfer cannot leave sender negative)" >:: fun _ ->
      assert_raises
        (Wallet.InsufficientBalance "\n Insufficient balance in account \n")
        (fun () ->
          let x, y = (Wallet.create "x", Wallet.create "y") in
          Wallet.deposit x 3.;
          Wallet.deposit y 3.;
          Wallet.transfer x y 4.) );
    ( "transaction list - should be empty without any transactions" >:: fun _ ->
      assert_equal
        ( {
            Wallet.name = "x";
            balance = 0.;
            tokens = { Brb.Token.symbol = "CamelCoin"; supply = 0. };
            transactions = [];
          },
          {
            Wallet.name = "y";
            balance = 0.;
            tokens = { Brb.Token.symbol = "CamelCoin"; supply = 0. };
            transactions = [];
          } )
        (let x, y = (Wallet.create "x", Wallet.create "y") in
         (x, y)) );
    ( "transaction list - multiple transactions in different steps should \
       reflect in transactions"
    >:: fun _ ->
      assert_equal
        ( {
            Wallet.name = "x";
            balance = 5.;
            tokens = { Token.symbol = "CamelCoin"; supply = 0. };
            transactions =
              [
                CoinTransaction
                  { sender = "x"; recipient = "External"; amount = 2. };
                CoinTransaction
                  { sender = "External"; recipient = "x"; amount = 4. };
                CoinTransaction
                  { sender = "External"; recipient = "x"; amount = 3. };
              ];
          },
          {
            Wallet.name = "y";
            balance = 13.;
            tokens = { Token.symbol = "CamelCoin"; supply = 0. };
            transactions =
              [
                CoinTransaction
                  { sender = "External"; recipient = "y"; amount = 10. };
                CoinTransaction
                  { sender = "External"; recipient = "y"; amount = 3. };
              ];
          } )
        (let x, y = (Wallet.create "x", Wallet.create "y") in
         Wallet.deposit x 3.;
         Wallet.deposit y 3.;
         Wallet.deposit x 4.;
         Wallet.withdraw x 2.;
         Wallet.deposit y 10.;
         (x, y)) );
    ( "transaction list - transfers should be reflected in both wallet's \
       transactions"
    >:: fun _ ->
      assert_equal
        ( {
            Wallet.name = "x";
            balance = 75.;
            tokens = { Token.symbol = "CamelCoin"; supply = 0. };
            transactions =
              [
                CoinTransaction { sender = "y"; recipient = "x"; amount = 15. };
                CoinTransaction { sender = "x"; recipient = "y"; amount = 40. };
                CoinTransaction
                  { sender = "External"; recipient = "x"; amount = 100. };
              ];
          },
          {
            Wallet.name = "y";
            balance = 54.;
            tokens = { Token.symbol = "CamelCoin"; supply = 0. };
            transactions =
              [
                CoinTransaction { sender = "y"; recipient = "x"; amount = 15. };
                CoinTransaction { sender = "x"; recipient = "y"; amount = 40. };
                CoinTransaction
                  { sender = "External"; recipient = "y"; amount = 29. };
              ];
          } )
        (let x, y = (Wallet.create "x", Wallet.create "y") in
         Wallet.deposit x 100.;
         Wallet.deposit y 29.;
         Wallet.transfer x y 40.;
         Wallet.transfer y x 15.;
         (x, y)) );
    ( "deposit, withdraw, and transfer tests" >:: fun _ ->
      let wallet1 = Wallet.create "Annie" in
      let wallet2 = Wallet.create "Kate" in
      let amount = 100. in

      Wallet.deposit wallet1 amount;
      assert_equal amount (Wallet.get_balance wallet1);

      Wallet.withdraw wallet1 50.;
      assert_equal 50. (Wallet.get_balance wallet1);

      assert_raises
        (Wallet.InsufficientBalance "\n Insufficient balance in account \n")
        (fun () -> Wallet.withdraw wallet1 60.);

      Wallet.transfer wallet1 wallet2 25.;
      assert_equal 25. (Wallet.get_balance wallet1);
      assert_equal 25. (Wallet.get_balance wallet2);

      assert_raises
        (Wallet.InsufficientBalance "\n Insufficient balance in account \n")
        (fun () -> Wallet.transfer wallet1 wallet2 30.) );
    ( "giving tokens should increase tokens in a wallet" >:: fun _ ->
      assert_equal
        ( {
            Wallet.name = "x";
            balance = 0.;
            tokens = { symbol = "CamelCoin"; supply = 40. };
            transactions =
              [
                TokenTransaction
                  {
                    sender = "External";
                    recipient = "x";
                    amount = 40.;
                    token = { symbol = "CamelCoin"; supply = 40. };
                  };
              ];
          },
          {
            Wallet.name = "y";
            balance = 0.;
            tokens = { symbol = "CamelCoin"; supply = 100. };
            transactions =
              [
                TokenTransaction
                  {
                    sender = "External";
                    recipient = "y";
                    amount = 100.;
                    token = { symbol = "CamelCoin"; supply = 100. };
                  };
              ];
          } )
        (let x, y = (Wallet.create "x", Wallet.create "y") in
         Wallet.give_token x { Token.symbol = "CamelCoin"; supply = 40. };
         Wallet.give_token y { Token.symbol = "CamelCoin"; supply = 100. };
         (x, y)) );
    ( "transfering tokens between wallets" >:: fun _ ->
      assert_equal
        ( {
            Wallet.name = "x";
            balance = 0.;
            tokens = { symbol = "CamelCoin"; supply = 52. };
            transactions =
              [
                TokenTransaction
                  {
                    sender = "External";
                    recipient = "x";
                    amount = 40.;
                    token = { symbol = "CamelCoin"; supply = 52. };
                  };
              ];
          },
          {
            Wallet.name = "y";
            balance = 0.;
            tokens = { symbol = "CamelCoin"; supply = 88. };
            transactions =
              [
                TokenTransaction
                  {
                    sender = "External";
                    recipient = "y";
                    amount = 100.;
                    token = { symbol = "CamelCoin"; supply = 88. };
                  };
              ];
          } )
        (let x, y = (Wallet.create "x", Wallet.create "y") in
         Wallet.give_token x { Token.symbol = "CamelCoin"; supply = 40. };
         Wallet.give_token y { Token.symbol = "CamelCoin"; supply = 100. };
         Token.transfer_tokens y.tokens x.tokens 29.;
         Token.transfer_tokens x.tokens y.tokens 17.;
         (x, y)) );
    ( "transfering tokens and currency between wallets - should not impact \
       each other"
    >:: fun _ ->
      assert_equal
        ( {
            Wallet.name = "x";
            balance = 10.;
            tokens = { symbol = "CamelCoin"; supply = 52. };
            transactions =
              [
                CoinTransaction
                  { sender = "External"; recipient = "x"; amount = 10. };
                TokenTransaction
                  {
                    sender = "External";
                    recipient = "x";
                    amount = 40.;
                    token = { symbol = "CamelCoin"; supply = 52. };
                  };
              ];
          },
          {
            Wallet.name = "y";
            balance = 20.;
            tokens = { symbol = "CamelCoin"; supply = 88. };
            transactions =
              [
                CoinTransaction { sender = "y"; recipient = "z"; amount = 80. };
                CoinTransaction
                  { sender = "External"; recipient = "y"; amount = 100. };
                TokenTransaction
                  {
                    sender = "External";
                    recipient = "y";
                    amount = 100.;
                    token = { symbol = "CamelCoin"; supply = 88. };
                  };
              ];
          },
          {
            Wallet.name = "z";
            balance = 60.;
            tokens = { Brb.Token.symbol = "CamelCoin"; supply = 0. };
            transactions =
              [
                CoinTransaction
                  { sender = "z"; recipient = "External"; amount = 20. };
                CoinTransaction { sender = "y"; recipient = "z"; amount = 80. };
              ];
          } )
        (let x, y, z =
           (Wallet.create "x", Wallet.create "y", Wallet.create "z")
         in
         Wallet.give_token x { Token.symbol = "CamelCoin"; supply = 40. };
         Wallet.give_token y { Token.symbol = "CamelCoin"; supply = 100. };
         Wallet.deposit x 10.;
         Wallet.deposit y 100.;
         Wallet.transfer y z 80.;
         Token.transfer_tokens y.tokens x.tokens 29.;
         Token.transfer_tokens x.tokens y.tokens 17.;
         Wallet.withdraw z 20.;
         (x, y, z)) );
  ]

(******************************************************************************)
(*walletList.ml tests*)
(******************************************************************************)
let wallet_list_tests =
  [
    ( "new_wallet (add wallet to empty list)" >:: fun _ ->
      assert_equal
        [
          {
            Wallet.name = "dee";
            balance = 0.;
            tokens = Token.create_token "CamelCoin";
            transactions = [];
          };
        ]
        (WalletList.empty |> WalletList.new_wallet "dee") );
    ( "new_wallet (add wallet to empty list)" >:: fun _ ->
      assert_equal
        [
          {
            Wallet.name = "kate";
            balance = 0.;
            tokens = Token.create_token "CamelCoin";
            transactions = [];
          };
          {
            Wallet.name = "deepa";
            balance = 0.;
            tokens = Token.create_token "CamelCoin";
            transactions = [];
          };
          {
            Wallet.name = "annie";
            balance = 0.;
            tokens = Token.create_token "CamelCoin";
            transactions = [];
          };
          {
            Wallet.name = "dee";
            balance = 0.;
            tokens = Token.create_token "CamelCoin";
            transactions = [];
          };
        ]
        (WalletList.empty
        |> WalletList.new_wallet "dee"
        |> WalletList.new_wallet "annie"
        |> WalletList.new_wallet "deepa"
        |> WalletList.new_wallet "kate") );
    ( "find_wallet (find_wallet on empty WalletList always returns None)"
    >:: fun _ -> assert_equal None WalletList.(empty |> find_wallet "shuyu") );
    ( "find_wallet (find_wallet on list with wallet should return wallet option)"
    >:: fun _ ->
      assert_equal
        (Some
           {
             Wallet.name = "deepa";
             balance = 0.;
             tokens = Token.create_token "CamelCoin";
             transactions = [];
           })
        (WalletList.empty
        |> WalletList.new_wallet "dee"
        |> WalletList.new_wallet "annie"
        |> WalletList.new_wallet "deepa"
        |> WalletList.new_wallet "kate"
        |> WalletList.find_wallet "deepa") );
    ( "finding wallet and adding funds" >:: fun _ ->
      assert_equal
        (Some
           {
             Wallet.name = "deepa";
             balance = 10.;
             tokens = Token.create_token "CamelCoin";
             transactions =
               [
                 Brb.Wallet.CoinTransaction
                   { sender = "External"; recipient = "deepa"; amount = 10. };
               ];
           })
        (let wlst =
           WalletList.empty
           |> WalletList.new_wallet "dee"
           |> WalletList.new_wallet "annie"
           |> WalletList.new_wallet "deepa"
           |> WalletList.new_wallet "kate"
         in
         match WalletList.find_wallet "deepa" wlst with
         | Some w ->
             Wallet.deposit w 10.;
             WalletList.find_wallet "deepa" wlst
         | None -> failwith "help") );
    ( "finding wallets - deposit, tranfer, and withdraw coins" >:: fun _ ->
      assert_equal
        [
          Some
            {
              Wallet.name = "deepa";
              balance = 5.;
              tokens = { symbol = "CamelCoin"; supply = 0. };
              transactions =
                [
                  CoinTransaction
                    { sender = "deepa"; recipient = "kate"; amount = 5. };
                  CoinTransaction
                    { sender = "External"; recipient = "deepa"; amount = 10. };
                ];
            };
          Some
            {
              Wallet.name = "dee";
              balance = 6.;
              tokens = { symbol = "CamelCoin"; supply = 0. };
              transactions =
                [
                  CoinTransaction
                    { sender = "dee"; recipient = "External"; amount = 6. };
                  CoinTransaction
                    { sender = "External"; recipient = "dee"; amount = 12. };
                ];
            };
          Some
            {
              Wallet.name = "kate";
              balance = 5.;
              tokens = { symbol = "CamelCoin"; supply = 0. };
              transactions =
                [
                  CoinTransaction
                    { sender = "deepa"; recipient = "kate"; amount = 5. };
                ];
            };
        ]
        (let wlst =
           WalletList.empty
           |> WalletList.new_wallet "dee"
           |> WalletList.new_wallet "annie"
           |> WalletList.new_wallet "deepa"
           |> WalletList.new_wallet "kate"
         in
         let deepa =
           match WalletList.find_wallet "deepa" wlst with
           | Some w ->
               Wallet.deposit w 10.;
               WalletList.find_wallet "deepa" wlst
           | None -> failwith "help"
         in
         let dee =
           match WalletList.find_wallet "dee" wlst with
           | Some w ->
               Wallet.deposit w 12.;
               WalletList.find_wallet "dee" wlst
           | None -> failwith "help"
         in
         let kate =
           match (WalletList.find_wallet "kate" wlst, deepa, dee) with
           | Some k, Some dp, Some de ->
               Wallet.transfer dp k 5.;
               Wallet.withdraw de 6.;
               WalletList.find_wallet "kate" wlst
           | _ -> failwith "help"
         in
         [ deepa; dee; kate ]) );
    ( "Adding after altering wallets should not change previous wallets"
    >:: fun _ ->
      assert_equal
        (Some
           {
             Wallet.name = "deepa";
             balance = 10.;
             tokens = Token.create_token "CamelCoin";
             transactions =
               [
                 Brb.Wallet.CoinTransaction
                   { sender = "External"; recipient = "deepa"; amount = 10. };
               ];
           })
        (let wlst =
           WalletList.empty
           |> WalletList.new_wallet "dee"
           |> WalletList.new_wallet "annie"
           |> WalletList.new_wallet "deepa"
           |> WalletList.new_wallet "kate"
         in
         match WalletList.find_wallet "deepa" wlst with
         | Some w ->
             Wallet.deposit w 10.;
             let wlst = WalletList.new_wallet "shuyu" wlst in
             WalletList.find_wallet "deepa" wlst
         | None -> failwith "help") );
    ( "finding wallets - token actions wih normal transactios" >:: fun _ ->
      assert_equal
        [
          {
            Wallet.name = "kate";
            balance = 9.;
            tokens = { symbol = "CamelCoin"; supply = 28. };
            transactions =
              [
                CoinTransaction
                  { sender = "External"; recipient = "kate"; amount = 9. };
                TokenTransaction
                  {
                    sender = "dee";
                    recipient = "kate";
                    amount = 28.;
                    token = { Brb.Token.symbol = "CamelCoin"; supply = 101. };
                  };
              ];
          };
          {
            Wallet.name = "deepa";
            balance = 105.;
            tokens = { symbol = "CamelCoin"; supply = 101. };
            transactions =
              [
                CoinTransaction
                  { sender = "deepa"; recipient = "dee"; amount = 5. };
                CoinTransaction
                  { sender = "deepa"; recipient = "External"; amount = 89. };
                CoinTransaction
                  { sender = "External"; recipient = "deepa"; amount = 199. };
                TokenTransaction
                  {
                    sender = "External";
                    recipient = "deepa";
                    amount = 40.;
                    token = { symbol = "CamelCoin"; supply = 101. };
                  };
              ];
          };
          {
            Wallet.name = "annie";
            balance = 0.;
            tokens = { symbol = "CamelCoin"; supply = 0. };
            transactions = [];
          };
          {
            Wallet.name = "dee";
            balance = 14.;
            tokens = { symbol = "CamelCoin"; supply = 101. };
            transactions =
              [
                TokenTransaction
                  {
                    sender = "dee";
                    recipient = "kate";
                    amount = 28.;
                    token = { symbol = "CamelCoin"; supply = 101. };
                  };
                CoinTransaction
                  { sender = "External"; recipient = "dee"; amount = 9. };
                CoinTransaction
                  { sender = "deepa"; recipient = "dee"; amount = 5. };
                TokenTransaction
                  {
                    sender = "External";
                    recipient = "dee";
                    amount = 129.;
                    token = { symbol = "CamelCoin"; supply = 101. };
                  };
              ];
          };
        ]
        (let wlst =
           WalletList.empty
           |> WalletList.new_wallet "dee"
           |> WalletList.new_wallet "annie"
           |> WalletList.new_wallet "deepa"
           |> WalletList.new_wallet "kate"
         in
         let token = Token.create_token "CamelCoin" in
         let deepa =
           match WalletList.find_wallet "deepa" wlst with
           | Some w ->
               Token.mint_tokens token 40.;
               Wallet.give_token w token;
               Wallet.deposit w 199.;
               Wallet.withdraw w 89.;
               WalletList.find_wallet "deepa" wlst
           | None -> failwith "help"
         in
         let dee =
           match (WalletList.find_wallet "dee" wlst, deepa) with
           | Some w, Some dp ->
               Token.mint_tokens token 89.;
               Wallet.give_token w token;
               Wallet.transfer dp w 5.;
               Wallet.deposit w 9.;
               WalletList.find_wallet "dee" wlst
           | _ -> failwith "help"
         in
         let _ =
           match (WalletList.find_wallet "kate" wlst, deepa, dee) with
           | Some k, Some dp, Some de ->
               Wallet.transfer_token de k 28. token;
               Wallet.deposit k 9.;
               WalletList.find_wallet "kate" wlst
           | _ -> failwith "help"
         in
         wlst) );
  ]

(******************************************************************************)
(*tokens.ml tests*)
(******************************************************************************)

let token_tests =
  [
    ( "create a new token" >:: fun _ ->
      assert_equal
        { Token.symbol = "deepa"; supply = 0. }
        (Token.create_token "deepa") );
    ( "create a new token (different symbol)" >:: fun _ ->
      assert_equal
        { Token.symbol = "dee"; supply = 0. }
        (Token.create_token "dee") );
    ( "minting new token with positive amount" >:: fun _ ->
      assert_equal
        { Token.symbol = "deepa"; supply = 10. }
        (let temp = Token.create_token "deepa" in
         Token.mint_tokens temp 10.0;
         temp) );
    ( "minting new token with negative amount" >:: fun _ ->
      assert_equal
        { Token.symbol = "deepa"; supply = 0. }
        (let temp = Token.create_token "deepa" in
         Token.mint_tokens temp (-10.0);
         temp) );
    ( "normal transfer (sender amount should decrease)" >:: fun _ ->
      assert_equal
        { Token.symbol = "deepa"; supply = 5. }
        (let sender = Token.create_token "deepa" in
         let recipient = Token.create_token "dee" in
         Token.mint_tokens sender 10.0;
         Token.transfer_tokens sender recipient 5.0;
         sender) );
    ( "normal transfer (recipient amount should increase)" >:: fun _ ->
      assert_equal
        { Token.symbol = "dee"; supply = 5. }
        (let sender = Token.create_token "deepa" in
         let recipient = Token.create_token "dee" in
         Token.mint_tokens sender 10.0;
         Token.transfer_tokens sender recipient 5.0;
         recipient) );
    ( "attempt transfer too much(sender amount should not change)" >:: fun _ ->
      assert_equal
        { Token.symbol = "deepa"; supply = 10. }
        (let sender = Token.create_token "deepa" in
         let recipient = Token.create_token "dee" in
         Token.mint_tokens sender 10.0;
         Token.transfer_tokens sender recipient 15.0;
         sender) );
    ( "attempt transfer too much(recipient amount should not change)"
    >:: fun _ ->
      assert_equal
        { Token.symbol = "dee"; supply = 0. }
        (let sender = Token.create_token "deepa" in
         let recipient = Token.create_token "dee" in
         Token.mint_tokens sender 10.0;
         Token.transfer_tokens sender recipient 15.0;
         recipient) );
  ]

(******************************************************************************)
(*json tests*)
(******************************************************************************)
let some_wallets =
  WalletList.empty
  |> WalletList.new_wallet "dee"
  |> WalletList.new_wallet "annie"
  |> WalletList.new_wallet "deepa"
  |> WalletList.new_wallet "kate"

let no_wallets = WalletList.empty

let json_tests =
  [
    ( "sending a blockchain to a file" >:: fun _ ->
      assert_equal ()
        (Blockchain.send_to_file "resources/chain.json"
           (Blockchain.to_json multi)) );
    ( "reading a blockchain from a file" >:: fun _ ->
      assert_equal multi (Blockchain.read_from_json_file "resources/chain.json")
    );
    ( "sending another blockchain to a file" >:: fun _ ->
      assert_equal ()
        (Blockchain.send_to_file "resources/chain.json" (Blockchain.to_json one))
    );
    ( "reading the other blockchain from a file" >:: fun _ ->
      assert_equal one (Blockchain.read_from_json_file "resources/chain.json")
    );
    ( "sending a wallet list to a file" >:: fun _ ->
      assert_equal ()
        (WalletList.send_to_file "resources/wallets.json"
           (WalletList.to_json some_wallets)) );
    ( "reading a wallet list from a file" >:: fun _ ->
      assert_equal some_wallets
        (WalletList.read_from_json_file "resources/wallets.json") );
    ( "sending another wallet list to a file" >:: fun _ ->
      assert_equal ()
        (WalletList.send_to_file "resources/wallets.json"
           (WalletList.to_json no_wallets)) );
    ( "reading the other wallet list from a file" >:: fun _ ->
      assert_equal no_wallets
        (WalletList.read_from_json_file "resources/wallets.json") );
  ]

let transaction_tests =
  [
    ( "transfer tokens test" >:: fun _ ->
      let wallet1 = Wallet.create "Dee" in
      let wallet2 = Wallet.create "Deepa" in
      let token = Token.create_token "ExampleToken" in
      Token.mint_tokens token 1000.;
      Wallet.give_token wallet1 token;
      assert_equal 1000. (Token.get_supply wallet1.tokens);
      Wallet.transfer_token wallet1 wallet2 200. token;
      assert_equal 800. (Token.get_supply wallet1.tokens);
      assert_equal 200. (Token.get_supply wallet2.tokens);
      assert_equal 2 (List.length wallet1.transactions);
      assert_equal 1 (List.length wallet2.transactions) );
  ]

let suite =
  "test suite for CS 3110 Final Project"
  >::: List.flatten
         [
           blockchain_tests;
           block_tests;
           wallet_tests;
           wallet_list_tests;
           token_tests;
           json_tests;
           transaction_tests;
         ]

let () = run_test_tt_main suite
