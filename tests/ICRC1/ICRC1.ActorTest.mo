import Array "mo:base/Array";
import Blob "mo:base/Blob";
import D "mo:base/Debug";
import Int "mo:base/Int";
import Iter "mo:base/Iter";
import Nat "mo:base/Nat";
import Nat8 "mo:base/Nat8";
import Nat32 "mo:base/Nat32";
import Nat64 "mo:base/Nat64";
import Opt "mo:base/Option";
import Principal "mo:base/Principal";
import Result "mo:base/Result";
import Text "mo:base/Text";
import Time "mo:base/Time";

import Fake "../fake";

import Vec "mo:vector";
import Star "mo:star/star";

import Itertools "mo:itertools/Iter";
import StableBuffer "mo:StableBuffer/StableBuffer";
import Sha256 "mo:sha2/Sha256";


import ActorSpec "../utils/ActorSpec";

import MigrationTypes = "../../src/ICRC1/migrations/types";

import ICRC1 "../../src/ICRC1";
import T "../../src/ICRC1/migrations/types";

import U "../../src/ICRC1/Utils";

module {

  let base_environment= {
    get_time = null;
    add_ledger_transaction = null;
    can_transfer = null;
    get_fee = null;
  };

  type TransferResult = MigrationTypes.Current.TransferResult;
  type Account = MigrationTypes.Current.Account;
  type Balance = MigrationTypes.Current.Balance;
  let Map = ICRC1.Map;
  let ahash = ICRC1.ahash;
  let Vector = ICRC1.Vector;

  let e8s = 100000000;


    public func test() : async ActorSpec.Group {
        D.print("in test");

        let {
            assertTrue;
            assertFalse;
            assertAllTrue;
            describe;
            it;
            skip;
            pending;
            run;
        } = ActorSpec;

        func add_decimals(n : Nat, decimals : Nat) : Nat {
            n * (10 ** decimals);
        };

        func mock_tx(to : T.Current.Account, index : Nat) : T.Current.Transaction {
            {
                burn = null;
                transfer = null;
                kind = "MINT";
                timestamp = 0;
                index;
                mint = ?{
                    to;
                    amount = index + 1;
                    memo = null;
                    created_at_time = null;
                };
            };
        };

        let canister : T.Current.Account = {
            owner = Principal.fromText("x4ocp-k7ot7-oiqws-rg7if-j4q2v-ewcel-2x6we-l2eqz-rfz3e-6di6e-jae");
            subaccount = null;
        };

        let user1 : T.Current.Account = {
            owner = Principal.fromText("prb4z-5pc7u-zdfqi-cgv7o-fdyqf-n6afm-xh6hz-v4bk4-kpg3y-rvgxf-iae");
            subaccount = null;
        };

        let user2 : T.Current.Account = {
            owner = Principal.fromText("ygyq4-mf2rf-qmcou-h24oc-qwqvv-gt6lp-ifvxd-zaw3i-celt7-blnoc-5ae");
            subaccount = null;
        };

        func txs_range(start : Nat, end : Nat) : [T.Current.Transaction] {
            Array.tabulate(
                (end - start) : Nat,
                func(i : Nat) : T.Current.Transaction {
                    mock_tx(user1, start + i);
                },
            );
        };

        func is_tx_equal(t1 : T.Current.Transaction, t2 : T.Current.Transaction) : Bool {
            { t1 with timestamp = 0 } == { t2 with timestamp = 0 };
        };

        func is_opt_tx_equal(t1 : ?T.Current.Transaction, t2 : ?T.Current.Transaction) : Bool {
            switch (t1, t2) {
                case (?t1, ?t2) {
                    is_tx_equal(t1, t2);
                };
                case (_, ?t2) { false };
                case (?t1, _) { false };
                case (_, _) { true };
            };
        };

        func are_txs_equal(t1 : [T.Current.Transaction], t2 : [T.Current.Transaction]) : Bool {
            Itertools.equal<T.Current.Transaction>(t1.vals(), t2.vals(), is_tx_equal);
        };

        func create_mints(icrc1 : ICRC1.ICRC1, minting_principal : Principal, n : Nat) : async () {
            for (i in Itertools.range(0, n)) {
                ignore await* icrc1.mint(
                    {
                        to = user1;
                        amount = i + 1;
                        memo = null;
                        created_at_time = null;
                    },
                    minting_principal,
                );
            };
        };

        let base_fee = 5 * (10 ** 8);
        let max_supply = 1_000_000_000 * (10 ** 8);

        let default_token_args : T.Current.InitArgs = {
            name = ?"Under-Collaterised Lending Tokens";
            symbol = ?"UCLTs";
            decimals = 8;
            fee = ?#Fixed(base_fee);
            max_supply = ?(max_supply);
            minting_account = ?canister;
            initial_balances = [];
            min_burn_amount = ?(10 * (10 ** 8));
            advanced_settings = null;
            local_transactions = [];
            metadata = null;
            recent_transactions = [];
            max_memo = null;
            fee_collector = null;
            permitted_drift = null;
            transaction_window = null;
            max_accounts = null;
            settle_to_accounts = null;
        };
        var test_time : Int = Time.now();

        let environment : T.Current.Environment = {
          get_time = ?(func () : Int {test_time});
          add_ledger_transaction = null;
          get_fee = null;
          can_transfer = null;
          can_transfer_async = null;
        };


        func get_icrc(args : ICRC1.InitArgs) : ICRC1.ICRC1{

          let token = ICRC1.init(ICRC1.initialState(), #v0_1_0(#id),?args, canister.owner);

          ICRC1.ICRC1(?token, canister.owner, environment);
        };

        func get_icrc_env(args : ICRC1.InitArgs, env : ICRC1.Environment) : ICRC1.ICRC1{

          let token = ICRC1.init(ICRC1.initialState(), #v0_1_0(#id),?args, canister.owner);

          ICRC1.ICRC1(?token, canister.owner, env);
        };

        let externalCanTransferFalseSync = func (trx: ICRC1.Value, trxtop: ?ICRC1.Value, notification: ICRC1.TransactionRequestNotification) : Result.Result<(trx: ICRC1.Value, trxtop: ?ICRC1.Value, notification: ICRC1.TransactionRequestNotification), Text> {

            switch(notification.kind){
              case(#transfer(val)){
                return #err("always false");
              };
              case(_){
                return #ok(trx, trxtop, notification);
              }
            }
            // This mock externalCanTransfer function always returns false,
            // indicating the transfer should not proceed.
            
        };

        let externalCanTransferFalseAsync = func (trx: ICRC1.Value, trxtop: ?ICRC1.Value, notification: ICRC1.TransactionRequestNotification) : async* Star.Star<(trx: ICRC1.Value, trxtop: ?ICRC1.Value, notification: ICRC1.TransactionRequestNotification), Text> {
            // This mock externalCanTransfer function always returns false,
            // indicating the transfer should not proceed.
            let fake = await Fake.Fake();
            switch(notification.kind){
              case(#transfer(val)){
                return #err(#awaited("always false"));
              };
              case(_){
                return #awaited(trx, trxtop, notification);
              }
            }
        };

        let externalCanTransferUpdateSync = func (trx: ICRC1.Value, trxtop: ?ICRC1.Value, notification: ICRC1.TransactionRequestNotification) : Result.Result<(trx: ICRC1.Value, trxtop: ?ICRC1.Value, notification: ICRC1.TransactionRequestNotification), Text> {
            let results = Vector.new<(Text,ICRC1.Value)>();
            switch(notification.kind){
              case(#transfer){};
              case(_){
                return #ok(trx,trxtop,notification);
              };
            };
            switch(trx){
              case(#Map(val)){
                for(thisItem in val.vals()){
                  if(thisItem.0 == "amt"){
                    Vector.add(results, ("amt", #Nat(2)));
                  } else {
                    Vector.add(results, thisItem);
                  };
                }
              };
              case(_) return #err("not a map");
            };

            return #ok(#Map(Vector.toArray(results)), trxtop, {notification with
              amount = 2;
            });
        };

        let externalCanTransferUpdateAsync = func (trx: ICRC1.Value, trxtop: ?ICRC1.Value, notification: ICRC1.TransactionRequestNotification) : async* Star.Star<(trx: ICRC1.Value, trxtop: ?ICRC1.Value, notification: ICRC1.TransactionRequestNotification), Text> {
            let fake = await Fake.Fake();
            switch(notification.kind){
              case(#transfer){};
              case(_){
                return #awaited(trx,trxtop,notification);
              };
            };
            let results = Vector.new<(Text,ICRC1.Value)>();
            switch(trx){
              case(#Map(val)){
                for(thisItem in val.vals()){
                  if(thisItem.0 == "amt"){
                    Vector.add(results, ("amt", #Nat(2)));
                  } else {
                    Vector.add(results, thisItem);
                  };
                }
              };
              case(_) return #err(#awaited("not a map"))
            };

            return #awaited(#Map(Vector.toArray(results)), trxtop, {notification with
              amount = 2;
            });
        };

        return describe(
            "ICRC1 Token Implementation Tessts",
            [
                it(
                    "init()",
                    do {
                        let icrc1  = get_icrc(default_token_args);

                        let minting_account = icrc1.minting_account();

                        // returns without trapping
                        assertAllTrue([
                            ?icrc1.name() == default_token_args.name,
                            ?icrc1.symbol() == default_token_args.symbol,
                            icrc1.decimals() == default_token_args.decimals,
                            icrc1.fee() == base_fee,
                            icrc1.max_supply() == default_token_args.max_supply,

                            minting_account.owner == canister.owner,
                            minting_account.subaccount == canister.subaccount,
                            icrc1.supported_standards() == [U.default_standard],
                            Vec.size(icrc1.get_local_transactions()) == 0,
                        ]);
                    },
                ),

                it(
                    "name()",
                    do {
                        D.print("testing name");
                        let icrc1 =  get_icrc(default_token_args);

                        assertTrue(
                            ?icrc1.name() == default_token_args.name,
                        );
                    },
                ),

                it(
                    "symbol()",
                    do {
                        D.print("testing symbol");
                        

                        let icrc1 =  get_icrc(default_token_args);

                        assertTrue(
                            ?icrc1.symbol() == default_token_args.symbol,
                        );
                    },
                ),

                it(
                    "decimals()",
                    do {
                        D.print("testing decimals");
                       

                        let icrc1 =  get_icrc(default_token_args);

                        assertTrue(
                            icrc1.decimals() == default_token_args.decimals,
                        );
                    },
                ),
                it(
                    "fee()",
                    do {
                        D.print("testing fee");

                        let icrc1 =  get_icrc(default_token_args);

                        let ?#Fixed(base_fee) = default_token_args.fee;

                        assertTrue(
                            icrc1.fee() == base_fee,
                        );
                    },
                ),
                it(
                    "minting_account()",
                    do {
                        D.print("testing minting_account");
                        

                        let icrc1 =  get_icrc(default_token_args);

                        let init_minting = icrc1.minting_account();

                        let ?base_minting = default_token_args.minting_account;

                        ignore assertTrue(
                            init_minting.owner == base_minting.owner,
                        );

                        assertTrue(
                            init_minting.subaccount == base_minting.subaccount,
                        );
                    },
                ),
                it(
                    "balance_of()",
                    do {
                        D.print("testing balance_of");

                        let icrc1 =  get_icrc(default_token_args);

                        for(thisItem in  [
                                (user1, 100),
                                (user2, 200),
                            ].vals()){
                          ignore await* icrc1.mint({
                            to = thisItem.0;
                            amount = thisItem.1;
                            memo = ?Text.encodeUtf8("init");
                            created_at_time = null;
                          }, icrc1.minting_account().owner)
                        };

                        assertAllTrue([
                            icrc1.balance_of(user1) == 100,
                            icrc1.balance_of(user2) == 200,
                        ]);
                    },
                ),
                it(
                    "total_supply()",
                    do {
                        D.print("testing total_supply");
                       

                        let icrc1 =  get_icrc(default_token_args);

                        for(thisItem in  [
                                (user1, 100),
                                (user2, 200),
                            ].vals()){
                          ignore await* icrc1.mint({
                            to = thisItem.0;
                            amount = thisItem.1;
                            memo = ?Text.encodeUtf8("init");
                            created_at_time = null;
                          }, icrc1.minting_account().owner)
                        };

                        assertTrue(
                            icrc1.total_supply() == 300,
                        );
                    },
                ),

                it(
                    "metadata()",
                    do {
                        D.print("testing metadata");

                        let icrc1 =  get_icrc(default_token_args);

                        assertTrue(
                            icrc1.metadata() == #Map([
                                ("icrc1:fee", #Nat(5 * (10 ** 8))),
                                ("icrc1:name", #Text(Opt.get<Text>(default_token_args.name, "yuck"))),
                                ("icrc1:symbol", #Text(Opt.get<Text>(default_token_args.symbol, "yuk"))),
                                ("icrc1:decimals", #Nat(Nat8.toNat(default_token_args.decimals))),
                            ]),
                        );
                    },
                ),

                it(
                    "supported_standards()",
                    do {
                        D.print("testing supported_standards");
                        
                        let icrc1 =  get_icrc(default_token_args);

                        assertTrue(
                            icrc1.supported_standards() == [{
                                name = "ICRC-1";
                                url = "https://github.com/dfinity/ICRC-1";
                            }],
                        );
                    },
                ),

                it(
                    "mint()",
                    do {
                        D.print("testing mint");
                       

                        let icrc1 = get_icrc(default_token_args);

                        let mint_args : T.Current.Mint = {
                            to = user1;
                            amount = 200 * (10 ** Nat8.toNat(default_token_args.decimals));
                            memo = null;
                            created_at_time = null;
                        };

                        let #trappable(res) = await* icrc1.mint(
                            mint_args,
                            canister.owner
                        );

                        assertAllTrue([
                            res == #Ok(0),
                            icrc1.balance_of( user1) == mint_args.amount,
                            icrc1.balance_of( canister) == 0,
                            icrc1.total_supply() == mint_args.amount,
                        ]);
                    },
                ),

                describe(
                    "burn()",
                    [
                        it(
                            "from funded account",
                            do {
                                D.print("testing burn");
                                let icrc1 = get_icrc(default_token_args);

                                D.print("have class");
                                let mint_args : T.Current.Mint = {
                                    to = user1;
                                    amount = 200 * (10 ** Nat8.toNat(default_token_args.decimals));
                                    memo = null;
                                    created_at_time = null;
                                };

                                D.print("minting");
                                ignore await* icrc1.mint(
                                    
                                    mint_args,
                                    canister.owner,
                                );

                                let burn_args : T.Current.BurnArgs = {
                                    from_subaccount = user1.subaccount;
                                    amount = 50 * (10 ** Nat8.toNat(default_token_args.decimals));
                                    memo = null;
                                    created_at_time = null;
                                };

                                let prev_balance = icrc1.balance_of(user1);
                                let prev_total_supply = icrc1.total_supply();

                                D.print("burnin");
                                let #trappable(res) = await* icrc1.burn( burn_args, user1.owner, false);
                                D.print("tests" # debug_show(prev_balance, burn_args, prev_total_supply));
                                assertAllTrue([
                                    res == #Ok(1),
                                    icrc1.balance_of(user1) == ((prev_balance - burn_args.amount) : Nat),
                                    icrc1.total_supply() == ((prev_total_supply - burn_args.amount) : Nat),
                                ]);
                            },
                        ),
                        it(
                            "from an empty account",
                            do {
                                D.print(" testing burn2");
                                let icrc1 = get_icrc(default_token_args);

                                let burn_args : T.Current.BurnArgs = {
                                    from_subaccount = user1.subaccount;
                                    amount = 200 * (10 ** Nat8.toNat(default_token_args.decimals));
                                    memo = null;
                                    created_at_time = null;
                                };

                                let prev_balance = icrc1.balance_of(user1);
                                let prev_total_supply = icrc1.total_supply();
                                let #trappable(res) = await* icrc1.burn( burn_args, user1.owner, false);

                                assertAllTrue([
                                    res == #Err(
                                        #InsufficientFunds {
                                            balance = 0;
                                        },
                                    ),
                                ]);
                            },
                        ),
                        it(
                            "burn amount less than min_burn_amount",
                            do {
                                D.print("burn3");
                                let icrc1 = get_icrc(default_token_args);

                                let mint_args : T.Current.Mint = {
                                    to = user1;
                                    amount = 200 * (10 ** Nat8.toNat(default_token_args.decimals));
                                    memo = null;
                                    created_at_time = null;
                                };

                                ignore await* icrc1.mint(
                                    mint_args,
                                    canister.owner,
                                );

                                let burn_args : T.Current.BurnArgs = {
                                    from_subaccount = user1.subaccount;
                                    amount = 5 * (10 ** Nat8.toNat(default_token_args.decimals));
                                    memo = null;
                                    created_at_time = null;
                                };

                                let #trappable(res) = await* icrc1.burn(burn_args, user1.owner, false);

                                assertAllTrue([
                                    res == #Err(
                                        #BadBurn {
                                            min_burn_amount = 10 * (10 ** 8);
                                        },
                                    ),
                                ]);
                            },
                        ),
                    ],
                ),
                describe(
                    "transfer()",
                    [
                        it(
                            "Transfer from funded account",
                            do {
                                D.print(" testing transfer");
                                let icrc1 = get_icrc(default_token_args);

                                let mint_args = {
                                    to = user1;
                                    amount = 200 * (10 ** Nat8.toNat(icrc1.decimals()));
                                    memo = null;
                                    created_at_time = null;
                                };

                                ignore await* icrc1.mint(
                                    mint_args,
                                    canister.owner,
                                );

                                let transfer_args : T.Current.TransferArgs = {
                                    from_subaccount = user1.subaccount;
                                    to = user2;
                                    amount = 50 * (10 ** Nat8.toNat(icrc1.decimals()));
                                    fee = ?icrc1.fee();
                                    memo = null;
                                    created_at_time = null;
                                };

                                let #trappable(res) = await* icrc1.transfer(
                                    
                                    transfer_args,
                                    user1.owner,
                                    false
                                );


                                assertAllTrue([
                                    res == #Ok(1),
                                    icrc1.balance_of( user1) == 145,
                                    icrc1.get_state()._burned_tokens ==  5,
                                    icrc1.balance_of( user2) == 50,
                                    icrc1.total_supply() == 195,
                                ]);
                            },
                        ),
                      it("Max Accounts Cleanup", do {

                          let icrc1 = get_icrc({default_token_args with
                            max_accounts = ?100;
                            settle_to_accounts = ?75;
                          } : ICRC1.InitArgs);
                          
                          // Assuming an ICRC1 interface that allows setting a max number of accounts.
                          let max_accounts = 100; // Set the max accounts limit
                          let initial_accounts = Array.tabulate<ICRC1.Account>(max_accounts + 1, func(i : Nat) : ICRC1.Account {
                              { owner = user1.owner; subaccount = ?Blob.fromArray([ 
                                1,2,0,0,0,0,0,0,
                                0,0,0,0,0,0,0,0,
                                0,0,0,0,0,0,0,0,
                                0,0,0,0,0,0,0, Nat8.fromNat(i)])}
                          });

                          

                          // Add initial accounts to reach the limit
                          var tracker = 1;
                          for(thisItem in initial_accounts.vals()){
                            let result =  await* icrc1.mint({ to = thisItem; amount = tracker; memo = null; created_at_time = null; }, icrc1.minting_account().owner);
                            tracker +=1;
                            D.print(debug_show(result));
                          };

                          let fake1 = await Fake.Fake();
                          D.print("faking ");
                          let fake2 = await Fake.Fake();
                          let fake3 = await Fake.Fake();
                          let fake4 = await Fake.Fake();

                          D.print("size of items = " # debug_show(Map.size(icrc1.get_state().accounts)));

                          //simulate time

                          // Check that the account count is still within the limit and that the smallest account has been cleaned up
                          assertAllTrue([Map.size(icrc1.get_state().accounts) > 50,
                          Map.size(icrc1.get_state().accounts) < 80, //25 should have been deleted
                          Vector.size(icrc1.get_state().local_transactions) > 120, //removals should be in the trx log
                          Map.get(icrc1.get_state().accounts, ahash, initial_accounts[0]) == null]); // Assuming 1 is the smallest
                        }
                      ),
                      it("Different Memos Pass Deduplication", do {

                        let icrc1 = get_icrc(default_token_args);
                       
                        let tx_amount = 1000*e8s;
                        let mint =  await* icrc1.mint({ to = user1; amount = tx_amount; memo = null; created_at_time = null; }, canister.owner);
                        
                         D.print("result for mint 1 was " # debug_show(mint));
                        // First transfer
                        let result1 = await* icrc1.transfer({ to = user2; from_subaccount = user1.subaccount; amount = 1; created_at_time=null; fee=?500_000_000; memo = ?Text.encodeUtf8("Memo 1") }, user1.owner, false);
                        D.print("result for memo 1 was " # debug_show(result1));
                        // Second transfer with a different memo
                        let res = await* icrc1.transfer({ to = user2; from_subaccount = user1.subaccount; amount = 1; fee=?500_000_000; created_at_time=null; memo = ?Text.encodeUtf8("Memo 2") }, user1.owner, false);

                        D.print("result for memo 2 was " # debug_show(res));
                        let #trappable(#Ok(result)) = res;

                        assertTrue(result > 0); // Check transaction index or proper variant
                      }), 
                      it("Different created_at_time Pass Deduplication", do {

                        let icrc1 = get_icrc(default_token_args);
                       
                        let tx_amount = 1000*e8s;
                        let mint =  await* icrc1.mint({ to = user1; amount = tx_amount; memo = null; created_at_time = null; }, canister.owner);
                        
                        D.print("result for mint 1 was " # debug_show(mint));
                        // First transfer
                        let result1 =  await* icrc1.transfer({ to = user2; from_subaccount = user1.subaccount; amount = 1; created_at_time =?Nat64.fromNat(Int.abs(Time.now())); fee=?500_000_000;memo = ?Text.encodeUtf8("Memo 1") }, user1.owner, false);

                        D.print("result for date 1 was " # debug_show(result1));

                        // Second transfer with a different memo
                        let res= await* icrc1.transfer({ to = user2; from_subaccount = user1.subaccount; amount = 1; fee=?500_000_000; created_at_time= ?(Nat64.fromNat(Int.abs(Time.now() + 10_000))); memo = ?Text.encodeUtf8("Memo 1") }, user1.owner, false);
                         D.print("result for date 2 was " # debug_show(res));

                        let #trappable(#Ok(result)) = res;

                       

                        assertTrue(result > 0); // Check transaction index or proper variant
                      }),
                      it("Different created_at_time Pass Deduplication", do {

                        let icrc1 = get_icrc(default_token_args);
                       
                        let tx_amount = 1000*e8s;
                        let mint =  await* icrc1.mint({ to = user1; amount = tx_amount; memo = null; created_at_time = null; }, canister.owner);
                        
                        D.print("result for mint 1 was " # debug_show(mint));
                        // First transfer
                        let result1 =  await* icrc1.transfer({ to = user2; from_subaccount = user1.subaccount; amount = 1; created_at_time =?Nat64.fromNat(Int.abs(Time.now())); fee=?500_000_000;memo = ?Text.encodeUtf8("Memo 1") }, user1.owner, false);

                        D.print("result for date 1 was " # debug_show(result1));

                        // Second transfer with a different memo
                        let res= await* icrc1.transfer({ to = user2; from_subaccount = user1.subaccount; amount = 1; fee=?500_000_000; created_at_time= ?(Nat64.fromNat(Int.abs(Time.now() + 10_000))); memo = ?Text.encodeUtf8("Memo 1") }, user1.owner, false);
                         D.print("result for date 2 was " # debug_show(res));

                        let #trappable(#Ok(result)) = res;

                       

                        assertTrue(result > 0); // Check transaction index or proper variant
                      }),
                      it("Deduplication with Test Time", do {
                        test_time := Time.now();

                        let icrc1 = get_icrc(default_token_args);
                       
                        let tx_amount = 1000*e8s;
                        let mint =  await* icrc1.mint({ to = user1; amount = tx_amount; memo = null; created_at_time = null; }, canister.owner);
                        
                         D.print("Deduplication result for mint 1 was " # debug_show(mint));
                        // First transfer
                        let result1 = await* icrc1.transfer({ to = user2; from_subaccount = user1.subaccount; amount = 1; created_at_time=null; fee=?500_000_000;memo = ?Text.encodeUtf8("Memo 1") }, user1.owner, false);
                        D.print(" Deduplication result for memo 1 was " # debug_show(result1));

                        // Second exact transfer should fail
                        let result2 = await* icrc1.transfer({ to = user2; from_subaccount = user1.subaccount; amount = 1; created_at_time=null; fee=?500_000_000;memo = ?Text.encodeUtf8("Memo 1") }, user1.owner, false);

                        D.print("Deduplication result for memo 2 was " # debug_show(result2));
                        let #trappable(#Err(#Duplicate(result))) = result2;

                       

                        //advance time past the window
                        
                        test_time := test_time + 86_400_000_000_001 + 60_000_000_000;

                        //let fake1 = await Fake.Fake();

                        let result3 = await* icrc1.transfer({ to = user2; from_subaccount = user1.subaccount; amount = 1; created_at_time=null; fee=?500_000_000;memo = ?Text.encodeUtf8("Memo 1") }, user1.owner, false);

                        D.print("Dedup result for memo 3 was " # debug_show(result3));
                        let #trappable(#Ok(res3)) = result3;

                         

                        assertAllTrue([res3 == 2 ,
                        result.duplicate_of == 1]); // Check transaction index or proper variant
                      }),
                      it("Transfer with Insufficient Funds", do {
                        let icrc1 = get_icrc(default_token_args);
                       
                        let tx_amount = 1000*e8s;
                        let mint =  await* icrc1.mint({ to = user1; amount = tx_amount; memo = null; created_at_time = null; }, canister.owner);
                        // First transfer
                        let result1 = await* icrc1.transfer({ to = user2; from_subaccount = user1.subaccount; amount = tx_amount * 2; created_at_time=null; fee=?500_000_000;memo = ?Text.encodeUtf8("Memo 1") }, user1.owner, false);

                        D.print("Insufficent 1 was " # debug_show(result1));

                        let #trappable(#Err(#InsufficientFunds(res))) = result1;

                        assertTrue(res.balance == tx_amount);
                      }),
                      it("Transfer with higher fee ok", do {
                        let icrc1 = get_icrc(default_token_args);
                        let tx_amount = 1000*e8s;
                        let mint =  await* icrc1.mint({ to = user1; amount = tx_amount; memo = null; created_at_time = null; }, canister.owner);
                        
                        // First transfer
                        let result1 = await* icrc1.transfer({ to = user2; from_subaccount = user1.subaccount; amount = 1; created_at_time=null; fee=?(icrc1.fee() + 1);memo = ?Text.encodeUtf8("Memo 1") }, user1.owner, false);

                        D.print("valid fee " # debug_show(result1));

                        let #trappable(#Ok(res)) = result1;

                        D.print(debug_show(icrc1.balance_of(user1)));

                        assertAllTrue([
                          assertTrue(res == 1),
                          icrc1.balance_of(user1) == (1000*e8s) - 1 - (icrc1.fee() + 1),
                          icrc1.get_state()._burned_tokens == icrc1.fee() + 1
                          ]);
                      }),
                      it("Transfer with null fee ok", do {
                        let icrc1 = get_icrc(default_token_args);
                        let tx_amount = 1000*e8s;
                        let mint =  await* icrc1.mint({ to = user1; amount = tx_amount; memo = null; created_at_time = null; }, canister.owner);
                        
                        // First transfer
                        let result1 = await* icrc1.transfer({ to = user2; from_subaccount = user1.subaccount; amount = 1; created_at_time=null; fee=null;memo = ?Text.encodeUtf8("Memo 1") }, user1.owner, false);

                        D.print("valid fee " # debug_show(result1));

                        let #trappable(#Ok(res)) = result1;

                        D.print(debug_show(icrc1.balance_of(user1)));

                        assertAllTrue([
                          assertTrue(res == 1),
                          icrc1.balance_of(user1) == (1000*e8s) - 1 - (icrc1.fee()),
                          icrc1.get_state()._burned_tokens == icrc1.fee()
                          ]);
                      }),
                      it("Transfer with lower fee fails", do {
                        let icrc1 = get_icrc(default_token_args);
                        let tx_amount = 1000*e8s;
                        let mint =  await* icrc1.mint({ to = user1; amount = tx_amount; memo = null; created_at_time = null; }, canister.owner);
                        
                        // First transfer
                        let result1 = await* icrc1.transfer({ to = user2; from_subaccount = user1.subaccount; amount = 1; created_at_time=null; fee=?(icrc1.fee() - 1);memo = ?Text.encodeUtf8("Memo 1") }, user1.owner, false);

                        D.print("invalid fee " # debug_show(result1));

                        let #trappable(#Err(#BadFee(res))) = result1;

                        assertTrue(res.expected_fee == icrc1.fee());
                      }),
                      it("Transfer created in future fails", do {
                        let icrc1 = get_icrc(default_token_args);
                        let tx_amount = 1000*e8s;
                        let mint =  await* icrc1.mint({ to = user1; amount = tx_amount; memo = null; created_at_time = null; }, canister.owner);
                        
                        // First transfer
                        let result1 = await* icrc1.transfer({ to = user2; from_subaccount = user1.subaccount; amount = 1; created_at_time=?(U.get_time64(environment) + icrc1.get_state().permitted_drift + 1); fee=null;memo = ?Text.encodeUtf8("Memo 1") }, user1.owner, false);

                        D.print("future " # debug_show(result1));

                        let #trappable(#Err(#CreatedInFuture(res))) = result1;

                        assertTrue(res.ledger_time == U.get_time64(environment));
                      }),
                      it("Transfer too old fails", do {
                        let icrc1 = get_icrc(default_token_args);
                        let tx_amount = 1000*e8s;
                        let mint =  await* icrc1.mint({ to = user1; amount = tx_amount; memo = null; created_at_time = null; }, canister.owner);
                        
                        // First transfer
                        let result1 = await* icrc1.transfer({ to = user2; from_subaccount = user1.subaccount; amount = 1; created_at_time=?(U.get_time64(environment) - icrc1.get_state().transaction_window - icrc1.get_state().permitted_drift - 1); fee=null;memo = ?Text.encodeUtf8("Memo 1") }, user1.owner, false);

                        D.print("too old " # debug_show(result1));

                        let #trappable(#Err(#TooOld(res))) = result1;

                        assertTrue(result1 == #trappable(#Err(#TooOld)));
                      }),
                      it("External sync can_transfer invalidates a transaction",
                          do {
                              
                              let icrc1 = get_icrc_env(default_token_args, {
                                environment with
                                can_transfer = ?#Sync(externalCanTransferFalseSync)
                              });
                              let tx_amount = 1000*e8s;

                              let mint =  await* icrc1.mint({ to = user1; amount = tx_amount; memo = null; created_at_time = null; }, canister.owner);
                              
                              // First transfer
                              let result1 = await* icrc1.transfer({ to = user2; from_subaccount = user1.subaccount; amount = 1; created_at_time=null; fee=null;memo = ?Text.encodeUtf8("Memo 1") }, user1.owner, false);

                              D.print("reject sync " # debug_show(result1));

                              let #trappable(#Err(#GenericError(res))) = result1;

                              assertTrue(res.message == "always false");
                          }),
                          it("External async can_transfer invalidates a transaction",
                          do {
                              
                              let icrc1 = get_icrc_env(default_token_args, {
                                environment with
                                can_transfer = ?#Async(externalCanTransferFalseAsync)
                              });
                              let tx_amount = 1000*e8s;

                              let mint =  await* icrc1.mint({ to = user1; amount = tx_amount; memo = null; created_at_time = null; }, canister.owner);
                              
                              // First transfer
                              let result1 = await* icrc1.transfer({ to = user2; from_subaccount = user1.subaccount; amount = 1; created_at_time=null; fee=null;memo = ?Text.encodeUtf8("Memo 1") }, user1.owner, false);

                              D.print("reject async " # debug_show(result1));

                              let #awaited(#Err(#GenericError(res))) = result1;

                              assertTrue(res.message == "always false");
                          }),
                          it("External sync can_transfer updates a transaction",
                          do {
                              
                              let icrc1 = get_icrc_env(default_token_args, {
                                environment with
                                can_transfer = ?#Sync(externalCanTransferUpdateSync)
                              });
                              let tx_amount = 1000*e8s;

                              let mint =  await* icrc1.mint({ to = user1; amount = tx_amount; memo = null; created_at_time = null; }, canister.owner);
                              
                              // First transfer
                              let result1 = await* icrc1.transfer({ to = user2; from_subaccount = user1.subaccount; amount = 1; created_at_time=null; fee=null;memo = ?Text.encodeUtf8("Memo 1") }, user1.owner, false);

                              D.print("update sync " # debug_show(result1));

                              let #trappable(#Ok(res)) = result1;
                              let ledger = Vector.toArray(icrc1.get_local_transactions());
                              let ?trn = ledger[1].transfer;

                              assertAllTrue([
                                res == 1,
                                trn.amount == 2
                              ]);
                          }),
                          it("External async can_transfer updates a transaction",
                          do {
                              
                              let icrc1 = get_icrc_env(default_token_args, {
                                environment with
                                can_transfer = ?#Async(externalCanTransferUpdateAsync)
                              });
                              let tx_amount = 1000*e8s;

                              let mint =  await* icrc1.mint({ to = user1; amount = tx_amount; memo = null; created_at_time = null; }, canister.owner);
                              
                              // First transfer
                              let result1 = await* icrc1.transfer({ to = user2; from_subaccount = user1.subaccount; amount = 1; created_at_time=null; fee=null;memo = ?Text.encodeUtf8("Memo 1") }, user1.owner, false);

                              D.print("update async " # debug_show(result1));

                              let #awaited(#Ok(res)) = result1;
                              let ledger = Vector.toArray(icrc1.get_local_transactions());
                              let ?trn = ledger[1].transfer;

                               assertAllTrue([
                                res == 1,
                                trn.amount == 2
                              ]);
                          }),
                          it("Cant transfer to self",
                          do {
                              
                              let icrc1 = get_icrc(default_token_args);
                              let tx_amount = 1000*e8s;

                              let mint =  await* icrc1.mint({ to = user1; amount = tx_amount; memo = null; created_at_time = null; }, canister.owner);
                              
                              // First transfer
                              let result1 = await* icrc1.transfer({ to = user1; from_subaccount = user1.subaccount; amount = 1; created_at_time=null; fee=null;memo = ?Text.encodeUtf8("Memo 1") }, user1.owner, false);

                              D.print("update self send " # debug_show(result1));

                              let #trappable(#Err(#GenericError(res))) = result1;

                               assertAllTrue([
                                res.error_code == 1,
                              ]);
                          }),
                          it("Cant have 0 amount in transfer",
                          do {
                              
                              let icrc1 = get_icrc(default_token_args);
                              let tx_amount = 1000*e8s;

                              let mint =  await* icrc1.mint({ to = user1; amount = tx_amount; memo = null; created_at_time = null; }, canister.owner);
                              
                              // First transfer
                              let result1 = await* icrc1.transfer({ to = user2; from_subaccount = user1.subaccount; amount = 0; created_at_time=null; fee=null;memo = ?Text.encodeUtf8("Memo 1") }, user1.owner, false);

                              D.print("update self send " # debug_show(result1));

                              let #trappable(#Err(#GenericError(res))) = result1;

                               assertAllTrue([
                                res.error_code == 5,
                              ]);
                          }),

                          it("Can listen for tranfers",
                          do {
                              
                              let icrc1 = get_icrc(default_token_args);
                              let tx_amount = 1000*e8s;

                               var called_count = 0;

                              let a_func = func(trx: ICRC1.Transaction, trxid: Nat){
                                 D.print("listner called " # debug_show(trx));
                                called_count += 1;
                              };
                              
                              icrc1.register_token_transferred_listener("icrc_test", a_func);

                              let mint =  await* icrc1.mint({ to = user1; amount = tx_amount; memo = null; created_at_time = null; }, canister.owner);

                             
                              
                              // First transfer
                              let result1 = await* icrc1.transfer({ to = user2; from_subaccount = user1.subaccount; amount = 1; created_at_time=null; fee=null;memo = ?Text.encodeUtf8("Memo 1") }, user1.owner, false);

                              D.print("listner result " # debug_show(result1));

                              

                               assertAllTrue([
                                called_count == 2,
                              ]);
                          })

                    ],
                ),

            ],
        );
    };

    public func testInitInvalidParameters() : async ActorSpec.Group {
      D.print("in testInitInvalidParameters");
      let spender1 : ICRC1.Account = {owner = Principal.fromText("2dzql-vc5j3-p5nyh-vidom-fhtyt-edvv6-bqewt-j63fn-ovwug-h67hb-yqe"); subaccount = ?Blob.fromArray([ 1,2,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0])};
      let spender2 : ICRC1.Account = {owner = Principal.fromText("32fn4-qqaaa-aaaak-ad65a-cai"); subaccount = ?Blob.fromArray([3,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0])};
      let owner : ICRC1.Account = {owner = Principal.fromText("zfcdd-tqaaa-aaaaq-aaaga-cai"); subaccount = ?Blob.fromArray([5,6,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0])};
      let spender4 : ICRC1.Account = {owner = Principal.fromText("x33ed-h457x-bsgyx-oqxqf-6pzwv-wkhzr-rm2j3-npodi-purzm-n66cg-gae"); subaccount = ?Blob.fromArray([7,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0])};
      let spender5 : ICRC1.Account = {owner = Principal.fromText("dtnbn-kyaaa-aaaak-aeigq-cai"); subaccount = ?Blob.fromArray([9,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0])};

    let {
      assertAllTrue;
      it;
      describe;
      run;
    } = ActorSpec;

    let faultyInitialBalances : [(T.Current.Account, Nat)] = [
      (spender1, 100),   // Make more initial than possibl max
      (spender2, 100),    // Positive initial balance
    ];

    // Negative max_supply
    let negativeMaxSupplyArgs : T.Current.InitArgs = {
      name = ?"Invalid Token";
      symbol = ?"INV";
      decimals = 8;
      fee = ?#Fixed(5 * (10 ** 8));
      max_supply = ?(100);
      minting_account = null;
      min_burn_amount = ?(10 * (10 ** 8));
      advanced_settings = null;
      metadata = null;
      max_memo = null;
      fee_collector = null;
      permitted_drift = null;
            transaction_window = null;
            max_accounts = null;
            settle_to_accounts = null;
    };

    let canister : T.Current.Account = {
        owner = Principal.fromText("x4ocp-k7ot7-oiqws-rg7if-j4q2v-ewcel-2x6we-l2eqz-rfz3e-6di6e-jae");
        subaccount = null;
    };

    let token_state = ICRC1.init(ICRC1.initialState(), #v0_1_0(#id), ?negativeMaxSupplyArgs, owner.owner);

   let icrc1  = ICRC1.ICRC1(?token_state, canister.owner, base_environment);

   for(thisItem in faultyInitialBalances.vals()){
    ignore await* icrc1.mint({
      to = thisItem.0;
      amount = thisItem.1;
      memo = ?Text.encodeUtf8("init");
      created_at_time = null;
    }, icrc1.minting_account().owner)
  };

    return describe("ICRC-1 Token Initialization with initial balances over max supply only mints to max supply", [
      it("Only mints  up to the max supply", do {
        D.print("total supply over " # debug_show(icrc1.total_supply()));
        assertAllTrue([icrc1.total_supply() == 100]);
      }),
      
    ]);
  };
};
