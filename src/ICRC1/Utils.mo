
import Array "mo:base/Array";
import Blob "mo:base/Blob";
import D "mo:base/Debug";
import Hash "mo:base/Hash";
import Int "mo:base/Int";
import Iter "mo:base/Iter";
import Nat "mo:base/Nat";
import Nat8 "mo:base/Nat8";
import Nat32 "mo:base/Nat32";
import Nat64 "mo:base/Nat64";
import Option "mo:base/Option";
import Principal "mo:base/Principal";
import Result "mo:base/Result";
import Time "mo:base/Time";

import ArrayModule "mo:array/Array";
import Itertools "mo:itertools/Iter";
import STMap "mo:StableTrieMap";
import Vec "mo:vector";

import Map "mo:map9/Map";

import Account "Account";
import MigrationTypes "./migrations/types";

module {

    type State = MigrationTypes.Current.State;
    type Environment = MigrationTypes.Current.Environment;
    type Value = MigrationTypes.Current.Value;

    let ahash = MigrationTypes.Current.ahash;

    // Creates a Stable Buffer with the default metadata and returns it.
    public func init_metadata(state : MigrationTypes.Current.State, canister: Principal) : MigrationTypes.Current.Value {
        let metadata = Vec.new<MigrationTypes.Current.MetaDatum>();
        Vec.add(metadata, ("icrc1:fee", #Nat(switch(state._fee){
          case(null) 10000;
          case(?val){
            switch(val){
              case(#Fixed(val))val;
              case(#Environment) 10000; //a lie as it is determined at runtime.
            };
          }
        })));
        Vec.add(metadata, ("icrc1:name", #Text(switch(state.name){
          case(null) Principal.toText(canister);
          case(?val) val;
        })));
        Vec.add(metadata, ("icrc1:symbol", #Text(switch(state.symbol){
          case(null) Principal.toText(canister);
          case(?val) val;
        })));
        Vec.add(metadata, ("icrc1:decimals", #Nat(Nat8.toNat(state.decimals))));

        #Map(Vec.toArray(metadata));
    };

    public let default_standard : MigrationTypes.Current.SupportedStandard = {
        name = "ICRC-1";
        url = "https://github.com/dfinity/ICRC-1";
    };

    // Creates a Stable Buffer with the default supported standards and returns it.
    public func init_standards() : Vec.Vector<MigrationTypes.Current.SupportedStandard> {
        let standards = Vec.new<MigrationTypes.Current.SupportedStandard>();
        Vec.add(standards, default_standard);
        standards;
    };

    // Returns the default subaccount for cases where a user does
    // not specify it.
    public func default_subaccount() : MigrationTypes.Current.Subaccount {
        Blob.fromArray(
            Array.tabulate(32, func(_ : Nat) : Nat8 { 0 }),
        );
    };

    // this is a local copy of deprecated Hash.hashNat8 (redefined to suppress the warning)
    func hashNat8(key : [Nat32]) : Hash.Hash {
        var hash : Nat32 = 0;
        for (natOfKey in key.vals()) {
            hash := hash +% natOfKey;
            hash := hash +% hash << 10;
            hash := hash ^ (hash >> 6);
        };
        hash := hash +% hash << 3;
        hash := hash ^ (hash >> 11);
        hash := hash +% hash << 15;
        return hash;
    };

    // Computes a hash from the least significant 32-bits of `n`, ignoring other bits.
    public func hash(n : Nat) : Hash.Hash {
        let j = Nat32.fromNat(n);
        hashNat8([
            j & (255 << 0),
            j & (255 << 8),
            j & (255 << 16),
            j & (255 << 24),
        ]);
    };

    public func get_time64(environment: Environment) : Nat64{
      //D.print("in get_time64");
      return (switch(environment.get_time){
        case(null){
          //D.print("returning time from time");
          Time.now();
        };
        case(?get_time){
          //D.print("returning time env" # debug_show(get_time()));
          get_time();
        };
      }) 
      |> Int.abs(_) 
      |> Nat64.fromNat(_);
    };

    // Formats the different operation arguements into
    // a `TransactionRequest`, an internal type to access fields easier.
    public func create_transfer_req(
        args : MigrationTypes.Current.TransferArgs,
        owner : Principal,
        tx_kind: MigrationTypes.Current.TxKind,
    ) : MigrationTypes.Current.TransactionRequest {
        
        let from = {
            owner;
            subaccount = args.from_subaccount;
        };

        switch (tx_kind) {
            case (#mint) {
                {
                    args with kind = #mint;
                    fee = null;
                    from;
                };
            };
            case (#burn) {
                {
                    args with kind = #burn;
                    fee = null;
                    from;
                };
            };
            case (#transfer) {
                {
                    args with kind = #transfer;
                    from;
                };
            };
        };
    };

    // Transforms the transaction kind from `variant` to `Text`
    public func kind_to_text(kind : MigrationTypes.Current.TxKind) : Text {
        switch (kind) {
            case (#mint) "MINT";
            case (#burn) "BURN";
            case (#transfer) "TRANSFER";
        };
    };

    // Formats the tx request into a finalised transaction
    public func req_to_tx(tx_req : MigrationTypes.Current.TransactionRequest, index: Nat) : MigrationTypes.Current.Transaction {

        {
            kind = kind_to_text(tx_req.kind);
            mint = switch (tx_req.kind) {
                case (#mint) { ?tx_req };
                case (_) null;
            };

            burn = switch (tx_req.kind) {
                case (#burn) { ?tx_req };
                case (_) null;
            };

            transfer = switch (tx_req.kind) {
                case (#transfer) { ?tx_req };
                case (_) null;
            };
            
            index;
            timestamp = Nat64.fromNat(Int.abs(Time.now()));
        };
    };

    public func div_ceil(n : Nat, d : Nat) : Nat {
        (n + d - 1) / d;
    };

    public func accountToValue(acc : MigrationTypes.Current.Account) : MigrationTypes.Current.Value {
        let vec = Vec.new<MigrationTypes.Current.Value>();
        Vec.add(vec, #Blob(Principal.toBlob(acc.owner)));
        switch(acc.subaccount){
          case(null){};
          case(?val){
            Vec.add(vec, #Blob(val));
          };
        };

        return #Array(Vec.toArray(vec));
      };

    /// Retrieves the balance of an account
    public func get_balance(accounts : MigrationTypes.Current.AccountBalances, account : MigrationTypes.Current.Account) : MigrationTypes.Current.Balance {
        let res = Map.get(
            accounts,
            ahash,
            account,
        );

        switch (res) {
            case (?balance) {
                balance;
            };
            case (_) 0;
        };
    };

    /// Updates the balance of an account
    public func update_balance(
        accounts : MigrationTypes.Current.AccountBalances,
        account : MigrationTypes.Current.Account,
        update : (MigrationTypes.Current.Balance) -> MigrationTypes.Current.Balance,
    ) {
        let prev_balance = get_balance(accounts, account);
        let updated_balance = update(prev_balance);

        if (updated_balance != prev_balance) {
            if(updated_balance == 0){
              ignore Map.remove(
                  accounts,
                  ahash,
                  account
              );
            } else {
              ignore Map.put(
                  accounts,
                  ahash,
                  account,
                  updated_balance,
              );
            };
        };

        
    };

    public func insert_map(top : ?Value, key : Text, val : Value) : Result.Result<Value, Text> {
        let foundTop = switch (top) {
          case (?val) val;
          case (null) #Map([]);
        };
        switch (foundTop) {
          case (#Map(a_map)) {
            let vecMap = Vec.fromArray<(Text, Value)>(a_map);
            Vec.add<(Text, Value)>(vecMap, (key, val));
            return #ok(#Map(Vec.toArray(vecMap)));
          };
          case (_) return #err("bad map");
        };
      };

    // Transfers tokens from the sender to the
    // recipient in the tx request
    public func transfer_balance(
        state : MigrationTypes.Current.State,
        tx_req : MigrationTypes.Current.TransactionRequest,
    ) { 
        let { amount; from; to;} = tx_req;

        update_balance(
            state.accounts,
            from,
            func(balance) {
                balance - amount;
            },
        );

        update_balance(
            state.accounts,
            to,
            func(balance) {
                balance + amount;
            },
        );
    };

    public func mint_balance(
        state : MigrationTypes.Current.State,
        account : MigrationTypes.Current.Account,
        amount : MigrationTypes.Current.Balance,
    ) {
        update_balance(
            state.accounts,
            account,
            func(balance) {
                balance + amount;
            },
        );

        state._minted_tokens += amount;
    };

    public func burn_balance(
        state : MigrationTypes.Current.State,
        account : MigrationTypes.Current.Account,
        amount : MigrationTypes.Current.Balance,
    ) {
        update_balance(
            state.accounts,
            account,
            func(balance) {
                balance - amount;
            },
        );

        state._burned_tokens += amount;
    };

    
};
