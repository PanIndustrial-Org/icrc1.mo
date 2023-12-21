import Array "mo:base/Array";
import Blob "mo:base/Blob";
import D "mo:base/Debug";
import EC "mo:base/ExperimentalCycles";
import Float "mo:base/Float";
import Int "mo:base/Int";
import Iter "mo:base/Iter";
import Nat "mo:base/Nat";
import Nat64 "mo:base/Nat64";
import Nat8 "mo:base/Nat8";
import Option "mo:base/Option";
import Order "mo:base/Order";
import Principal "mo:base/Principal";
import Result "mo:base/Result";
import Text "mo:base/Text";
import Timer "mo:base/Timer";

import Itertools "mo:itertools/Iter";
import RepIndy "mo:rep-indy-hash";
import Star "mo:star/star";
import Vec "mo:vector";

import Account "Account";
import Migration "./migrations";
import MigrationTypes "./migrations/types";
import Utils "Utils";

/// The ICRC1 class defines the structure and functions necessary for creating and managing ICRC-1 compliant tokens on the Internet Computer. 
/// It encapsulates the state and behavior of a token ledger which includes transfer, mint, and burn functionalities, as well 
/// as metadata handling and tracking of transactions via ICRC-3 transaction logs.
module {

    /// Used to control debug printing for various actions.
    let debug_channel = {
      announce = false;
      transfer = false;
    };

    /// Exposes types from the migrations library to users of this module, allowing them to utilize these types in interacting 
    /// with instances of ICRC1 tokens and their respective attributes and actions.
    public type State =               MigrationTypes.State;


    // Imports from types to make code more readable
    public type CurrentState =        MigrationTypes.Current.State;
    public type Environment =         MigrationTypes.Current.Environment;

    public type Account =             MigrationTypes.Current.Account;
    public type Balance =             MigrationTypes.Current.Balance;
    public type Value =               MigrationTypes.Current.Value;
    public type Subaccount =          MigrationTypes.Current.Subaccount;
    public type AccountBalances =     MigrationTypes.Current.AccountBalances;

    public type Transaction =         MigrationTypes.Current.Transaction;
    public type Fee =                 MigrationTypes.Current.Fee;
    public type MetaData =            MigrationTypes.Current.MetaData;
    public type TransferArgs =        MigrationTypes.Current.TransferArgs;
    public type Mint =                MigrationTypes.Current.Mint;
    public type BurnArgs =            MigrationTypes.Current.BurnArgs;
    public type TransactionRequest =  MigrationTypes.Current.TransactionRequest;
    public type TransactionRequestNotification = MigrationTypes.Current.TransactionRequestNotification;
    public type TransferError =       MigrationTypes.Current.TransferError;

    public type SupportedStandard =   MigrationTypes.Current.SupportedStandard;

    public type InitArgs =            MigrationTypes.Current.InitArgs;
    public type AdvancedSettings =    MigrationTypes.Current.AdvancedSettings;
    public type MetaDatum =           MigrationTypes.Current.MetaDatum;
    public type TxLog =               MigrationTypes.Current.TxLog;
    public type TxIndex =             MigrationTypes.Current.TxIndex;


    public type TransferResult = MigrationTypes.Current.TransferResult;
    public type TokenTransferredListener = MigrationTypes.Current.TokenTransferredListener;
    public type TransferRequest = MigrationTypes.Current.TransactionRequest;


    /// Defines functions to create an initial state, versioning, and utilities for the token ledger. 
    /// These are direct mappings from the Migration types library to provide an easy-to-use API surface for users of the ICRC1 class.
    public func initialState() : State {#v0_0_0(#data)};
    public let currentStateVersion = #v0_1_0(#id);

    // Initializes the state with default or migrated data and sets up other utilities such as maps and vector data structures.
    /// Also initializes helper functions and constants like hashing, account equality checks, and comparisons.
    public let init = Migration.migrate;

    //convienence variables to make code more readable
    public let Map = MigrationTypes.Current.Map;
    public let Vector = MigrationTypes.Current.Vector;
    public let AccountHelper = Account;
    public let UtilsHelper = Utils;
    public let ahash = MigrationTypes.Current.ahash;
    public let account_eq = MigrationTypes.Current.account_eq;
    public let account_compare = MigrationTypes.Current.account_compare;
    public let account_hash32 = MigrationTypes.Current.account_hash32;

    //// The `ICRC1` class encapsulates the logic required for managing a token ledger, providing capabilities 
    //// such as transferring tokens, getting account balances, and maintaining a log of transactions.
    //// It also supports minting and burning tokens while following compliance with the ICRC-1 standard.
    ////
    //// Parameters:
    //// - `stored`: An optional parameter that can be the previously stored state of the ledger for migration purposes.
    //// - `canister`: The `Principal` of the canister where this token ledger is deployed.
    //// - `environment`: Contextual information for the ledger such as fees and timestamp functions.
    public class ICRC1(stored: ?State, canister: Principal, environment: Environment){

      /// Initializes the ledger state with either a new state or a given state for migration. 
      /// This setup process involves internal data migration routines.
      var state : CurrentState = switch(stored){
        case(null) {
          let #v0_1_0(#data(foundState)) = init(initialState(),currentStateVersion, null, canister);
          foundState;
        };
        case(?val) {
          let #v0_1_0(#data(foundState)) = init(val,currentStateVersion, null, canister);
          foundState;
        };
      };

      /// Holds the list of listeners that are notified when a token transfer takes place. 
      /// This allows the ledger to communicate token transfer events to other canisters or entities that have registered an interest.
      private let token_transferred_listeners = Vec.new<(Text, TokenTransferredListener)>();

      
      
      //// Retrieves the full internal state of the token ledger.
      //// This state includes all balances, metadata, transaction logs, and other relevant financial and operational data.
      ////
      //// Returns:
      //// - `CurrentState`: The complete state data of the ledger.
      public func get_state() : CurrentState {
        return state;
      };

      /// Returns the array of local transactions. Does not scale use icrc3-mo for scalable archives
      ///
      /// Returns:
      /// - `Vector<Transaction>`: A vector containing the local transactions recorded in the ledger.
      public func get_local_transactions() : Vec.Vector<Transaction> {
        return state.local_transactions;
      };

      /// Returns the current environment settings for the ledger.
      ///
      /// Returns:
      /// - `Environment`: The environment context in which the ledger operates, encapsulating properties 
      ///   like transfer fee calculation and timing functions.
      public func get_environment() : Environment {
        return environment;
      };

      /// Returns the name of the token for display.
      ///
      /// Returns:
      /// - `Text`: The token's name; or if not set, the default is the canister's principal in text form.
      public func name() : Text {
          switch(state.name){
            case(?val) val;
            case(_) Principal.toText(canister);
          };
      };

      /// Returns the symbol of the token for display, e.g. "BTC" or "ETH".
      ///
      /// Returns:
      /// - `Text`: The token's symbol; or if not set, the default is the canister's principal in text form.
      public func symbol() : Text {
          switch(state.symbol){
            case(?val) val;
            case(_) Principal.toText(canister);
          };
      };

      /// Returns the number of decimals the token uses for precision.
      ///
      /// Best Practice: 8
      ///
      /// Returns:
      /// - `Nat8`: The number of decimals used in token quantity representation.
      public func decimals() : Nat8 {
          state.decimals;
      };

      /// Returns the default or environment-specified transfer fee.
      ///
      /// Returns:
      /// - `Balance`: The fixed or computed fee for each token transfer.
      public func fee() : MigrationTypes.Current.Balance {
          switch(state._fee){
            case(?val) switch(val){
              case(#Fixed(val))val;
              case(#Environment){
                //calculated at runtime
                10000;
              };
            };
            case(_) 10000;
          };
      };

      /// Sets the transfer fee to the specified rate.
      ///
      /// Parameters:
      /// - `fee`: A `Fee` structure specifying the updated fee.
      public func set_fee(fee : Fee) {
          state._fee := ?fee;
      };

      /// `metadata`
      ///
      /// Retrieves all metadata associated with the token ledger, such as the symbol, name, and other relevant data.
      /// If no metadata is found, the method initializes default metadata based on the state and the canister Principal.
      ///
      /// Returns:
      /// `MetaData`: A record containing all metadata entries for this ledger.
      public func metadata() : MetaData {
         switch(state.metadata){
          case(?val) val;
          case(null) {
            let newdata = Utils.init_metadata(state, canister);
            state.metadata := ?newdata;
            newdata;
          };
         };
      };

      /// `total_supply`
      ///
      /// Returns the current total supply of the circulating tokens by subtracting the number of burned tokens from the minted tokens.
      ///
      /// Returns:
      /// `Balance`: The total number of tokens currently in circulation.
      public func total_supply() : MigrationTypes.Current.Balance {
          state._minted_tokens - state._burned_tokens;
      };

      /// `minted_supply`
      ///
      /// Returns the total number of tokens that have been minted since the inception of the ledger.
      ///
      /// Returns:
      /// `Balance`: The total number of tokens minted.
      public func minted_supply() : MigrationTypes.Current.Balance {
          state._minted_tokens;
      };

      /// `burned_supply`
      ///
      /// Returns the total number of tokens that have been burned since the inception of the ledger.
      ///
      /// Returns:
      /// `Balance`: The total number of tokens burned.
      public func burned_supply() : MigrationTypes.Current.Balance {
          state._burned_tokens;
      };

      /// `max_supply`
      ///
      /// Returns the maximum supply of tokens that the ledger can support.
      /// If no maximum supply is set, the function returns `null`.
      ///
      /// Returns:
      /// `?Balance`: The maximum number of tokens that can exist, or `null` if there is no limit.
      public func max_supply() : ?MigrationTypes.Current.Balance {
          state.max_supply;
      };

      /// `minting_account`
      ///
      /// Retrieves the account designated for minting operations. If tokens are sent to this account, they are considered burned.
      ///
      /// Returns:
      /// `Account`: The account with the permission to mint and burn tokens.
      public func minting_account() : MigrationTypes.Current.Account {
          state.minting_account;
      };

      /// `balance_of`
      ///
      /// Retrieves the balance of the specified account.
      ///
      /// Parameters:
      /// - `account`: The account whose balance is being requested.
      ///
      /// Returns:
      /// `Balance`: The number of tokens currently held in the account.
      public func balance_of(account : MigrationTypes.Current.Account) : MigrationTypes.Current.Balance {
          Utils.get_balance(state.accounts, account);
      };

      /// `supported_standards`
      ///
      /// Provides a list of standards supported by the ledger, indicating compliance with various ICRC standards.
      ///
      /// Returns:
      /// `[SupportedStandard]`: An array of supported standards including their names and URLs.
      public func supported_standards() : [MigrationTypes.Current.SupportedStandard] {
          switch(state.supported_standards){
            case(?val){
              Vec.toArray(val);
            };
            case(null){
              let base = Utils.init_standards();
              state.supported_standards := ?base;
              Vec.toArray(base);
              
            };
          };

      };

      /// `update_fee_collector`
      ///
      /// Updates the collector account responsible for gathering transaction fees. If no account is provided, fees are burned.
      ///
      /// Parameters:
      /// - `account`: An optional account that, if provided, will collect the transaction fees.
      public func update_fee_collector(account : ?Account) : () {
        state.fee_collector := account;
        state.fee_collector_emitted := false;
      };

      /// `add_local_ledger`
      ///
      /// Adds a transaction to the local transaction log and returns its index.
      ///
      /// Parameters:
      /// - `tx`: The transaction to add to the log.
      ///
      /// Returns:
      /// `Nat`: The index at which the transaction was added in the local log.
      public func add_local_ledger(tx : Transaction) : Nat{
        Vec.add(state.local_transactions, tx);
        Vec.size(state.local_transactions) - 1;
      };

      


      /// `transfer`
      ///
      /// Processes a token transfer request according to the provided arguments, handling both regular transfers and special cases like minting and burning.
      ///
      /// Parameters:
      /// - `args`: Details about the transfer including source, destination, amount, and other relevant data.
      /// - `caller`: The principal of the caller initiating the transfer.
      /// - `system_override`: A boolean that, if true, allows bypassing certain checks (reserved for system operations like cleaning up small balances).
      ///
      /// Returns:
      /// `TransferResult`: The result of the attempt to transfer tokens, either indicating success or providing error information.
      public func transfer(
          args : MigrationTypes.Current.TransferArgs,
          caller : Principal,
          system_override : Bool
      ) : async* Star.Star<MigrationTypes.Current.TransferResult, Text> {

          D.print("in transfer");

          let from = {
              owner = caller;
              subaccount = args.from_subaccount;
          };

          let tx_kind = if (account_eq(from, state.minting_account)) {
                #mint;
              } else if (account_eq(args.to, state.minting_account)) {
                  #burn;
              } else {
                  #transfer;
              };

          let tx_req = Utils.create_transfer_req(args, caller, tx_kind);

          //when we create the transfer we should calculate the required fee. This should only be done once and used throughout the rest of the calcualtion

          let calculated_fee = switch(tx_req.kind){
            case(#transfer){
              get_fee(args);
            };
            case(_){
              0;
            };
          };
          D.print("validating");
          switch (validate_request(tx_req, calculated_fee, system_override)) {
              case (#err(errorType)) {
                  return #trappable(#Err(errorType));
              };
              case (#ok(_)) {};
          };

          let txMap = transfer_req_to_value(tx_req);
          let txTopMap = transfer_req_to_value_top(calculated_fee, tx_req);

          let pre_notification = {
            tx_req with
            calculated_fee = calculated_fee;
          };

          var bAwaited = false;

          let (finaltx, finaltxtop, notification) : (Value, ?Value, TransferRequest) = switch(environment.can_transfer){
            case(null){
              (txMap, ?txTopMap, pre_notification);
            };
            case(?#Sync(remote_func)){
              switch(remote_func(txMap, ?txTopMap, pre_notification)){
                case(#ok(val)) val;
                case(#err(tx)){
                  return #trappable(#Err(#GenericError({error_code= 6453; message=tx})));
                };
              };
            };
            case(?#Async(remote_func)){
              bAwaited := true;
              switch(await* remote_func(txMap, ?txTopMap, pre_notification)){
                case(#trappable(val)) val;
                case(#awaited(val)){
                  //revalidate 
                  switch (validate_request(val.2, calculated_fee, system_override)) {
                      case (#err(errorType)) {
                          return #awaited(#Err(errorType));
                      };
                      case (#ok(_)) {};
                  };
                  val;
                };
                case(#err(#awaited(tx))){
                  return #awaited(#Err(#GenericError({error_code= 6453; message=tx})));
                };
                case(#err(#trappable(tx))){
                  return #trappable(#Err(#GenericError({error_code= 6453; message=tx})));
                };
              };
            };
          };

          let { amount; to; } = notification;

          D.print("Moving tokens");

          var finaltxtop_var = finaltxtop;

          // process transaction
          switch(notification.kind){
              case(#mint){
                  
                  Utils.mint_balance(state, to, amount);
              };
              case(#burn){
                  Utils.burn_balance(state, from, amount);
              };
              case(#transfer){
                  Utils.transfer_balance(state, notification);

                  // burn fee
                  switch(state.fee_collector){
                    case(null){
                      Utils.burn_balance(state, from, calculated_fee);
                    };
                    case(?val){
                       
                      if(calculated_fee > 0){
                        if(state.fee_collector_emitted){
                          finaltxtop_var := switch(Utils.insert_map(finaltxtop, "fee_collector_block", #Nat(state.fee_collector_block))){
                            case(#ok(val)) ?val;
                            case(#err(err)) return if(bAwaited){
                              #err(#awaited("unreachable map addition"));
                            } else {
                              #err(#trappable("unreachable map addition"));
                            };
                          };
                        } else {
                          finaltxtop_var := switch(Utils.insert_map(finaltxtop, "fee_collector", Utils.accountToValue(val))){
                            case(#ok(val)) ?val;
                            case(#err(err)) return if(bAwaited){
                              #err(#awaited("unreachable map addition"));
                            } else {
                              #err(#trappable("unreachable map addition"));
                            };
                          };
                        };
                      };

                      Utils.transfer_balance(state,{
                        notification with
                        kind = #transfer;
                        to = val;
                        amount = calculated_fee;
                      });
                    };
                  }
              };
          };

          

          // store transaction
          let index = switch(environment.add_ledger_transaction){
            case(?add_ledger_transaction){
              add_ledger_transaction(finaltx, finaltxtop_var);
            };
            case(null){
              let tx = Utils.req_to_tx(notification, Vec.size(state.local_transactions));

              add_local_ledger(tx);
            }
          };

          let tx_final = Utils.req_to_tx(notification, index);

          switch(state.fee_collector){
            case(?val){
              if(calculated_fee > 0){
                if(state.fee_collector_emitted){} else {
                  state.fee_collector_block := index;
                  state.fee_collector_emitted := true;
                };
              };
            };
            case(null){
            };
          };

          //add trx for dedupe
          let trxhash = Blob.fromArray(RepIndy.hash_val(finaltx));

          D.print("attempting to add recent" # debug_show(trxhash, finaltx));

          ignore Map.put<Blob, (Nat64, Nat)>(state.recent_transactions, Map.bhash, trxhash, (get_time64(), index));

          for(thisItem in Vec.vals(token_transferred_listeners)){
            thisItem.1(tx_final, index);
          };


          D.print("cleaning");
          cleanUpRecents();
          switch(state.cleaning_timer){
            case(null){ //only need one active timer
              debug if(debug_channel.transfer) D.print("setting clean up timer");
              state.cleaning_timer := ?Timer.setTimer(#seconds(0), checkAccounts);
            };
            case(_){}
          };

          D.print("done transfer");
          if(bAwaited){
            #awaited(#Ok(index));
          } else {
            #trappable(#Ok(index));
          };
      };

      /// `mint`
      ///
      /// Allows the minting account to create new tokens and add them to a specified beneficiary account.
      ///
      /// Parameters:
      /// - `args`: Minting arguments including the destination account and the amount to mint.
      /// - `caller`: The principal of the caller requesting the mint operation.
      ///
      /// Returns:
      /// `TransferResult`: The result of the mint operation, either indicating success or providing error information.
      public func mint(args : MigrationTypes.Current.Mint, caller : Principal) : async* Star.Star<MigrationTypes.Current.TransferResult, Text> {

         
          if (caller != state.minting_account.owner) {
              return #trappable(#Err(
                  #GenericError {
                      error_code = 401;
                      message = "Unauthorized: Only the minting_account can mint tokens.";
                  },
              ));
          };

          let transfer_args : MigrationTypes.Current.TransferArgs = {
              args with from_subaccount = state.minting_account.subaccount;
              fee = null;
          };
          //todo: override on initial mint?
          await* transfer(transfer_args, caller, false);
      };

      /// `burn`
      ///
      /// Allows an account to burn tokens by transferring them to the minting account and removing them from the total token supply.
      ///
      /// Parameters:
      /// - `args`: Burning arguments including the amount to burn.
      /// - `caller`: The principal of the caller requesting the burn operation.
      /// - `system_override`: A boolean that allows bypassing the minimum burn amount check if true. Reserved for system operations.
      ///
      /// Returns:
      /// `TransferResult`: The result of the burn operation, either indicating success or providing error information.
      public func burn(args : MigrationTypes.Current.BurnArgs, caller : Principal, system_override: Bool) : async* Star.Star<MigrationTypes.Current.TransferResult, Text> {



          let transfer_args : MigrationTypes.Current.TransferArgs = {
              args with 
              to = state.minting_account;
              fee : ?Balance = null;
          };

          await* transfer(transfer_args, caller, system_override);
      };

      /// `validate_memo`
      ///
      /// Validates the provided memo by checking that its size is within the bounds set by the ledger's maximum memo size.
      ///
      /// Parameters:
      /// - `memo`: The optional memo field of a transfer request.
      ///
      /// Returns:
      /// `Bool`: True if the memo is valid, false otherwise.
      public func validate_memo(memo : ?MigrationTypes.Current.Memo) : Bool {
          switch (memo) {
              case (?bytes) {
                  bytes.size() <= state.max_memo;
              };
              case (_) true;
          };
      };

      /// `is_too_old`
      ///
      /// Checks whether the `created_at_time` of a transfer request is too old according to the ledger's permitted time range.
      ///
      /// Parameters:
      /// - `created_at_time`: The timestamp denoting when the transfer was initiated.
      ///
      /// Returns:
      /// `Bool`: True if the transaction is considered too old, false otherwise.
      public func is_too_old(created_at_time : Nat64) : Bool {
          D.print("testing is_too_old");
          let current_time : Nat64  = get_time64();
          D.print("current time is" # debug_show(current_time,state.transaction_window ,state.permitted_drift ));
          let lower_bound = current_time - state.transaction_window - state.permitted_drift;
          created_at_time < lower_bound;
      };

      /// `is_in_future`
      ///
      /// Determines if the `created_at_time` of a transfer request is set in the future relative to the ledger's clock.
      ///
      /// Parameters:
      /// - `created_at_time`: The timestamp to validate against the current ledger time.
      ///
      /// Returns:
      /// `Bool`: True if the timestamp is in the future, false otherwise.
      public func is_in_future(created_at_time : Nat64) : Bool {
          D.print("testing is_in_future" # debug_show(created_at_time, state.permitted_drift, get_time64()));
          let current_time : Nat64  = get_time64();
          let upper_bound = current_time + state.permitted_drift;
          created_at_time > upper_bound;
      };

    /// `find_dupe`
    ///
    /// Searches for a duplicate transaction using the provided hash.
    ///
    /// Parameters:
    /// - `trxhash`: The hash of the transaction to find.
    ///
    /// Returns:
    /// - `?Nat`: An optional index of the duplicated transaction or null if no duplicate is found.
    public func find_dupe(trxhash: Blob) : ?Nat {
      switch(Map.get<Blob, (Nat64,Nat)>(state.recent_transactions, Map.bhash, trxhash)){
          case(?found){
            if(found.0 + state.permitted_drift + state.transaction_window > get_time64()){
              return ?found.1;
            };
          };
          case(null){};
        };
        return null;
    };

    /// `deduplicate`
    ///
    /// Checks if a transaction request is a duplicate of an existing transaction based on the hashing of its contents.
    /// If a duplicate is found, it returns an error with the transaction index.
    ///
    /// Parameters:
    /// - `tx_req`: The transaction request to check for duplication.
    ///
    /// Returns:
    /// - `Result<(), Nat>`: Returns `#ok` if no duplicate is found, or `#err` with the index of the duplicate.
    public func deduplicate(tx_req : MigrationTypes.Current.TransactionRequest) : Result.Result<(), Nat> {

      let trxhash = Blob.fromArray(RepIndy.hash_val(transfer_req_to_value(tx_req)));
      D.print("attempting to deduplicate" # debug_show(trxhash, tx_req));

      switch(find_dupe(trxhash)){
        case(?found){
          return #err(found);
        };
        case(null){};
      };
        #ok();
    };

    /// `cleanUpRecents`
    ///
    /// Iterates through and removes transactions from the 'recent transactions' index that are no longer within the permitted drift.
    public func cleanUpRecents() : (){
      label clean for(thisItem in Map.entries(state.recent_transactions)){
        if(thisItem.1.0 + state.transaction_window < get_time64()){
          //we can remove this item;
          ignore Map.remove(state.recent_transactions, Map.bhash, thisItem.0);
        } else {
          //items are inserted in order in this map so as soon as we hit a still valid item, the rest of the list should still be valid as well
          break clean;
        };
      };
    };

     /// `checkAccounts`
    ///
    /// Iterates over the ledger accounts and transfers balances below a set threshold to the minting account.
    /// It's meant to clean up small balances and is called periodically according to a set timer.
    public func checkAccounts() : async (){
      D.print("in check accounts");
      if(Map.size(state.accounts) > state.max_accounts){
        D.print("cleaning accounts");
        let comp = func(a : (Account, Nat), b: (Account,Nat)) : Order.Order{
          return Nat.compare(a.1, b.1);
        };
        label clean for(thisItem in Iter.sort(Map.entries(state.accounts), comp)){
          D.print("inspecting item" # debug_show(thisItem));
          let result = await* transfer({
            from_subaccount = thisItem.0.subaccount;
            to = state.minting_account;
            amount = thisItem.1;
            fee = null;
            memo = ?Text.encodeUtf8("clean");
            created_at_time = null;
          }, thisItem.0.owner, true);

          D.print("inspecting result " # debug_show(result));

          switch(result){
            case(#err(_)){
              //don't waste cycles. something is wrong
              //todo: add notification
              return;
            };
            case(_){};
          };

          if(Map.size(state.accounts) <= state.settle_to_accounts ){break clean};
        };
      };
    };

    /// `validate_fee`
    ///
    /// Validates the fee specified in a transaction request against the calculated fee based on the ledger's fee policy.
    ///
    /// Parameters:
    /// - `calculated_fee`: The fee calculated by the ledger for a transaction.
    /// - `opt_fee`: The optional fee specified in the transaction request by the user.
    ///
    /// Returns:
    /// - `Bool`: True if the fee is valid, false if it doesn't meet the required threshold.
    public func validate_fee(
        calculated_fee : MigrationTypes.Current.Balance,
        opt_fee : ?MigrationTypes.Current.Balance,
    ) : Bool {
        switch (opt_fee) {
            case (?tx_fee) {
                if (tx_fee < calculated_fee) {
                    return false;
                };
            };
            case (null) {};
        };

        true;
    };

    /// `get_fee`
    ///
    /// Retrieves the appropriate transfer fee for a given transaction request.
    ///
    /// Parameters:
    /// - `request`: The transfer request which includes the amount and potential fee parameters.
    ///
    /// Returns:
    /// - `Nat`: The required transfer fee for the specified transaction request.
    public func get_fee(request: TransferArgs) : Nat {
      switch(state._fee){
        case(?fee){
          switch(fee){
            case(#Fixed(val)){
              switch(request.fee){
                case(null) val;
                case(?user_fee){
                  Nat.max(val,user_fee);
                };
              };
            };
            case(#Environment){
              switch(environment.get_fee){
                case(?get_fee_env){
                  let val = get_fee_env(state, environment, request);
                  switch(request.fee){
                    case(null) val;
                    case(?user_fee){
                      Nat.max(val,user_fee);
                    };
                  };
                };
                case(_){
                  10000;
                };
              };
            };
          };
        };
        case(null){
          10000;
        };
      };
    };

    /// `validate_request`
    ///
    /// Perform checks against a transfer request to ensure it meets all the criteria for a valid and secure transfer.
    /// Checks include account validation, memo size, balance sufficiency, mint constraints, burn constraints, and deduplication.
    ///
    /// Parameters:
    /// - `tx_req`: The transaction request to validate.
    /// - `calculated_fee`: The calculated fee for the transaction.
    /// - `system_override`: If true, allows bypassing certain checks for system-level operations.
    ///
    /// Returns:
    /// - `Result<(), TransferError>`: Returns `#ok` if the request is valid, or `#err` with the appropriate error if any check fails.
    public func validate_request(
        tx_req : MigrationTypes.Current.TransactionRequest,
        calculated_fee : MigrationTypes.Current.Balance,
        system_override : Bool
    ) : Result.Result<(), MigrationTypes.Current.TransferError> {

        D.print("in validate_request");

        if (tx_req.from == tx_req.to) {
            return #err(
                #GenericError({
                    error_code = 1;
                    message = "The sender cannot have the same account as the recipient.";
                }),
            );
        };

        if (not Account.validate(tx_req.from)) {
            return #err(
                #GenericError({
                    error_code = 2;
                    message = "Invalid account entered for sender. "  # debug_show(tx_req.from);
                }),
            );
        };

        if (not Account.validate(tx_req.to)) {
            return #err(
                #GenericError({
                    error_code = 3;
                    message = "Invalid account entered for recipient " # debug_show(tx_req.to);
                }),
            );
        };

        D.print("Checking memo");

        if (not validate_memo(tx_req.memo)) {
            return #err(
                #GenericError({
                    error_code = 4;
                    message = "Memo must not be more than 32 bytes";
                }),
            );
        };

        if (tx_req.amount == 0) {
            return #err(
                #GenericError({
                    error_code = 5;
                    message = "Amount must be greater than 0";
                }),
            );
        };

        D.print("starting filter");
        label filter switch (tx_req.kind) {
            case (#transfer) {
                D.print("validating fee");
                if (not validate_fee( calculated_fee, tx_req.fee)) {
                    return #err(
                        #BadFee {
                            expected_fee = calculated_fee;
                        },
                    );
                };

                let final_fee = switch(tx_req.fee){
                  case(null) calculated_fee;
                  case(?val) val;
                };

                D.print("getting balance");
                let balance : MigrationTypes.Current.Balance = Utils.get_balance(
                    state.accounts,
                    tx_req.from,
                );

                D.print("found balance" # debug_show(balance));

                if (tx_req.amount + final_fee > balance) {
                    return #err(#InsufficientFunds { balance });
                };
            };

            case (#mint) {

                let ?max_supply = state.max_supply else break filter;
                
                if (max_supply < state._minted_tokens + tx_req.amount) {
                    let remaining_tokens = (max_supply - state._minted_tokens) : Nat;

                    return #err(
                        #GenericError({
                            error_code = 6;
                            message = "Cannot mint more than " # Nat.toText(remaining_tokens) # " tokens";
                        }),
                    );
                };
                
            };
            case (#burn) {

                let balance : MigrationTypes.Current.Balance = Utils.get_balance(
                    state.accounts,
                    tx_req.from,
                );

                if (balance < tx_req.amount) {
                    return #err(#InsufficientFunds { balance });
                };


                
                let ?min_burn_amount = state.min_burn_amount else break filter;
              
                if (system_override == false and tx_req.to == state.minting_account and tx_req.amount < min_burn_amount) {
                    return #err(
                        #BadBurn { min_burn_amount = min_burn_amount },
                    );
                };
            };
        };

        D.print("testing Time");
        switch (tx_req.created_at_time) {
            case (null) {};
            case (?created_at_time) {
                if (is_too_old(created_at_time)) {
                    return #err(#TooOld);
                };

                if (is_in_future(created_at_time)) {
                    return #err(
                        #CreatedInFuture {
                            ledger_time = get_time64();
                        },
                    );
                };

                
            };
        };

        switch (deduplicate(tx_req)) {
            case (#err(tx_index)) {
                return #err(
                    #Duplicate {
                        duplicate_of = tx_index;
                    },
                );
            };
            case (_) {};
        };

        D.print("done validate");
        #ok();
    };


    /// `transfer_req_to_value`
    ///
    /// Converts a transaction request into a `Value` type that can be processed by an ICRC-3 transaction log.
    ///
    /// Parameters:
    /// - `request`: The transaction request to convert.
    ///
    /// Returns:
    /// - `Value`: The transaction request converted to a `Value` type suitable for logs.
    public func transfer_req_to_value(request: TransactionRequest) : Value {
      let trx = Vec.new<(Text, Value)>();

      Vec.add(trx, ("amt",#Nat(request.amount)));

      switch(request.kind){
        case(#mint) {
          Vec.add(trx, ("op",#Text("mint")));
          Vec.add(trx, ("to", Utils.accountToValue(request.to)));
        };
        case(#burn){
          Vec.add(trx, ("op",#Text("burn")));
          Vec.add(trx, ("from", Utils.accountToValue(request.from)));
        };
        case(#transfer){
          Vec.add(trx, ("op",#Text("xfer")));
          Vec.add(trx, ("to", Utils.accountToValue(request.to)));
          Vec.add(trx, ("from", Utils.accountToValue(request.from)));
        };
      };

      switch(request.fee){
        case(null){
        };
        case(?val){
          Vec.add(trx, ("fee", #Nat(val)));
        };
      };

      switch(request.created_at_time){
        case(null){
        };
        case(?val){
          Vec.add(trx, ("ts", #Nat(Nat64.toNat(val))));
        };
      };

      switch(request.memo){
        case(null){
        };
        case(?val){
          Vec.add(trx, ("memo", #Blob(val)));
        };
      };

      let vTrx = #Map(Vec.toArray(trx));

      return vTrx
    };

    /// `get_time64`
    ///
    /// Retrieves the current time in nanoseconds in a 64-bit unsigned integer format.
    ///
    /// Returns:
    /// - `Nat64`: The current ledger time.
    public func get_time64() : Nat64{
      return Utils.get_time64(environment);
    };

    /// `transfer_req_to_value_top`
    ///
    /// Converts a transaction request with an additional layer that includes calculated fee information, meant for ICRC-3 transaction log top layer.
    ///
    /// Parameters:
    /// - `calculated_fee`: The calculated fee for the transaction to include.
    /// - `request`: The transaction request to convert.
    ///
    /// Returns:
    /// - `Value`: The transaction request converted to a top layer `Value` type suitable for logs.
    public func transfer_req_to_value_top(calculated_fee : MigrationTypes.Current.Balance, request: TransactionRequest) : Value {
      let trx = Vec.new<(Text, Value)>();


      switch(request.fee){
        case(null){
          if(calculated_fee > 0){
            Vec.add(trx, ("fee", #Nat(calculated_fee)));
          };
        };
        case(?val){
        };
      };

      Vec.add(trx, ("ts", #Nat(Nat64.toNat(get_time64()))));

      let vTrx = #Map(Vec.toArray(trx));

      return vTrx
    };

    //events

    type Listener<T> = (Text, T);

      /// Generic function to register a listener.
      ///
      /// Parameters:
      ///     namespace: Text - The namespace identifying the listener.
      ///     remote_func: T - A callback function to be invoked.
      ///     listeners: Vec<Listener<T>> - The list of listeners.
      public func register_listener<T>(namespace: Text, remote_func: T, listeners: Vec.Vector<Listener<T>>) {
        let listener: Listener<T> = (namespace, remote_func);
        switch(Vec.indexOf<Listener<T>>(listener, listeners, func(a: Listener<T>, b: Listener<T>) : Bool {
          Text.equal(a.0, b.0);
        })){
          case(?index){
            Vec.put<Listener<T>>(listeners, index, listener);
          };
          case(null){
            Vec.add<Listener<T>>(listeners, listener);
          };
        };
      };

    /// `register_listener`
    ///
    /// Registers a new listener or updates an existing one in the provided `listeners` vector.
    ///
    /// Parameters:
    /// - `namespace`: A unique namespace used to identify the listener.
    /// - `remote_func`: The listener's callback function.
    /// - `listeners`: The vector of existing listeners that the new listener will be added to or updated in.
    public func register_token_transferred_listener(namespace: Text, remote_func : TokenTransferredListener){
      register_listener<TokenTransferredListener>(namespace, remote_func, token_transferred_listeners);
    };
  };
};