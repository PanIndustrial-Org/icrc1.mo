import Array "mo:base/Array";
import Blob "mo:base/Blob";
import Debug "mo:base/Debug";
import Iter "mo:base/Iter";
import Nat8 "mo:base/Nat8";
import Principal "mo:base/Principal";

import Itertools "mo:itertools/Iter";

import Account "../../src/ICRC1/Account";
import ActorSpec "../utils/ActorSpec";

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

let principal = Principal.fromText("prb4z-5pc7u-zdfqi-cgv7o-fdyqf-n6afm-xh6hz-v4bk4-kpg3y-rvgxf-iae");

let success = run([
    describe(
        "Account",
        [
            describe(
                "encode / decode Account",
                [
                    
                    it(
                        "should return false for invalid subaccount (length < 32)",
                        do {

                            var len = 0;
                            var is_valid = false;

                            label _loop while (len < 32){
                                let account = {
                                    owner = principal;
                                    subaccount = ?Blob.fromArray(Array.tabulate(len, Nat8.fromNat));
                                };

                                is_valid := is_valid or Account.validate(account) 
                                            or Account.validate_subaccount(account.subaccount);

                                if (is_valid) {
                                    break _loop;
                                };

                                len += 1;
                            };
                            
                            not is_valid;
                        }
                    )
                ],
            ),
        ],
    ),
]);

if (success == false) {
    Debug.trap("\1b[46;41mTests failed\1b[0m");
} else {
    Debug.print("\1b[23;42;3m Success!\1b[0m");
};
