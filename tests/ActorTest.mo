import Debug "mo:base/Debug";

import ICRC1 "ICRC1/ICRC1.ActorTest";

import ActorSpec "./utils/ActorSpec";

actor {
    let { run } = ActorSpec;

    let test_modules = [
        ICRC1.test,
        ICRC1.testInitInvalidParameters
    ];

    public func runTests() : async () {
        for (test in test_modules.vals()) {
            let success = ActorSpec.run([await test()]);

            if (success == false) {
                Debug.trap("\1b[46;41mTests failed\1b[0m");
            } else {
                Debug.print("\1b[23;42;3m Success!\1b[0m");
            };
        };
    };
};
