module {
    public type Member = {
        owner:Principal;
        allocation:Nat;
        claimAmount:Nat;
        lastClaim:Int;
        claimCount:Nat;
        payout:Nat;
    };
}