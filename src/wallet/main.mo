import Prim "mo:prim";
import Iter "mo:base/Iter";
import Principal "mo:base/Principal";
import Float "mo:base/Float";
import Nat "mo:base/Nat";
import Int "mo:base/Int";
import Nat32 "mo:base/Nat32";
import Array "mo:base/Array";
import HashMap "mo:base/HashMap";
import TrieMap "mo:base/TrieMap";
import List "mo:base/List";
import Time "mo:base/Time";
import Text "mo:base/Text";
import Http "../helpers/http";
import Utils "../helpers/Utils";
import JSON "../helpers/JSON";
import Constants "../Constants";
import Response "../models/Response";
import Member "../models/Member";
import TokenService "../services/TokenService";
import Cycles "mo:base/ExperimentalCycles";
import Result "mo:base/Result";
import Error "mo:base/Error";

actor {

    private type Member = Member.Member;
    private type ApiError = Response.ApiError;

    private stable let startDate = Time.now();
    private stable let day:Int = 86400000000000;
    private stable let month:Int = day * 30;
    private stable let year:Int = month * 12;
    private stable let maxClaims:Int = 36;

    private stable let clif = year; // 12 month clif until vesting starts
    private stable let vestingThreshold = year * 3; //years

    private stable var memberEntries : [(Principal,Member)] = [];
    private var members = HashMap.fromIter<Principal,Member>(memberEntries.vals(), 0, Principal.equal, Principal.hash);

    system func preupgrade() {
        memberEntries := Iter.toArray(members.entries());
    };

    system func postupgrade() {
        memberEntries := [];
    };

    public query func fetchMembers(): async [(Principal,Member)] {
      Iter.toArray(members.entries());
    };

    public shared({caller}) func updateMemberOwner(value:Principal): async Result.Result<(),ApiError> {
      let exist = members.get(caller);
      switch(exist){
        case(?exist){
          let member = {
            owner = value;
            allocation = exist.allocation;
            claimAmount = exist.claimAmount;
            lastClaim = exist.lastClaim;
            claimCount = exist.claimCount;
            payout = exist.payout;
          };
          members.put(caller,member);
          #ok();
        };
        case(null) {
          let principal = Principal.toText(caller);
          #err(#NotFound("No member with principal " #principal #" could be found"));
        };
      };
    };

    public shared({caller}) func updateMemberKey(value:Principal): async Result.Result<(),ApiError> {
      let exist = members.get(caller);
      switch(exist){
        case(?exist){
          members.delete(caller);
          members.put(value,exist);
          #ok();
        };
        case(null) {
          let principal = Principal.toText(caller);
          #err(#NotFound("No member with principal " #principal #" could be found"));
        };
      };
    };

    public shared({caller}) func withdraw(): async TokenService.TxReceipt {
      let now = Time.now();
      let startThreshold = startDate + year;
      assert(now >= startThreshold);
      let exist = members.get(caller);
      switch(exist){
        case(?exist){
          assert(exist.lastClaim < now);
          let claimTime = now - exist.lastClaim;
          if(claimTime >= month) {
            let claimCount = claimTime / month;
            let amount = exist.claimAmount * Utils.textToNat(Int.toText(claimCount));
            let receipt = await _transfer(exist.owner, amount);
            switch(receipt){
              case(#Ok(value)){
                let member = {
                  owner = exist.owner;
                  allocation = exist.allocation;
                  claimAmount = exist.claimAmount;
                  lastClaim = now;
                  claimCount = exist.claimCount + Utils.textToNat(Int.toText(claimCount));
                  payout = exist.payout + amount;
                };
                members.put(caller,member);
                return #Ok(value);
              };
              case(#Err(value)){
                return #Err(value);
              };
            };
            // legal claim
          }else {
            // not legal claim
            #Err(#Unauthorized);
          };
        };
        case(null) {
          let principal = Principal.toText(caller);
          #Err(#Unauthorized);
        };
      };
    };

    private func _transfer(to:Principal, amount:Nat): async TokenService.TxReceipt {
      await TokenService.transfer(to, amount);
    };

    private func _setup() {
      let allocation1 = 3000000000000000000;
      let allocation2 = 1000000000000000000;

      let allidoizcode_principal = Principal.fromText(Constants.allidoizcode);
      let cryptoisgood_principal = Principal.fromText(Constants.cryptoisgood);
      let remco_principal = Principal.fromText(Constants.remco);
      let cajun_principal = Principal.fromText(Constants.cajun);
      let notdom_principal = Principal.fromText(Constants.notdom);

      let allidoizcode = _createMember(allocation1, allidoizcode_principal);
      let cryptoisgood = _createMember(allocation1, cryptoisgood_principal);
      let remco = _createMember(allocation2, remco_principal);
      let cajun = _createMember(allocation2, cajun_principal);
      let notdom = _createMember(allocation2, notdom_principal);

      members.put(allidoizcode_principal,allidoizcode);
      members.put(cryptoisgood_principal,cryptoisgood);
      members.put(remco_principal,remco);
      members.put(cajun_principal,cajun);
      members.put(notdom_principal,notdom);
    };

    private func _createMember(allocation:Nat, owner:Principal): Member {
      {
        owner = owner;
        allocation = allocation;
        claimAmount = allocation / Utils.textToNat(Int.toText(maxClaims));
        lastClaim = Time.now();
        claimCount = 0;
        payout = 0;
      };
    };
    _setup();
};
