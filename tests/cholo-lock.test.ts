import { Cl, cvToString, type ClarityValue } from "@stacks/transactions";
import { describe, expect, it } from "vitest";

const contract = "cholo-lock";
const account = (name = "wallet_1") => simnet.getAccounts().get(name)!;
const vault = () => `${account("deployer")}.${contract}`;
const call = (fn: string, args: ClarityValue[], sender = account()) =>
  simnet.callPublicFn(contract, fn, args, sender).result;
const read = (fn: string, args: ClarityValue[] = []) =>
  simnet.callReadOnlyFn(contract, fn, args, account()).result;
const height = () => Number(cvToString(read("get-current-height")).slice(1));
const balance = (who: string) => simnet.getAssetsMap().get("STX")!.get(who) ?? 0n;
const lock = (amount = 1000, duration: number | bigint = 10, recipient = account("wallet_2"), tag = "reembolso 🐕") =>
  call("lock-funds", [Cl.uint(amount), Cl.uint(duration), Cl.principal(recipient), Cl.stringUtf8(tag)]);

describe("cholo-lock", () => {
  it("escrows funds and preserves all immutable metadata", () => {
    const before = balance(account());
    expect(lock()).toBeOk(Cl.uint(0));
    expect(balance(account())).toBe(before - 1000n);
    expect(balance(vault())).toBe(1000n);
    expect(read("get-lock", [Cl.uint(0)])).toBeSome(Cl.tuple({
      depositor: Cl.principal(account()), recipient: Cl.principal(account("wallet_2")),
      amount: Cl.uint(1000), tag: Cl.stringUtf8("reembolso 🐕"),
      "created-at": Cl.uint(height()), "unlock-at": Cl.uint(height() + 10), released: Cl.bool(false),
    }));
  });

  it("rejects early release and pays only the recipient at the exact boundary, once", () => {
    simnet.setEpoch("3.0");
    expect(lock()).toBeOk(Cl.uint(0));
    const start = height();
    simnet.mineEmptyStacksBlocks(20);
    expect(height()).toBe(start);
    expect(call("release", [Cl.uint(0)])).toBeErr(Cl.uint(102));
    simnet.mineEmptyBurnBlocks(9);
    expect(call("release", [Cl.uint(0)])).toBeErr(Cl.uint(102));
    simnet.mineEmptyBurnBlocks(1);
    const recipientBefore = balance(account("wallet_2"));
    const callerBefore = balance(account("wallet_3"));
    expect(call("release", [Cl.uint(0)], account("wallet_3"))).toBeOk(Cl.bool(true));
    expect(height()).toBe(start + 10);
    expect(balance(account("wallet_2"))).toBe(recipientBefore + 1000n);
    expect(balance(account("wallet_3"))).toBe(callerBefore);
    expect(balance(vault())).toBe(0n);
    expect(call("release", [Cl.uint(0)])).toBeErr(Cl.uint(103));
  });

  it("rejects invalid deposits without consuming ids or funds", () => {
    const before = balance(account());
    for (const result of [lock(0), lock(1, 0), lock(1, 10, vault()), lock(1, 2n ** 128n - 1n)]) {
      expect(result).toBeErr(Cl.uint(100));
    }
    expect(lock(Number.MAX_SAFE_INTEGER)).toBeErr(Cl.uint(1));
    expect(read("get-next-id")).toBeUint(0);
    expect(balance(account())).toBe(before);
    expect(read("get-lock", [Cl.uint(0)])).toBeNone();
    expect(call("release", [Cl.uint(99)])).toBeErr(Cl.uint(101));
  });

  it("keeps multiple locks independent and allows empty or duplicate tags", () => {
    expect(lock(100, 10, account(), "")).toBeOk(Cl.uint(0));
    expect(lock(200, 100, account("wallet_2"), "same")).toBeOk(Cl.uint(1));
    expect(lock(300, 100, account("wallet_2"), "same")).toBeOk(Cl.uint(2));
    simnet.mineEmptyBurnBlocks(10);
    expect(call("release", [Cl.uint(0)])).toBeOk(Cl.bool(true));
    expect(call("release", [Cl.uint(1)])).toBeErr(Cl.uint(102));
    expect(balance(vault())).toBe(500n);
  });

  it("rejects an intermediary trying to lock the transaction sender's funds", () => {
    simnet.deployContract("lock-relay", `
      (define-public (deposit) (contract-call? .cholo-lock lock-funds u100 u10 tx-sender u"relay"))
    `, { clarityVersion: 2 }, account("deployer"));
    expect(simnet.callPublicFn("lock-relay", "deposit", [], account()).result).toBeErr(Cl.uint(104));
    expect(read("get-next-id")).toBeUint(0);
  });
});
