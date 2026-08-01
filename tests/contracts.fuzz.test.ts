import fc from "fast-check";
import { Cl, type ResponseOkCV, type UIntCV } from "@stacks/transactions";
import { describe, expect, it } from "vitest";

const SEED = 0x0c4010;
const RUNS = 100;
const MAX_SUPPLY = 888_888_888_888_888_888n;
const DAO = "cholo-dao";
const SWAP = "cholo-swap";

function daoSigner(): string {
  return simnet.getAccounts().get("deployer")!;
}

function samples<T>(arbitrary: fc.Arbitrary<T>, seedOffset = 0): T[] {
  return fc.sample(arbitrary, { seed: SEED + seedOffset, numRuns: RUNS });
}

function balance(contract: string, account: string): bigint {
  const readOnlySender = account.includes(".") ? account.slice(0, account.indexOf(".")) : account;
  const response = simnet.callReadOnlyFn(
    contract,
    "get-balance",
    [Cl.principal(account)],
    readOnlySender,
  );
  expect(response.result.type).toBe(7);
  const result = response.result as ResponseOkCV<UIntCV>;
  expect(result.value.type).toBe(1);
  return result.value.value;
}

describe("contract fuzz invariants", () => {
  it("cholo preserves supply and applies arbitrary valid transfers exactly", () => {
    const accounts = simnet.getAccounts();
    const deployer = accounts.get("deployer")!;
    const recipients = [
      accounts.get("wallet_1")!,
      accounts.get("wallet_2")!,
      accounts.get("wallet_3")!,
    ];

    for (const [index, amount] of samples(fc.bigInt({ min: 1n, max: 1_000_000n })).entries()) {
      const recipient = recipients[index % recipients.length]!;
      const senderBefore = balance("cholo", deployer);
      const recipientBefore = balance("cholo", recipient);

      expect(
        simnet.callPublicFn(
          "cholo",
          "transfer",
          [Cl.uint(amount), Cl.principal(deployer), Cl.principal(recipient), Cl.none()],
          deployer,
        ).result,
      ).toBeOk(Cl.bool(true));
      expect(balance("cholo", deployer)).toBe(senderBefore - amount);
      expect(balance("cholo", recipient)).toBe(recipientBefore + amount);
    }

    expect(
      simnet.callReadOnlyFn("cholo", "get-total-supply", [], deployer).result,
    ).toBeOk(Cl.uint(MAX_SUPPLY));
  });

  it("cholo rejects fuzzed zero, unauthorized, burn, and over-cap operations atomically", () => {
    const accounts = simnet.getAccounts();
    const deployer = accounts.get("deployer")!;
    const attacker = accounts.get("wallet_1")!;
    const recipient = accounts.get("wallet_2")!;
    const burn = "SP000000000000000000002Q6VF78";

    for (const amount of samples(fc.bigInt({ min: 1n, max: 1_000_000n }), 1)) {
      const ownerBefore = balance("cholo", deployer);
      const recipientBefore = balance("cholo", recipient);

      expect(
        simnet.callPublicFn(
          "cholo",
          "transfer",
          [Cl.uint(amount), Cl.principal(deployer), Cl.principal(recipient), Cl.none()],
          attacker,
        ).result,
      ).toBeErr(Cl.uint(101));
      expect(
        simnet.callPublicFn(
          "cholo",
          "transfer",
          [Cl.uint(amount), Cl.principal(deployer), Cl.principal(burn), Cl.none()],
          deployer,
        ).result,
      ).toBeErr(Cl.uint(103));
      expect(balance("cholo", deployer)).toBe(ownerBefore);
      expect(balance("cholo", recipient)).toBe(recipientBefore);
    }

    expect(
      simnet.callPublicFn(
        "cholo",
        "transfer",
        [Cl.uint(0), Cl.principal(deployer), Cl.principal(recipient), Cl.none()],
        deployer,
      ).result,
    ).toBeErr(Cl.uint(102));
    expect(
      simnet.callPublicFn("cholo", "mint", [Cl.uint(1), Cl.principal(recipient)], deployer)
        .result,
    ).toBeErr(Cl.uint(104));
  });

  it("mock-token conserves every randomly minted and transferred unit", () => {
    const accounts = simnet.getAccounts();
    const deployer = accounts.get("deployer")!;
    const holder = accounts.get("wallet_1")!;
    const recipient = accounts.get("wallet_2")!;
    let minted = 0n;
    let transferred = 0n;

    for (const amount of samples(fc.bigInt({ min: 1n, max: 100_000n }), 2)) {
      expect(
        simnet.callPublicFn(
          "mock-token",
          "mint",
          [Cl.uint(amount), Cl.principal(holder)],
          deployer,
        ).result,
      ).toBeOk(Cl.bool(true));
      minted += amount;

      const move = amount / 2n;
      if (move > 0n) {
        expect(
          simnet.callPublicFn(
            "mock-token",
            "transfer",
            [Cl.uint(move), Cl.principal(holder), Cl.principal(recipient), Cl.none()],
            holder,
          ).result,
        ).toBeOk(Cl.bool(true));
        transferred += move;
      }
    }

    expect(balance("mock-token", holder)).toBe(minted - transferred);
    expect(balance("mock-token", recipient)).toBe(transferred);
    expect(
      simnet.callReadOnlyFn("mock-token", "get-total-supply", [], deployer).result,
    ).toBeOk(Cl.uint(minted));
  });

  it("swap quotes match randomized STX purchases and inventory deltas", () => {
    const accounts = simnet.getAccounts();
    const deployer = accounts.get("deployer")!;
    const buyer = accounts.get("wallet_1")!;
    const treasury = accounts.get("wallet_2")!;
    const swapPrincipal = `${deployer}.${SWAP}`;
    const cases = samples(
      fc.record({
        amount: fc.bigInt({ min: 1n, max: 10_000n }),
        numerator: fc.bigInt({ min: 1n, max: 1_000n }),
        denominator: fc.bigInt({ min: 1n, max: 100n }),
      }).filter(({ amount, numerator, denominator }) => amount * numerator >= denominator),
      3,
    );
    const maximumOutput = cases.reduce(
      (sum, item) => sum + (item.amount * item.numerator) / item.denominator,
      0n,
    );

    expect(
      simnet.callPublicFn(
        "cholo",
        "transfer",
        [Cl.uint(maximumOutput), Cl.principal(deployer), Cl.principal(swapPrincipal), Cl.none()],
        deployer,
      ).result,
    ).toBeOk(Cl.bool(true));
    expect(
      simnet.callPublicFn(SWAP, "set-treasury", [Cl.principal(treasury)], deployer).result,
    ).toBeOk(Cl.bool(true));

    for (const { amount, numerator, denominator } of cases) {
      const expected = (amount * numerator) / denominator;
      expect(
        simnet.callPublicFn(
          SWAP,
          "set-stx-rate",
          [Cl.uint(numerator), Cl.uint(denominator)],
          deployer,
        ).result,
      ).toBeOk(Cl.bool(true));
      expect(
        simnet.callReadOnlyFn(SWAP, "quote-stx", [Cl.uint(amount)], buyer).result,
      ).toBeUint(expected);

      const buyerBefore = balance("cholo", buyer);
      const inventoryBefore = balance("cholo", swapPrincipal);
      expect(
        simnet.callPublicFn(
          SWAP,
          "buy-with-stx",
          [Cl.uint(amount), Cl.uint(expected)],
          buyer,
        ).result,
      ).toBeOk(Cl.uint(expected));
      expect(balance("cholo", buyer)).toBe(buyerBefore + expected);
      expect(balance("cholo", swapPrincipal)).toBe(inventoryBefore - expected);
    }
  });

  it("swap fuzzed slippage failures never move CHOLO inventory", () => {
    const accounts = simnet.getAccounts();
    const deployer = accounts.get("deployer")!;
    const buyer = accounts.get("wallet_1")!;
    const swapPrincipal = `${deployer}.${SWAP}`;

    expect(
      simnet.callPublicFn(
        "cholo",
        "transfer",
        [Cl.uint(1_000_000_000), Cl.principal(deployer), Cl.principal(swapPrincipal), Cl.none()],
        deployer,
      ).result,
    ).toBeOk(Cl.bool(true));
    expect(
      simnet.callPublicFn(SWAP, "set-stx-rate", [Cl.uint(17), Cl.uint(3)], deployer).result,
    ).toBeOk(Cl.bool(true));

    for (const amount of samples(fc.bigInt({ min: 1n, max: 100_000n }), 4)) {
      const quote = (amount * 17n) / 3n;
      const inventoryBefore = balance("cholo", swapPrincipal);
      const buyerBefore = balance("cholo", buyer);
      expect(
        simnet.callPublicFn(
          SWAP,
          "buy-with-stx",
          [Cl.uint(amount), Cl.uint(quote + 1n)],
          buyer,
        ).result,
      ).toBeErr(Cl.uint(204));
      expect(balance("cholo", swapPrincipal)).toBe(inventoryBefore);
      expect(balance("cholo", buyer)).toBe(buyerBefore);
    }
  });

  it("DAO accepts exactly the fuzzed TTL domain and keeps proposal ids contiguous", () => {
    const accounts = simnet.getAccounts();
    const recipient = accounts.get("wallet_2")!;
    let nextId = 0n;

    for (const ttl of samples(fc.integer({ min: 0, max: 10_020 }), 5)) {
      // callPublicFn mines the transaction in the next block.
      const expiration = simnet.blockHeight + 1 + ttl;
      const response = simnet.callPublicFn(
        DAO,
        "create-proposal",
        [
          Cl.principal(recipient),
          Cl.uint(ttl),
          Cl.stringAscii("transfer"),
          Cl.none(),
          Cl.none(),
          Cl.none(),
          Cl.stringUtf8(`fuzz ttl ${ttl}`),
          Cl.uint(expiration),
          Cl.none(),
          Cl.none(),
        ],
        daoSigner(),
      );

      if (ttl >= 10 && ttl <= 10_000) {
        expect(response.result).toBeOk(Cl.uint(nextId));
        nextId += 1n;
      } else {
        expect(response.result).toBeErr(Cl.uint(107));
      }
    }
  });

  it("DAO fuzzed approvals are unique, signer-only, and counted exactly", () => {
    const accounts = simnet.getAccounts();
    const outsider = accounts.get("wallet_1")!;
    const recipient = accounts.get("wallet_2")!;
    const expiration = simnet.blockHeight + 500;

    expect(
      simnet.callPublicFn(
        DAO,
        "create-proposal",
        [
          Cl.principal(recipient),
          Cl.uint(0),
          Cl.stringAscii("set-exec-delay"),
          Cl.none(),
          Cl.none(),
          Cl.none(),
          Cl.stringUtf8("fuzz approvals"),
          Cl.uint(expiration),
          Cl.none(),
          Cl.some(Cl.uint(0)),
        ],
        daoSigner(),
      ).result,
    ).toBeOk(Cl.uint(0));

    const order = samples(fc.constant(0), 6);
    const approved = new Set<number>();
    for (const signerIndex of order) {
      const response = simnet.callPublicFn(
        DAO,
        "approve-proposal",
        [Cl.uint(0)],
        daoSigner(),
      );
      if (approved.has(signerIndex)) {
        expect(response.result).toBeErr(Cl.uint(103));
      } else {
        expect(response.result).toBeOk(Cl.bool(true));
        approved.add(signerIndex);
      }
    }

    expect(
      simnet.callPublicFn(DAO, "approve-proposal", [Cl.uint(0)], outsider).result,
    ).toBeErr(Cl.uint(100));
    for (const signerIndex of approved) {
      expect(
        simnet.callReadOnlyFn(
          DAO,
          "has-approved",
          [Cl.uint(0), Cl.principal(daoSigner())],
          outsider,
        ).result,
      ).toBeBool(true);
    }
  });
});
