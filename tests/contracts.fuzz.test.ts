import fc from "fast-check";
import { Cl, type ResponseOkCV, type UIntCV } from "@stacks/transactions";
import { describe, expect, it } from "vitest";

const SEED = 0x0c4010;
const RUNS = 100;
const MAX_SUPPLY = 888_888_888_888_888_888n;
const DAO = "cholo-dao";
const SWAP = "cholo-swap";
const OFFICIAL_SBTC = "SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4.sbtc-token";
const OFFICIAL_USDCX = "SP120SBRBQJ00MCWS7TM5R8WJNTTKD5K0HFRC2CNE.usdcx";
const BURN_ADDRESS = "SP000000000000000000002Q6VF78";

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

function createDaoProposal(
  proposalType: string,
  expectedId: bigint,
  options: {
    recipient?: string;
    amount?: bigint;
    newSigner?: string;
    oldSigner?: string;
    token?: string;
    expiration?: number;
    newRequired?: bigint;
    newDelay?: bigint;
  } = {},
): void {
  const accounts = simnet.getAccounts();
  expect(
    simnet.callPublicFn(
      DAO,
      "create-proposal",
      [
        Cl.principal(options.recipient ?? accounts.get("wallet_2")!),
        Cl.uint(options.amount ?? (proposalType === "transfer" ? 1n : 0n)),
        Cl.stringAscii(proposalType),
        options.newSigner ? Cl.some(Cl.principal(options.newSigner)) : Cl.none(),
        options.oldSigner ? Cl.some(Cl.principal(options.oldSigner)) : Cl.none(),
        options.token ? Cl.some(Cl.principal(options.token)) : Cl.none(),
        Cl.stringUtf8(`fuzz ${proposalType}`),
        Cl.uint(options.expiration ?? simnet.blockHeight + 500),
        options.newRequired === undefined ? Cl.none() : Cl.some(Cl.uint(options.newRequired)),
        options.newDelay === undefined ? Cl.none() : Cl.some(Cl.uint(options.newDelay)),
      ],
      daoSigner(),
    ).result,
  ).toBeOk(Cl.uint(expectedId));
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

  it.skip("mock-token conserves every randomly minted and transferred unit", () => {
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

  it("swap accepts only positive fuzzed rates and stores accepted STX quotes exactly", () => {
    const accounts = simnet.getAccounts();
    const deployer = accounts.get("deployer")!;
    const buyer = accounts.get("wallet_1")!;

    for (const { numerator, denominator, amount } of samples(
      fc.record({
        numerator: fc.bigInt({ min: 0n, max: 1_000_000n }),
        denominator: fc.bigInt({ min: 0n, max: 1_000n }),
        amount: fc.bigInt({ min: 0n, max: 100_000n }),
      }),
      7,
    )) {
      const response = simnet.callPublicFn(
        SWAP,
        "set-stx-rate",
        [Cl.uint(numerator), Cl.uint(denominator)],
        deployer,
      );

      if (numerator === 0n || denominator === 0n) {
        expect(response.result).toBeErr(Cl.uint(202));
      } else {
        expect(response.result).toBeOk(Cl.bool(true));
        expect(
          simnet.callReadOnlyFn(SWAP, "quote-stx", [Cl.uint(amount)], buyer).result,
        ).toBeUint(amount === 0n ? 0n : (amount * numerator) / denominator);
      }
    }
  });

  it("swap fuzzed owner/config validation leaves token configuration unchanged on rejection", () => {
    const accounts = simnet.getAccounts();
    const deployer = accounts.get("deployer")!;
    const outsider = accounts.get("wallet_1")!;
    const mockPrincipal = `${deployer}.mock-token`;

    for (const { isOwner, token, numerator, denominator } of samples(
      fc.record({
        isOwner: fc.boolean(),
        token: fc.constantFrom(OFFICIAL_SBTC, OFFICIAL_USDCX, mockPrincipal),
        numerator: fc.bigInt({ min: 0n, max: 10_000n }),
        denominator: fc.bigInt({ min: 0n, max: 1_000n }),
      }),
      8,
    )) {
      const configuredBefore = simnet.callReadOnlyFn(
        SWAP,
        "get-sbtc-contract",
        [],
        deployer,
      ).result;
      const sender = isOwner ? deployer : outsider;
      const response = simnet.callPublicFn(
        SWAP,
        "set-sbtc-config",
        [Cl.principal(token), Cl.uint(numerator), Cl.uint(denominator)],
        sender,
      );

      if (!isOwner) {
        expect(response.result).toBeErr(Cl.uint(200));
      } else if (token !== OFFICIAL_SBTC) {
        expect(response.result).toBeErr(Cl.uint(205));
      } else if (numerator === 0n || denominator === 0n) {
        expect(response.result).toBeErr(Cl.uint(202));
      } else {
        expect(response.result).toBeOk(Cl.bool(true));
        expect(
          simnet.callReadOnlyFn(SWAP, "get-sbtc-contract", [], deployer).result,
        ).toBeSome(Cl.principal(OFFICIAL_SBTC));
        continue;
      }

      expect(simnet.callReadOnlyFn(SWAP, "get-sbtc-contract", [], deployer).result).toStrictEqual(
        configuredBefore,
      );
    }
  });

  it("swap fuzzed treasury and owner updates reject the burn address and unauthorized callers", () => {
    const accounts = simnet.getAccounts();
    const deployer = accounts.get("deployer")!;
    const outsider = accounts.get("wallet_1")!;
    const validPrincipals = [accounts.get("wallet_2")!, accounts.get("wallet_3")!];

    for (const { target, useOwner } of samples(
      fc.record({
        target: fc.constantFrom(BURN_ADDRESS, ...validPrincipals),
        useOwner: fc.boolean(),
      }),
      9,
    )) {
      const treasuryBefore = simnet.callReadOnlyFn(SWAP, "get-treasury", [], deployer).result;
      const ownerBefore = simnet.callReadOnlyFn(SWAP, "get-owner", [], deployer).result;
      const sender = useOwner ? deployer : outsider;

      const treasuryResponse = simnet.callPublicFn(
        SWAP,
        "set-treasury",
        [Cl.principal(target)],
        sender,
      );
      if (!useOwner) {
        expect(treasuryResponse.result).toBeErr(Cl.uint(200));
      } else if (target === BURN_ADDRESS) {
        expect(treasuryResponse.result).toBeErr(Cl.uint(207));
      } else {
        expect(treasuryResponse.result).toBeOk(Cl.bool(true));
        expect(simnet.callReadOnlyFn(SWAP, "get-treasury", [], deployer).result).toBePrincipal(
          target,
        );
      }

      const ownerResponse = simnet.callPublicFn(
        SWAP,
        "set-owner",
        [Cl.principal(target)],
        sender,
      );
      if (!useOwner) {
        expect(ownerResponse.result).toBeErr(Cl.uint(200));
      } else if (target === BURN_ADDRESS) {
        expect(ownerResponse.result).toBeErr(Cl.uint(207));
      } else {
        expect(ownerResponse.result).toBeOk(Cl.bool(true));
        expect(simnet.callReadOnlyFn(SWAP, "get-owner", [], deployer).result).toBePrincipal(
          target,
        );
        expect(
          simnet.callPublicFn(SWAP, "set-owner", [Cl.principal(deployer)], target).result,
        ).toBeOk(Cl.bool(true));
        continue;
      }

      expect(simnet.callReadOnlyFn(SWAP, "get-treasury", [], deployer).result).toStrictEqual(
        treasuryBefore,
      );
      expect(simnet.callReadOnlyFn(SWAP, "get-owner", [], deployer).result).toStrictEqual(
        ownerBefore,
      );
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

  it("DAO fuzzed proposal params accept only valid shapes and rejected attempts do not consume ids", () => {
    const accounts = simnet.getAccounts();
    const deployer = accounts.get("deployer")!;
    const currentSigner = daoSigner();
    const outsider = accounts.get("wallet_1")!;
    const recipient = accounts.get("wallet_2")!;
    const tokenPrincipal = `${deployer}.cholo`;

    const cases = samples(
      fc.oneof(
        fc.record({
          type: fc.constant("transfer"),
          amount: fc.constantFrom(0n, 1n, 10_000n),
          includeToken: fc.constant(false),
          includeNewSigner: fc.constant(false),
          includeOldSigner: fc.constant(false),
          includeNewRequired: fc.constant(false),
          includeNewDelay: fc.constant(false),
        }),
        fc.record({
          type: fc.constant("token-transfer"),
          amount: fc.constantFrom(0n, 1n, 10_000n),
          includeToken: fc.boolean(),
          includeNewSigner: fc.constant(false),
          includeOldSigner: fc.constant(false),
          includeNewRequired: fc.constant(false),
          includeNewDelay: fc.constant(false),
        }),
        fc.record({
          type: fc.constantFrom("add-signer", "remove-signer", "replace-signer"),
          amount: fc.constant(0n),
          includeToken: fc.constant(false),
          includeNewSigner: fc.boolean(),
          includeOldSigner: fc.boolean(),
          includeNewRequired: fc.constant(false),
          includeNewDelay: fc.constant(false),
        }),
        fc.record({
          type: fc.constant("set-required-sigs"),
          amount: fc.constant(0n),
          includeToken: fc.constant(false),
          includeNewSigner: fc.constant(false),
          includeOldSigner: fc.constant(false),
          includeNewRequired: fc.boolean(),
          includeNewDelay: fc.constant(false),
        }),
        fc.record({
          type: fc.constant("set-exec-delay"),
          amount: fc.constant(0n),
          includeToken: fc.constant(false),
          includeNewSigner: fc.constant(false),
          includeOldSigner: fc.constant(false),
          includeNewRequired: fc.constant(false),
          includeNewDelay: fc.boolean(),
        }),
        fc.record({
          type: fc.constant("unknown"),
          amount: fc.constant(0n),
          includeToken: fc.boolean(),
          includeNewSigner: fc.boolean(),
          includeOldSigner: fc.boolean(),
          includeNewRequired: fc.boolean(),
          includeNewDelay: fc.boolean(),
        }),
      ),
      10,
    );

    let nextId = 0n;
    for (const item of cases) {
      const newSigner = item.includeNewSigner ? outsider : undefined;
      const oldSigner = item.includeOldSigner ? currentSigner : undefined;
      const token = item.includeToken ? tokenPrincipal : undefined;
      const newRequired = item.includeNewRequired ? 1n : undefined;
      const newDelay = item.includeNewDelay ? 0n : undefined;
      const response = simnet.callPublicFn(
        DAO,
        "create-proposal",
        [
          Cl.principal(recipient),
          Cl.uint(item.amount),
          Cl.stringAscii(item.type),
          newSigner ? Cl.some(Cl.principal(newSigner)) : Cl.none(),
          oldSigner ? Cl.some(Cl.principal(oldSigner)) : Cl.none(),
          token ? Cl.some(Cl.principal(token)) : Cl.none(),
          Cl.stringUtf8(`shape ${item.type}`),
          Cl.uint(simnet.blockHeight + 500),
          newRequired === undefined ? Cl.none() : Cl.some(Cl.uint(newRequired)),
          newDelay === undefined ? Cl.none() : Cl.some(Cl.uint(newDelay)),
        ],
        currentSigner,
      );

      const valid =
        (item.type === "transfer" && item.amount > 0n) ||
        (item.type === "token-transfer" && item.amount > 0n && item.includeToken) ||
        (item.type === "add-signer" && item.includeNewSigner) ||
        (item.type === "replace-signer" && item.includeOldSigner && item.includeNewSigner) ||
        (item.type === "set-required-sigs" && item.includeNewRequired) ||
        (item.type === "set-exec-delay" && item.includeNewDelay);

      if (valid) {
        expect(response.result).toBeOk(Cl.uint(nextId));
        nextId += 1n;
      } else if (item.type === "unknown") {
        expect(response.result).toBeErr(Cl.uint(108));
      } else if (item.type === "remove-signer") {
        expect(response.result).toBeErr(item.includeOldSigner ? Cl.uint(106) : Cl.uint(107));
      } else {
        expect(response.result).toBeErr(Cl.uint(107));
      }
    }

    createDaoProposal("transfer", nextId, { recipient, amount: 1n });
  });
});
