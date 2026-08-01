import { Cl, type ClarityValue } from "@stacks/transactions";
import { describe, expect, it } from "vitest";

const DAO = "cholo-dao";

function initialSigner(): string {
  return simnet.getAccounts().get("deployer")!;
}

function proposalArgs(
  proposalType = "transfer",
  expiration = simnet.blockHeight + 50,
  options: {
    recipient?: string;
    amount?: number;
    newSigner?: string;
    oldSigner?: string;
    token?: string;
    newRequired?: number;
    newDelay?: number;
  } = {},
): ClarityValue[] {
  const accounts = simnet.getAccounts();
  return [
    Cl.principal(options.recipient ?? accounts.get("wallet_2")!),
    Cl.uint(options.amount ?? (proposalType === "transfer" ? 1 : 0)),
    Cl.stringAscii(proposalType),
    options.newSigner ? Cl.some(Cl.principal(options.newSigner)) : Cl.none(),
    options.oldSigner ? Cl.some(Cl.principal(options.oldSigner)) : Cl.none(),
    options.token ? Cl.some(Cl.principal(options.token)) : Cl.none(),
    Cl.stringUtf8("DAO test proposal"),
    Cl.uint(expiration),
    options.newRequired === undefined
      ? Cl.none()
      : Cl.some(Cl.uint(options.newRequired)),
    options.newDelay === undefined
      ? Cl.none()
      : Cl.some(Cl.uint(options.newDelay)),
  ];
}

function createProposal(
  proposalType = "transfer",
  options: Parameters<typeof proposalArgs>[2] = {},
): void {
  const response = simnet.callPublicFn(
    DAO,
    "create-proposal",
    proposalArgs(proposalType, simnet.blockHeight + 50, options),
    initialSigner(),
  );
  expect(response.result).toBeOk(Cl.uint(0));
}

function approveByQuorum(): void {
  const response = simnet.callPublicFn(
    DAO,
    "approve-proposal",
    [Cl.uint(0)],
    initialSigner(),
  );
  expect(response.result).toBeOk(Cl.bool(true));
}

describe("cholo-dao", () => {
  it("bootstraps the deployer as the sole signer with quorum one", () => {
    const count = simnet.callReadOnlyFn(
      DAO,
      "get-signer-count",
      [],
      initialSigner(),
    );
    const quorum = simnet.callReadOnlyFn(
      DAO,
      "get-required-sigs",
      [],
      initialSigner(),
    );
    const signer = simnet.callReadOnlyFn(
      DAO,
      "is-signer",
      [Cl.principal(initialSigner())],
      initialSigner(),
    );

    expect(count.result).toBeUint(1);
    expect(quorum.result).toBeUint(1);
    expect(signer.result).toBeBool(true);
  });

  it("rejects proposal creation by a non-signer", () => {
    const outsider = simnet.getAccounts().get("wallet_1")!;
    const response = simnet.callPublicFn(
      DAO,
      "create-proposal",
      proposalArgs(),
      outsider,
    );

    expect(response.result).toBeErr(Cl.uint(100));
  });

  it("enforces proposal expiration bounds", () => {
    const tooSoon = simnet.callPublicFn(
      DAO,
      "create-proposal",
      proposalArgs("transfer", simnet.blockHeight + 9),
      initialSigner(),
    );
    const tooLate = simnet.callPublicFn(
      DAO,
      "create-proposal",
      proposalArgs("transfer", simnet.blockHeight + 10_002),
      initialSigner(),
    );

    expect(tooSoon.result).toBeErr(Cl.uint(107));
    expect(tooLate.result).toBeErr(Cl.uint(107));
  });

  it("records approvals and rejects duplicate or unauthorized approvals", () => {
    createProposal();

    const approved = simnet.callPublicFn(
      DAO,
      "approve-proposal",
      [Cl.uint(0)],
      initialSigner(),
    );
    const duplicate = simnet.callPublicFn(
      DAO,
      "approve-proposal",
      [Cl.uint(0)],
      initialSigner(),
    );
    const outsider = simnet.callPublicFn(
      DAO,
      "approve-proposal",
      [Cl.uint(0)],
      simnet.getAccounts().get("wallet_1")!,
    );

    expect(approved.result).toBeOk(Cl.bool(true));
    expect(duplicate.result).toBeErr(Cl.uint(103));
    expect(outsider.result).toBeErr(Cl.uint(100));
  });

  it("requires quorum and the execution delay", () => {
    createProposal("set-exec-delay", { newDelay: 10 });

    const beforeQuorum = simnet.callPublicFn(
      DAO,
      "execute-proposal",
      [Cl.uint(0), Cl.none()],
      initialSigner(),
    );
    expect(beforeQuorum.result).toBeErr(Cl.uint(102));

    approveByQuorum();
    const beforeDelay = simnet.callPublicFn(
      DAO,
      "execute-proposal",
      [Cl.uint(0), Cl.none()],
      initialSigner(),
    );
    expect(beforeDelay.result).toBeErr(Cl.uint(107));

    simnet.mineEmptyBlocks(10);
    const executed = simnet.callPublicFn(
      DAO,
      "execute-proposal",
      [Cl.uint(0), Cl.none()],
      initialSigner(),
    );
    const replay = simnet.callPublicFn(
      DAO,
      "execute-proposal",
      [Cl.uint(0), Cl.none()],
      initialSigner(),
    );

    expect(executed.result).toBeOk(Cl.bool(true));
    expect(replay.result).toBeErr(Cl.uint(101));
  });

  it("deposits and transfers STX from the treasury", () => {
    const accounts = simnet.getAccounts();
    const depositor = accounts.get("wallet_1")!;
    const recipient = accounts.get("wallet_2")!;

    const deposited = simnet.callPublicFn(
      DAO,
      "deposit",
      [Cl.uint(1_000)],
      depositor,
    );
    expect(deposited.result).toBeOk(Cl.bool(true));

    createProposal("transfer", { recipient, amount: 400 });
    approveByQuorum();
    simnet.mineEmptyBlocks(10);

    const executed = simnet.callPublicFn(
      DAO,
      "execute-proposal",
      [Cl.uint(0), Cl.none()],
      initialSigner(),
    );
    expect(executed.result).toBeOk(Cl.bool(true));
  });

  it("transfers SIP-010 tokens from the treasury", () => {
    const accounts = simnet.getAccounts();
    const deployer = accounts.get("deployer")!;
    const recipient = accounts.get("wallet_2")!;
    const daoPrincipal = `${deployer}.${DAO}`;
    const tokenPrincipal = `${deployer}.cholo`;

    const funded = simnet.callPublicFn(
      "cholo",
      "transfer",
      [
        Cl.uint(1_000),
        Cl.principal(deployer),
        Cl.principal(daoPrincipal),
        Cl.none(),
      ],
      deployer,
    );
    expect(funded.result).toBeOk(Cl.bool(true));

    createProposal("token-transfer", {
      recipient,
      amount: 400,
      token: tokenPrincipal,
    });
    approveByQuorum();
    simnet.mineEmptyBlocks(10);

    const executed = simnet.callPublicFn(
      DAO,
      "execute-proposal",
      [Cl.uint(0), Cl.some(Cl.contractPrincipal(deployer, "cholo"))],
      initialSigner(),
    );
    const balance = simnet.callReadOnlyFn(
      "cholo",
      "get-balance",
      [Cl.principal(recipient)],
      recipient,
    );

    expect(executed.result).toBeOk(Cl.bool(true));
    expect(balance.result).toBeOk(Cl.uint(400));
  });

  it("changes the fixed quorum through an approved proposal", () => {
    createProposal("set-required-sigs", { newRequired: 1 });
    approveByQuorum();
    simnet.mineEmptyBlocks(10);

    const executed = simnet.callPublicFn(
      DAO,
      "execute-proposal",
      [Cl.uint(0), Cl.none()],
      initialSigner(),
    );
    const quorum = simnet.callReadOnlyFn(
      DAO,
      "get-required-sigs",
      [],
      initialSigner(),
    );

    expect(executed.result).toBeOk(Cl.bool(true));
    expect(quorum.result).toBeUint(1);
  });

  it("adds a signer and keeps both signer indexes consistent", () => {
    const newSigner = simnet.getAccounts().get("wallet_3")!;
    createProposal("add-signer", { newSigner });
    approveByQuorum();

    const executed = simnet.callPublicFn(
      DAO,
      "execute-proposal",
      [Cl.uint(0), Cl.none()],
      initialSigner(),
    );
    const count = simnet.callReadOnlyFn(
      DAO,
      "get-signer-count",
      [],
      initialSigner(),
    );
    const signer = simnet.callReadOnlyFn(
      DAO,
      "is-signer",
      [Cl.principal(newSigner)],
      initialSigner(),
    );
    const indexedSigner = simnet.callReadOnlyFn(
      DAO,
      "get-signer",
      [Cl.uint(1)],
      initialSigner(),
    );

    expect(executed.result).toBeOk(Cl.bool(true));
    expect(count.result).toBeUint(2);
    expect(signer.result).toBeBool(true);
    expect(indexedSigner.result).toBeSome(Cl.principal(newSigner));
  });

  it("restores the timelock after the bootstrap signer adds member two", () => {
    const accounts = simnet.getAccounts();
    const second = accounts.get("wallet_1")!;
    const third = accounts.get("wallet_2")!;

    createProposal("add-signer", { newSigner: second });
    approveByQuorum();
    expect(
      simnet.callPublicFn(
        DAO,
        "execute-proposal",
        [Cl.uint(0), Cl.none()],
        initialSigner(),
      ).result,
    ).toBeOk(Cl.bool(true));

    const created = simnet.callPublicFn(
      DAO,
      "create-proposal",
      proposalArgs("add-signer", simnet.blockHeight + 50, { newSigner: third }),
      initialSigner(),
    );
    expect(created.result).toBeOk(Cl.uint(1));
    expect(
      simnet.callPublicFn(
        DAO,
        "approve-proposal",
        [Cl.uint(1)],
        initialSigner(),
      ).result,
    ).toBeOk(Cl.bool(true));
    expect(
      simnet.callPublicFn(
        DAO,
        "approve-proposal",
        [Cl.uint(1)],
        second,
      ).result,
    ).toBeOk(Cl.bool(true));

    expect(
      simnet.callPublicFn(
        DAO,
        "execute-proposal",
        [Cl.uint(1), Cl.none()],
        initialSigner(),
      ).result,
    ).toBeErr(Cl.uint(107));
  });

  it("protects the bootstrap signer and rejects duplicate growth proposals", () => {
    expect(
      simnet.callPublicFn(
        DAO,
        "create-proposal",
        proposalArgs("remove-signer", simnet.blockHeight + 50, {
          oldSigner: initialSigner(),
        }),
        initialSigner(),
      ).result,
    ).toBeErr(Cl.uint(106));

    expect(
      simnet.callPublicFn(
        DAO,
        "create-proposal",
        proposalArgs("add-signer", simnet.blockHeight + 50, {
          newSigner: initialSigner(),
        }),
        initialSigner(),
      ).result,
    ).toBeErr(Cl.uint(107));
  });

  it("rejects malformed proposals at creation", () => {
    const response = simnet.callPublicFn(
      DAO,
      "create-proposal",
      proposalArgs("not-a-real-type"),
      initialSigner(),
    );
    expect(response.result).toBeErr(Cl.uint(108));

    expect(
      simnet.callPublicFn(
        DAO,
        "create-proposal",
        proposalArgs("transfer", simnet.blockHeight + 50, { amount: 0 }),
        initialSigner(),
      ).result,
    ).toBeErr(Cl.uint(107));

    expect(
      simnet.callPublicFn(
        DAO,
        "create-proposal",
        proposalArgs("token-transfer", simnet.blockHeight + 50, { amount: 1 }),
        initialSigner(),
      ).result,
    ).toBeErr(Cl.uint(107));
  });

  it("rejects zero deposits and authorizes before proposal lookup", () => {
    const outsider = simnet.getAccounts().get("wallet_1")!;
    expect(
      simnet.callPublicFn(DAO, "deposit", [Cl.uint(0)], outsider).result,
    ).toBeErr(Cl.uint(107));
    expect(
      simnet.callPublicFn(DAO, "approve-proposal", [Cl.uint(999)], outsider).result,
    ).toBeErr(Cl.uint(100));
  });
});
