import { Cl, type ClarityValue } from "@stacks/transactions";
import { describe, expect, it } from "vitest";

const DAO = "cholo-dao";
const SIGNERS = [
  "SP193GXQTNHVV9WSAPHAB89M6R9QSEXZKS3774CMD",
  "ST2YDY8H45J5HTN5M0H2XQH0JFCR4RWCA92QCZ7W6",
  "ST4ZB0M2ZKP1HRZPVAPE4X14K689X22N29YQQBG2",
] as const;

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
    Cl.uint(options.amount ?? 0),
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
    SIGNERS[0],
  );
  expect(response.result).toBeOk(Cl.uint(0));
}

function approveByQuorum(): void {
  for (const signer of SIGNERS) {
    const response = simnet.callPublicFn(
      DAO,
      "approve-proposal",
      [Cl.uint(0)],
      signer,
    );
    expect(response.result).toBeOk(Cl.bool(true));
  }
}

describe("cholo-dao", () => {
  it("initializes five signers and computes a 51% quorum", () => {
    const count = simnet.callReadOnlyFn(
      DAO,
      "get-signer-count",
      [],
      SIGNERS[0],
    );
    const quorum = simnet.callReadOnlyFn(
      DAO,
      "get-required-sigs",
      [],
      SIGNERS[0],
    );
    const signer = simnet.callReadOnlyFn(
      DAO,
      "is-signer",
      [Cl.principal(SIGNERS[0])],
      SIGNERS[0],
    );

    expect(count.result).toBeUint(5);
    expect(quorum.result).toBeUint(3);
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
      SIGNERS[0],
    );
    const tooLate = simnet.callPublicFn(
      DAO,
      "create-proposal",
      proposalArgs("transfer", simnet.blockHeight + 10_002),
      SIGNERS[0],
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
      SIGNERS[0],
    );
    const duplicate = simnet.callPublicFn(
      DAO,
      "approve-proposal",
      [Cl.uint(0)],
      SIGNERS[0],
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
      SIGNERS[0],
    );
    expect(beforeQuorum.result).toBeErr(Cl.uint(102));

    approveByQuorum();
    const beforeDelay = simnet.callPublicFn(
      DAO,
      "execute-proposal",
      [Cl.uint(0), Cl.none()],
      SIGNERS[0],
    );
    expect(beforeDelay.result).toBeErr(Cl.uint(107));

    simnet.mineEmptyBlocks(10);
    const executed = simnet.callPublicFn(
      DAO,
      "execute-proposal",
      [Cl.uint(0), Cl.none()],
      SIGNERS[0],
    );
    const replay = simnet.callPublicFn(
      DAO,
      "execute-proposal",
      [Cl.uint(0), Cl.none()],
      SIGNERS[0],
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
      SIGNERS[0],
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
      SIGNERS[0],
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
    createProposal("set-required-sigs", { newRequired: 4 });
    approveByQuorum();
    simnet.mineEmptyBlocks(10);

    const executed = simnet.callPublicFn(
      DAO,
      "execute-proposal",
      [Cl.uint(0), Cl.none()],
      SIGNERS[0],
    );
    const quorum = simnet.callReadOnlyFn(
      DAO,
      "get-required-sigs",
      [],
      SIGNERS[0],
    );

    expect(executed.result).toBeOk(Cl.bool(true));
    expect(quorum.result).toBeUint(4);
  });

  it("adds a signer and keeps both signer indexes consistent", () => {
    const newSigner = simnet.getAccounts().get("wallet_3")!;
    createProposal("add-signer", { newSigner });
    approveByQuorum();
    simnet.mineEmptyBlocks(10);

    const executed = simnet.callPublicFn(
      DAO,
      "execute-proposal",
      [Cl.uint(0), Cl.none()],
      SIGNERS[0],
    );
    const count = simnet.callReadOnlyFn(
      DAO,
      "get-signer-count",
      [],
      SIGNERS[0],
    );
    const signer = simnet.callReadOnlyFn(
      DAO,
      "is-signer",
      [Cl.principal(newSigner)],
      SIGNERS[0],
    );
    const indexedSigner = simnet.callReadOnlyFn(
      DAO,
      "get-signer",
      [Cl.uint(5)],
      SIGNERS[0],
    );

    expect(executed.result).toBeOk(Cl.bool(true));
    expect(count.result).toBeUint(6);
    expect(signer.result).toBeBool(true);
    expect(indexedSigner.result).toBeSome(Cl.principal(newSigner));
  });

  it("rejects an unknown proposal type at execution", () => {
    createProposal("not-a-real-type");
    approveByQuorum();
    simnet.mineEmptyBlocks(10);

    const response = simnet.callPublicFn(
      DAO,
      "execute-proposal",
      [Cl.uint(0), Cl.none()],
      SIGNERS[0],
    );

    expect(response.result).toBeErr(Cl.uint(108));
  });
});
