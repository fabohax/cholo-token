import { Cl } from "@stacks/transactions";
import { describe, expect, it } from "vitest";

const SWAP = "cholo-swap";

function setup() {
  const accounts = simnet.getAccounts();
  const deployer = accounts.get("deployer")!;
  const buyer = accounts.get("wallet_1")!;
  const treasury = accounts.get("wallet_2")!;
  const swapPrincipal = `${deployer}.${SWAP}`;
  const mockPrincipal = `${deployer}.mock-token`;

  expect(
    simnet.callPublicFn(
      "cholo",
      "transfer",
      [Cl.uint(10_000_000), Cl.principal(deployer), Cl.principal(swapPrincipal), Cl.none()],
      deployer,
    ).result,
  ).toBeOk(Cl.bool(true));
  expect(
    simnet.callPublicFn(SWAP, "set-treasury", [Cl.principal(treasury)], deployer).result,
  ).toBeOk(Cl.bool(true));
  expect(
    simnet.callPublicFn(SWAP, "set-stx-rate", [Cl.uint(100), Cl.uint(1)], deployer).result,
  ).toBeOk(Cl.bool(true));
  expect(
    simnet.callPublicFn(
      SWAP,
      "set-sbtc-config",
      [Cl.principal(mockPrincipal), Cl.uint(2), Cl.uint(1)],
      deployer,
    ).result,
  ).toBeOk(Cl.bool(true));
  expect(
    simnet.callPublicFn(
      SWAP,
      "set-usdcx-config",
      [Cl.principal(mockPrincipal), Cl.uint(50), Cl.uint(1)],
      deployer,
    ).result,
  ).toBeOk(Cl.bool(true));

  return { deployer, buyer, treasury, swapPrincipal, mockPrincipal };
}

describe("cholo-swap", () => {
  it("quotes and buys CHOLO with STX", () => {
    const { buyer } = setup();

    expect(
      simnet.callReadOnlyFn(SWAP, "quote-stx", [Cl.uint(1_000)], buyer).result,
    ).toBeUint(100_000);
    expect(
      simnet.callPublicFn(
        SWAP,
        "buy-with-stx",
        [Cl.uint(1_000), Cl.uint(100_000)],
        buyer,
      ).result,
    ).toBeOk(Cl.uint(100_000));
    expect(
      simnet.callReadOnlyFn("cholo", "get-balance", [Cl.principal(buyer)], buyer).result,
    ).toBeOk(Cl.uint(100_000));
  });

  it("buys with configured sBTC and sends payment to treasury", () => {
    const { buyer, treasury, deployer } = setup();
    expect(
      simnet.callPublicFn("mock-token", "mint", [Cl.uint(1_000), Cl.principal(buyer)], deployer)
        .result,
    ).toBeOk(Cl.bool(true));

    expect(
      simnet.callPublicFn(
        SWAP,
        "buy-with-sbtc",
        [Cl.uint(1_000), Cl.uint(2_000), Cl.contractPrincipal(deployer, "mock-token")],
        buyer,
      ).result,
    ).toBeOk(Cl.uint(2_000));
    expect(
      simnet.callReadOnlyFn("mock-token", "get-balance", [Cl.principal(treasury)], buyer).result,
    ).toBeOk(Cl.uint(1_000));
  });

  it("buys with configured USDCx", () => {
    const { buyer, deployer } = setup();
    expect(
      simnet.callPublicFn("mock-token", "mint", [Cl.uint(100), Cl.principal(buyer)], deployer)
        .result,
    ).toBeOk(Cl.bool(true));

    expect(
      simnet.callPublicFn(
        SWAP,
        "buy-with-usdcx",
        [Cl.uint(100), Cl.uint(5_000), Cl.contractPrincipal(deployer, "mock-token")],
        buyer,
      ).result,
    ).toBeOk(Cl.uint(5_000));
  });

  it("enforces minimum output and pause state", () => {
    const { buyer, deployer } = setup();
    expect(
      simnet.callPublicFn(
        SWAP,
        "buy-with-stx",
        [Cl.uint(1_000), Cl.uint(100_001)],
        buyer,
      ).result,
    ).toBeErr(Cl.uint(204));

    expect(
      simnet.callPublicFn(SWAP, "set-paused", [Cl.bool(true)], deployer).result,
    ).toBeOk(Cl.bool(true));
    expect(
      simnet.callPublicFn(
        SWAP,
        "buy-with-stx",
        [Cl.uint(1_000), Cl.uint(0)],
        buyer,
      ).result,
    ).toBeErr(Cl.uint(203));
  });

  it("restricts configuration and inventory withdrawal to the owner", () => {
    const { buyer } = setup();
    expect(
      simnet.callPublicFn(SWAP, "set-stx-rate", [Cl.uint(1), Cl.uint(1)], buyer).result,
    ).toBeErr(Cl.uint(200));
    expect(
      simnet.callPublicFn(
        SWAP,
        "withdraw-cholo",
        [Cl.uint(1), Cl.principal(buyer)],
        buyer,
      ).result,
    ).toBeErr(Cl.uint(200));
  });
});
