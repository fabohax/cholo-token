# $CHOLO

Smart contracts for **$CHOLO**, a Latin American community token built on
[Stacks](https://www.stacks.co/) and anchored in the Bitcoin ecosystem.

$CHOLO takes its identity from the Peruvian hairless dog—a cultural symbol of
Peru—and combines meme culture with an open-source mission around public goods,
DeSci, research and development, and community-led projects.

> Learn more at [cholo.meme](https://cholo.meme/).

## Repository overview

This Clarinet project contains two Clarity contracts:

| Contract | Purpose |
| --- | --- |
| [`cholo.clar`](contracts/cholo.clar) | SIP-010-style fungible token with fixed supply, transfers, owner management, and token metadata. |
| [`cholo-dao.clar`](contracts/cholo-dao.clar) | Multisignature DAO treasury with proposals, signer approvals, expiration, execution delay, STX and SIP-010 transfers, and signer/quorum administration. |

The repository also includes a Vitest/Clarinet SDK test suite for the DAO
contract.

## Token contract

`cholo.clar` defines the `$CHOLO` fungible token.

### Token parameters

| Property | Value |
| --- | --- |
| Name | `CHOLO` |
| Symbol | `CHOLO` |
| Decimals | `8` |
| Maximum supply | `888,888,888,888,888,888` base units |
| Human-readable supply | `8,888,888,888.88888888 CHOLO` |
| Initial recipient | Contract deployer |
| Token URI | `https://cholo.meme/bafkreibwuiavedbqjkvksvulm3focfv7ic2kd63c6lu5frtklteiys2mnq` |

The full supply is minted to the deployer when the contract is published.
Although the contract exposes `mint`, the initial mint already reaches
`MAX_SUPPLY`, so no additional tokens can be created unless the supply logic is
changed.

### Public functions

| Function | Description |
| --- | --- |
| `transfer` | Transfers CHOLO between principals. The transaction sender must match the supplied sender. |
| `mint` | Owner-only minting bounded by `MAX_SUPPLY`. |
| `set-owner` | Transfers contract ownership to another principal. |
| `set-token-uri` | Updates the token metadata URI. |

Read-only functions expose balances, total supply, name, symbol, decimals, and
the token URI.

## DAO treasury

`cholo-dao.clar` is a proposal-driven multisignature treasury. It starts with
five signers and uses a 51% quorum by default, rounded up. With five signers,
three approvals are required.

The default execution delay is 10 Stacks blocks measured from proposal
creation. Proposals must expire between 10 and 10,000 blocks after creation.

### Supported proposal types

| Proposal type | Action |
| --- | --- |
| `transfer` | Transfer STX from the DAO treasury. |
| `token-transfer` | Transfer a SIP-010 token from the DAO treasury. |
| `add-signer` | Add a signer. |
| `remove-signer` | Remove a signer while preserving the minimum signer count. |
| `replace-signer` | Replace an existing signer. |
| `set-required-sigs` | Set a fixed approval quorum. |
| `set-exec-delay` | Change the execution timelock. |

### Proposal lifecycle

1. A current signer calls `create-proposal`.
2. Signers call `approve-proposal` before the proposal expires.
3. The proposal reaches the configured quorum.
4. The execution delay measured from proposal creation passes.
5. `execute-proposal` performs the action and permanently marks the proposal as
   executed.

Execution is atomic: if the requested action fails, the executed flag and all
other state changes are reverted.

For non-token proposals, call `execute-proposal` with `none` as its optional
token-contract argument. Token transfers require a typed SIP-010 contract
reference at execution time; the DAO verifies that it matches the principal
stored in the proposal.

### Treasury deposits

Anyone can deposit STX:

```clarity
(contract-call? .cholo-dao deposit u1000000)
```

SIP-010 tokens can be deposited by transferring them directly to the DAO
contract principal.

### DAO read-only functions

| Function | Description |
| --- | --- |
| `is-signer` | Checks whether a principal is an active signer. |
| `get-signer-count` | Returns the active signer count. |
| `get-required-sigs` | Returns the fixed or computed quorum. |
| `get-signer` | Looks up a signer by index. |
| `has-approved` | Checks whether a signer approved a proposal. |
| `get-proposal` | Returns a stored proposal by ID. |

## Development

### Prerequisites

- A current [Node.js](https://nodejs.org/) LTS release
- npm
- [Clarinet](https://docs.stacks.co/clarinet) for local console and deployment
  workflows

The tests use the Clarinet SDK through Vitest, so a separate Clarinet binary is
not required just to run the npm test suite.

### Install

```bash
git clone <repository-url>
cd cholo-token
npm install
```

### Run tests

```bash
npm test
```

The suite covers:

- Initial signer set and computed quorum
- Signer-only proposal creation
- Proposal TTL validation
- Duplicate and unauthorized approvals
- Quorum and timelock enforcement
- STX treasury deposits and transfers
- SIP-010 treasury transfers
- Quorum changes
- Signer additions and index consistency
- Unknown proposal types

Generate Clarity coverage and execution-cost reports with:

```bash
npm run test:report
```

Run the tests continuously while editing contracts or test files:

```bash
npm run test:watch
```

Type-check the TypeScript test suite:

```bash
npx tsc --noEmit
```

## Project structure

```text
.
├── Clarinet.toml
├── contracts/
│   ├── cholo.clar
│   └── cholo-dao.clar
├── deployments/
│   ├── default.mainnet-plan.yaml
│   ├── default.testnet-plan.yaml
│   └── default.simnet-plan.yaml
├── settings/
├── tests/
│   └── cholo-dao.test.ts
├── package.json
├── tsconfig.json
└── vitest.config.js
```

## Deployment

Clarinet deployment plans are available under `deployments/`. Review expected
senders, network endpoints, fees, contract order, and generated transactions
before broadcasting.

The existing mainnet and testnet plans publish `cholo.clar` only. Regenerate or
update those plans before deploying `cholo-dao.clar`.

Never deploy directly from an unreviewed working tree. Run the complete test
suite and obtain an independent Clarity security review before managing assets
with the DAO treasury.

## Contract error codes

### Token

| Code | Meaning |
| --- | --- |
| `u100` | Owner-only operation |
| `u101` | Sender is not the token owner |
| `u102` | Invalid amount |
| `u103` | Invalid recipient |
| `u104` | Maximum supply exceeded |
| `u105` | Invalid owner |

### DAO

| Code | Meaning |
| --- | --- |
| `u100` | Caller is not a signer |
| `u101` | Proposal already executed |
| `u102` | Not enough approvals |
| `u103` | Signer already approved |
| `u104` | Proposal not found |
| `u105` | Proposal expired |
| `u106` | Minimum signer count reached |
| `u107` | Invalid parameters or timelock not reached |
| `u108` | Unknown proposal type |

Underlying STX or SIP-010 operations may return their own contract or runtime
error codes.

## Supply notice

The contract in this repository is the authoritative source for the CHOLO
supply:

**`8,888,888,888.88888888 CHOLO`**

The current [cholo.meme](https://cholo.meme/) metadata still mentions
7,000,000,000 tokens. That figure is outdated and should be updated to match the
contract before public distribution.

## Community

- Website: [cholo.meme](https://cholo.meme/)
- X/Twitter: [@cholomemecoin](https://x.com/cholomemecoin)

## Disclaimer

$CHOLO is experimental open-source software. Nothing in this repository is
financial, investment, legal, or tax advice. Smart contracts can contain bugs
and blockchain transactions are irreversible. Review the code, verify contract
addresses, and understand the risks before interacting with any deployment.
