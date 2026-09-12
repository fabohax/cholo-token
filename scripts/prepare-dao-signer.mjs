import { readFile, writeFile } from 'node:fs/promises';
import { Cl, serializeCV, deserializeCV, cvToString, validateStacksAddress } from '@stacks/transactions';

// Prepare wallet call arguments only. Never reads credentials or broadcasts.
const api = 'https://api.mainnet.hiro.so';
const deployer = 'SP193GXQTNHVV9WSAPHAB89M6R9QSEXZKS3774CMD';
const signer = 'SP1N4FTM6XK4FS4KQGZBTJY70F4CR36WQET7JFSS7';
const contract = `${deployer}.cholo-dao`;
const hex = value => `0x${Buffer.from(serializeCV(value)).toString('hex')}`;
async function request(path, body) {
  const response = await fetch(`${api}${path}`, {
    signal: AbortSignal.timeout(30_000),
    ...(body ? { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body) } : {}),
  });
  if (!response.ok) throw new Error(`HTTP ${response.status}: ${path}`);
  return response.json();
}
async function read(fn, args = []) {
  const result = await request(`/v2/contracts/call-read/${deployer}/cholo-dao/${fn}`, {
    sender: deployer, arguments: args.map(hex),
  });
  if (!result.okay) throw new Error(result.cause);
  return deserializeCV(result.result);
}
try {
  if (!validateStacksAddress(signer) || !signer.startsWith('SP')) throw new Error('Invalid mainnet signer');
  const [info, source, member, count, quorum, delay, version, initial] = await Promise.all([
    request('/v2/info'), request(`/v2/contracts/source/${deployer}/cholo-dao?proof=0`),
    read('is-signer', [Cl.principal(signer)]), read('get-signer-count'),
    read('get-required-sigs'), read('get-execution-delay'), read('get-governance-version'),
    read('get-signer', [Cl.uint(0)]),
  ]);
  if (info.network_id !== 1) throw new Error('Wrong network');
  const local = await readFile(new URL('../contracts/cholo-dao.clar', import.meta.url), 'utf8');
  if (source.source !== local) throw new Error('Deployed source differs from reviewed local source');
  if (cvToString(member) !== 'false') throw new Error('Requested address is already a signer');
  if (cvToString(count) !== 'u1' || cvToString(quorum) !== 'u1' || cvToString(version) !== 'u0'
      || cvToString(initial) !== `(some ${deployer})`) throw new Error('Governance changed; review the sequence again');
  const expiration = info.tenure_height + 500;
  const args = [Cl.principal(deployer), Cl.uint(0), Cl.stringAscii('add-signer'),
    Cl.some(Cl.principal(signer)), Cl.none(), Cl.none(),
    Cl.stringUtf8(`Add signer ${signer}`), Cl.uint(expiration), Cl.none(), Cl.none()];
  const report = {
    preparedAt: new Date().toISOString(), status: 'prepared-not-submitted',
    network: 'mainnet', contract, sender: deployer, newSigner: signer,
    functionName: 'create-proposal', functionArgs: args.map(hex),
    clarityArgs: args.map(cvToString), postConditionMode: 'deny', postConditions: [],
    expiration, executionDelay: cvToString(delay),
    expectedAfterExecution: { signerCount: 2, requiredApprovals: 2 },
    nextSteps: [
      'Refresh this file before signing; review fee and nonce in the wallet.',
      'Submit create-proposal directly from sender; save the returned proposal ID.',
      'After confirmed success, approve-proposal(ID) directly from sender.',
      'Read get-executable-at(ID); wait until tenure height reaches it.',
      'Before expiration, execute-proposal(ID, none), with deny postconditions and no asset transfers.',
      'Verify is-signer(newSigner), get-signer-count = u2 and get-required-sigs = u2.',
    ],
  };
  await writeFile(new URL('../docs/dao-add-signer.json', import.meta.url), JSON.stringify(report, null, 2) + '\n');
  console.log(JSON.stringify(report, null, 2));
} catch (error) {
  console.error(`Signer preparation failed: ${error.message}`);
  process.exitCode = 1;
}
