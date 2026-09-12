import { readFile, writeFile } from 'node:fs/promises';
import { createHash } from 'node:crypto';
import { Cl, serializeCV, deserializeCV, cvToString } from '@stacks/transactions';

// Public reads only: no environment secrets, signing or transaction broadcast.
const api = 'https://api.mainnet.hiro.so';
const sender = 'SP193GXQTNHVV9WSAPHAB89M6R9QSEXZKS3774CMD';
const name = 'cholo-dao';
const hash = value => createHash('sha256').update(value).digest('hex');
async function request(path, body) {
  const response = await fetch(`${api}${path}`, {
    signal: AbortSignal.timeout(30_000),
    ...(body ? { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body) } : {}),
  });
  if (!response.ok) throw new Error(`${path}: HTTP ${response.status}`);
  return response.json();
}
async function read(fn, args = []) {
  const result = await request(`/v2/contracts/call-read/${sender}/${name}/${fn}`, {
    sender, arguments: args.map(arg => '0x' + Buffer.from(serializeCV(arg)).toString('hex')),
  });
  if (!result.okay) throw new Error(`${fn}: ${result.cause}`);
  return deserializeCV(result.result);
}
try {
  const local = await readFile(new URL('../contracts/cholo-dao.clar', import.meta.url), 'utf8');
  const [info, deployed, metadata, balances, count, quorum, delay, version] = await Promise.all([
    request('/v2/info'),
    request(`/v2/contracts/source/${sender}/${name}?proof=0`),
    request(`/extended/v1/contract/${sender}.${name}`),
    request(`/extended/v1/address/${sender}.${name}/balances`),
    read('get-signer-count'), read('get-required-sigs'),
    read('get-execution-delay'), read('get-governance-version'),
  ]);
  if (info.network_id !== 1) throw new Error('Expected mainnet network_id 1');
  const signerCount = Number(count.value);
  if (!Number.isSafeInteger(signerCount) || signerCount < 1 || signerCount > 1000) throw new Error('Unexpected signer count');
  const signers = [];
  for (let i = 0; i < signerCount; i++) signers.push(cvToString(await read('get-signer', [Cl.uint(i)])));
  const transaction = await request(`/extended/v1/tx/${metadata.tx_id}`);
  const report = {
    checkedAt: new Date().toISOString(), api, contract: `${sender}.${name}`,
    stacksTipHeight: info.stacks_tip_height, tenureHeight: info.tenure_height,
    sourceMatches: deployed.source === local,
    localSha256: hash(local), deployedSha256: hash(deployed.source),
    publishHeight: deployed.publish_height,
    deploymentTx: metadata.tx_id, deploymentStatus: transaction.tx_status,
    canonical: transaction.canonical, clarityVersion: metadata.clarity_version,
    signerCount, signers, quorum: cvToString(quorum),
    executionDelay: cvToString(delay), governanceVersion: cvToString(version),
    treasury: { stx: balances.stx, fungibleTokens: balances.fungible_tokens },
  };
  await writeFile(new URL('../docs/dao-mainnet-status.json', import.meta.url), JSON.stringify(report, null, 2) + '\n');
  console.log(JSON.stringify(report, null, 2));
  if (!report.sourceMatches || report.deploymentStatus !== 'success' || !report.canonical) process.exitCode = 1;
} catch (error) {
  console.error(`Mainnet verification failed: ${error.message}`);
  process.exitCode = 1;
}
