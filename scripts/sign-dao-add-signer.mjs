import { readFile, writeFile } from 'node:fs/promises';
import {
  AnchorMode,
  Cl,
  PostConditionMode,
  getAddressFromPrivateKey,
  makeContractCall,
  transactionToHex,
} from '@stacks/transactions';

// Creates a signed, unbroadcast mainnet transaction. The private key never
// leaves this process and the resulting raw transaction is written under logs/,
// which is ignored by git.
const envFile = new URL('../.env.local', import.meta.url);
const preparedFile = new URL('../docs/dao-add-signer.json', import.meta.url);
const outputFile = new URL('../logs/dao-add-signer-signed.json', import.meta.url);

function parseEnv(source) {
  return Object.fromEntries(source.split(/\r?\n/)
    .filter(line => /^[A-Za-z_][A-Za-z0-9_]*=/.test(line))
    .map(line => {
      const index = line.indexOf('=');
      return [line.slice(0, index), line.slice(index + 1).trim().replace(/^['"]|['"]$/g, '')];
    }));
}

try {
  const env = parseEnv(await readFile(envFile, 'utf8'));
  const senderKey = env.CHOLO_PRIVATE_KEY;
  if (!senderKey) throw new Error('CHOLO_PRIVATE_KEY is not set in .env.local');
  const prepared = JSON.parse(await readFile(preparedFile, 'utf8'));
  const [contractAddress, contractName] = prepared.contract.split('.');
  const derivedAddress = getAddressFromPrivateKey(senderKey);
  if (derivedAddress !== prepared.sender || contractAddress !== prepared.sender) {
    throw new Error('CHOLO_PRIVATE_KEY does not control the configured DAO signer');
  }
  if (prepared.network !== 'mainnet' || prepared.status !== 'prepared-not-submitted') {
    throw new Error('Prepared proposal is not an unsigned mainnet proposal');
  }

  const functionArgs = [
    Cl.principal(prepared.sender), Cl.uint(0), Cl.stringAscii('add-signer'),
    Cl.some(Cl.principal(prepared.newSigner)), Cl.none(), Cl.none(),
    Cl.stringUtf8(`Add signer ${prepared.newSigner}`), Cl.uint(prepared.expiration),
    Cl.none(), Cl.none(),
  ];
  const transaction = await makeContractCall({
    contractAddress,
    contractName,
    functionName: 'create-proposal',
    functionArgs,
    senderKey,
    network: 'mainnet',
    anchorMode: AnchorMode.Any,
    postConditionMode: PostConditionMode.Deny,
    postConditions: [],
  });
  const receipt = {
    signedAt: new Date().toISOString(),
    status: 'signed-not-broadcast',
    network: 'mainnet',
    txid: `0x${transaction.txid()}`,
    sender: prepared.sender,
    contract: prepared.contract,
    functionName: 'create-proposal',
    newSigner: prepared.newSigner,
    expiration: prepared.expiration,
    nonce: transaction.auth.spendingCondition.nonce.toString(),
    feeMicrostx: transaction.auth.spendingCondition.fee.toString(),
    postConditionMode: 'deny',
    rawTransaction: transactionToHex(transaction),
  };
  await writeFile(outputFile, JSON.stringify(receipt, null, 2) + '\n');
  console.log(JSON.stringify({ ...receipt, rawTransaction: '[saved to logs/dao-add-signer-signed.json]' }, null, 2));
} catch (error) {
  console.error(`Signing failed: ${error.message}`);
  process.exitCode = 1;
}
