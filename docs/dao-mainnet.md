# Puesta en operación de CHOLO DAO

Verificación del 12 de septiembre de 2026: el contrato ya está publicado y el
código coincide byte por byte con `contracts/cholo-dao.clar`.

- Principal: `SP193GXQTNHVV9WSAPHAB89M6R9QSEXZKS3774CMD.cholo-dao`.
- Transacción confirmada y canónica: `0x54f119f74129bce5ae0b7e1d25421aeb4eb3a78578a9f02fbad436669937a01b`.
- SHA-256: `5d977a77f51a39b55607ec644008bdfdb82efc5c14ce1e5a76bb59b2cd198968`.
- Clarity 2; único firmante: deployer; cuórum 1; demora 10 tenures; versión de gobernanza 0.
- Saldo observado: 0 STX y 380387300000000001 unidades base CHOLO
  (3 803 873 000,00000001 CHOLO, 8 decimales).

El plan `deployments/default.mainnet-plan.yaml` es una referencia histórica.
No volver a aplicarlo: ese principal ya tiene el contrato publicado.

## Verificación reproducible

```bash
npm run verify:dao:mainnet
npm test
npx tsc --noEmit
```

La verificación consulta datos públicos, compara hashes y guarda
`docs/dao-mainnet-status.json`. No lee `.env.local`, no firma y no transmite
transacciones. Un error de API o una discrepancia produce salida distinta de cero.
Las lecturas consultan la punta de la cadena y no son una instantánea atómica;
repetirlas antes de operar si hubo transacciones de gobernanza concurrentes.

Validación local: 40 pruebas aprobadas, una prueba de mock-token ya omitida;
TypeScript sin errores. Clarinet 3.23.1 comprobó los cuatro contratos, con
48 advertencias (datos potencialmente no comprobados y lints entre ellas).
Esto no constituye una auditoría independiente.

El CLI 3.23.1 regenera los planes con `transaction-type`, incompatible con el
SDK 2.x instalado. No regenerar el plan simnet con ese CLI antes de ejecutar
Vitest. Para comprobar solo la sintaxis del DAO sin regenerar el plan:
`clarinet check contracts/cholo-dao.clar`. La suite usa el formato del SDK.

## Configuración pendiente

Se solicitó añadir únicamente `SP1N4FTM6XK4FS4KQGZBTJY70F4CR36WQET7JFSS7`.
La propuesta está en `docs/dao-add-signer.json`; regenerarla antes de firmar con
`npm run prepare:dao:signer`. No se ha transmitido. Se conserva al deployer y,
tras la ejecución, el cuórum automático será 2 de 2. Las otras dos direcciones
del pedido anterior quedan excluidas. Se mantiene la demora actual.

1. Verificar control de cada dirección. Crear una propuesta `add-signer` por
   nuevo miembro mediante llamada directa a `create-proposal` desde un firmante.
   Argumentos: destinatario = deployer, monto = `u0`, tipo = `"add-signer"`,
   new-signer = `(some 'DIRECCION)`, old-signer/token = `none`, descripción UTF-8,
   expiration = altura actual de tenure + 500, new-required/new-delay = `none`.
   Obtener la altura de tenure de `/v2/info` y refrescarla antes de firmar;
   no usar `stacks_tip_height` como reloj de Clarity 2.
2. Guardar el ID retornado, aprobar con `approve-proposal(id)` desde el cuórum
   vigente y consultar `get-executable-at(id)`.
3. Cuando se alcance esa altura, ejecutar `execute-proposal(id, none)` antes
   del vencimiento. Verificar índices, membresía, cuórum y versión.
4. Repetir secuencialmente: cada cambio invalida TODAS las propuestas pendientes.
   Con un miembro se exige 1 aprobación; con dos, 2; con tres, 2. Para añadir
   al tercero deben aprobar los dos miembros existentes.
5. Una vez incorporados los miembros, fijar el cuórum acordado mediante
   `set-required-sigs` con `new-required = (some uN)` y el mismo ciclo.
   Debe ser positivo y no exceder el número de miembros. No existe API para
   volver al modo automático después de fijarlo.
6. Hacer una transferencia pequeña de CHOLO a un destinatario acordado:
   propuesta `token-transfer` con el token
   `SP193GXQTNHVV9WSAPHAB89M6R9QSEXZKS3774CMD.cholo`; ejecutar pasando ese contrato
   como argumento opcional tipado. Verificar saldo de origen/destino, evento y
   estado ejecutado. Las comisiones las paga quien envía cada transacción.

Los firmantes deben revisar destinatario, unidades base, token y postcondiciones
en la cartera antes de firmar. El control actual sigue siendo de una sola clave;
los tokens ya depositados están sujetos a esa gobernanza.

Referencia oficial: [despliegues con Clarinet](https://docs.stacks.co/learn-clarinet/contract-deployment).
