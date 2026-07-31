# $CHOLO

Contratos inteligentes de **$CHOLO**, un token comunitario latinoamericano
construido sobre [Stacks](https://www.stacks.co/) y anclado al ecosistema de
Bitcoin.

$CHOLO toma su identidad del perro peruano sin pelo —símbolo cultural del
Perú— y combina la cultura meme con una misión de código abierto enfocada en
bienes públicos, DeSci, investigación y desarrollo, y proyectos liderados por
la comunidad.

> Conoce más en [cholo.meme](https://cholo.meme/).

## Descripción del repositorio

Este proyecto de Clarinet contiene tres contratos principales en Clarity:

| Contrato | Propósito |
| --- | --- |
| [`cholo.clar`](contracts/cholo.clar) | Token fungible compatible con SIP-010, con suministro fijo, transferencias, administración del propietario y metadatos. |
| [`cholo-dao.clar`](contracts/cholo-dao.clar) | Tesorería multifirma para la DAO, con propuestas, aprobaciones de firmantes, vencimiento, demora de ejecución, transferencias de STX y SIP-010, y administración del cuórum y los firmantes. |
| [`cholo-swap.clar`](contracts/cholo-swap.clar) | Venta de CHOLO respaldada por inventario, con pagos en STX, sBTC o USDCx. |

El repositorio también incluye una suite de pruebas para la DAO y el swap,
basada en Vitest y Clarinet SDK.

## Contrato del token

`cholo.clar` define el token fungible `$CHOLO`.

### Parámetros del token

| Propiedad | Valor |
| --- | --- |
| Nombre | `CHOLO` |
| Símbolo | `CHOLO` |
| Decimales | `8` |
| Suministro máximo | `888,888,888,888,888,888` unidades base |
| Suministro legible | `8,888,888,888.88888888 CHOLO` |
| Destinatario inicial | Cuenta que despliega el contrato |
| URI del token | `https://cholo.meme/bafkreibwuiavedbqjkvksvulm3focfv7ic2kd63c6lu5frtklteiys2mnq` |
| Dirección | `SP193GXQTNHVV9WSAPHAB89M6R9QSEXZKS3774CMD.cholo` |

El suministro completo se acuña a favor de la cuenta que publica el contrato.
Aunque el contrato expone la función `mint`, la acuñación inicial ya alcanza
`MAX_SUPPLY`, por lo que no pueden crearse tokens adicionales a menos que se
modifique la lógica del suministro.

### Funciones públicas

| Función | Descripción |
| --- | --- |
| `transfer` | Transfiere CHOLO entre principales. El emisor de la transacción debe coincidir con el remitente indicado. |
| `mint` | Acuñación exclusiva del propietario y limitada por `MAX_SUPPLY`. |
| `set-owner` | Transfiere la propiedad del contrato a otro principal. |
| `set-token-uri` | Actualiza la URI de metadatos del token. |

Las funciones de solo lectura permiten consultar saldos, suministro total,
nombre, símbolo, decimales y URI del token.

## Swap de CHOLO

`cholo-swap.clar` permite comprar CHOLO con STX, sBTC o USDCx. Antes de abrir
las compras, el propietario debe transferir CHOLO al principal del contrato,
configurar la tesorería y establecer la tasa de cada activo.

Cada tasa usa una fracción `numerator / denominator` aplicada a unidades base:

```text
cholo-out = payment-amount * numerator / denominator
```

Los únicos activos de pago admitidos son STX nativo, el contrato oficial de
sBTC en mainnet
`SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4.sbtc-token` y el contrato oficial
de USDCx en mainnet
`SP120SBRBQJ00MCWS7TM5R8WJNTTKD5K0HFRC2CNE.usdcx`. El propietario no puede
configurar otros contratos SIP-010 en su lugar.

Las tasas no se inicializan con un precio predeterminado: hasta que el
propietario configura una tasa válida, la compra correspondiente permanece
deshabilitada. Es importante calcular las tasas teniendo en cuenta los
decimales de ambos activos. Por ejemplo, si STX usa 6 decimales y CHOLO usa 8,
una tasa de `u10000 / u1` entrega 100 CHOLO por 1 STX:

```text
1,000,000 micro-STX * 10,000 = 10,000,000,000 unidades base de CHOLO
                                      = 100.00000000 CHOLO
```

Las compras aceptan `min-cholo-out` para proteger al comprador frente a
cambios de tasa. Los pagos se envían directamente a la tesorería configurada;
el contrato solo custodia el inventario de CHOLO. Las compras con sBTC y USDCx
también comprueban que el contrato SIP-010 recibido coincida exactamente con
el principal permitido por el propietario.

Funciones públicas principales:

| Función | Descripción |
| --- | --- |
| `buy-with-stx` | Compra CHOLO pagando STX. |
| `buy-with-sbtc` | Compra CHOLO pagando el contrato sBTC permitido. |
| `buy-with-usdcx` | Compra CHOLO pagando el contrato USDCx permitido. |
| `set-stx-rate` | Configura la tasa de STX. |
| `set-sbtc-config` | Configura el principal y la tasa de sBTC. |
| `set-usdcx-config` | Configura el principal y la tasa de USDCx. |
| `set-treasury` | Cambia el destino de los pagos. |
| `set-paused` | Pausa o reactiva todas las compras. |
| `withdraw-cholo` | Retira inventario de CHOLO. |

### Preparación del swap

El siguiente ejemplo configura la tesorería, habilita una tasa de STX y
deposita inventario. Los principales de sBTC y USDCx deben reemplazarse por los
contratos oficiales de la red elegida.

```clarity
;; Enviar 1,000 CHOLO al inventario del swap.
(contract-call? .cholo transfer
  u100000000000
  tx-sender
  .cholo-swap
  none)

;; Enviar todos los pagos a la tesorería de la DAO.
(contract-call? .cholo-swap set-treasury .cholo-dao)

;; 1 STX = 100 CHOLO, suponiendo 6 decimales para STX y 8 para CHOLO.
(contract-call? .cholo-swap set-stx-rate u10000 u1)

;; Configurar los contratos oficiales permitidos y sus tasas.
(contract-call? .cholo-swap set-sbtc-config
  'SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4.sbtc-token u1000000 u1)
(contract-call? .cholo-swap set-usdcx-config
  'SP120SBRBQJ00MCWS7TM5R8WJNTTKD5K0HFRC2CNE.usdcx u10000 u1)
```

Una compra con STX que exige recibir al menos 100 CHOLO:

```clarity
(contract-call? .cholo-swap buy-with-stx u1000000 u10000000000)
```

Antes de enviar una compra, una aplicación puede consultar `quote-stx`,
`quote-sbtc` o `quote-usdcx` y usar el resultado —ajustado por su tolerancia de
deslizamiento— como `min-cholo-out`.

`mock-token.clar` existe únicamente como activo SIP-010 auxiliar para las
pruebas locales; no debe incluirse en despliegues públicos.

## Tesorería de la DAO

`cholo-dao.clar` es una tesorería multifirma gestionada mediante propuestas.
Comienza con cinco firmantes y, de forma predeterminada, utiliza un cuórum del
51 % redondeado hacia arriba. Con cinco firmantes se requieren tres
aprobaciones.

La demora de ejecución predeterminada es de 10 bloques de Stacks, contados
desde la creación de la propuesta. Las propuestas deben vencer entre 10 y
10 000 bloques después de su creación.

### Tipos de propuesta compatibles

| Tipo de propuesta | Acción |
| --- | --- |
| `transfer` | Transfiere STX desde la tesorería de la DAO. |
| `token-transfer` | Transfiere un token SIP-010 desde la tesorería de la DAO. |
| `add-signer` | Agrega un firmante. |
| `remove-signer` | Elimina un firmante sin vulnerar el mínimo de firmantes. |
| `replace-signer` | Reemplaza un firmante existente. |
| `set-required-sigs` | Establece un cuórum fijo de aprobaciones. |
| `set-exec-delay` | Modifica el bloqueo temporal de ejecución. |

### Ciclo de vida de una propuesta

1. Un firmante activo llama a `create-proposal`.
2. Los firmantes llaman a `approve-proposal` antes de que venza la propuesta.
3. La propuesta alcanza el cuórum configurado.
4. Transcurre la demora de ejecución medida desde la creación de la propuesta.
5. `execute-proposal` realiza la acción y marca definitivamente la propuesta
   como ejecutada.

La ejecución es atómica: si la acción solicitada falla, el indicador de
ejecución y todos los demás cambios de estado se revierten.

Para propuestas que no transfieren tokens, se debe llamar a
`execute-proposal` con `none` como argumento opcional `token-contract`. Las
transferencias de tokens requieren una referencia tipada al contrato SIP-010
durante la ejecución; la DAO verifica que esta coincida con el principal
almacenado en la propuesta.

### Depósitos en la tesorería

Cualquier persona puede depositar STX:

```clarity
(contract-call? .cholo-dao deposit u1000000)
```

Los tokens SIP-010 pueden depositarse transfiriéndolos directamente al
principal del contrato de la DAO.

### Funciones de solo lectura de la DAO

| Función | Descripción |
| --- | --- |
| `is-signer` | Comprueba si un principal es un firmante activo. |
| `get-signer-count` | Devuelve el número de firmantes activos. |
| `get-required-sigs` | Devuelve el cuórum fijo o calculado. |
| `get-signer` | Busca un firmante por su índice. |
| `has-approved` | Comprueba si un firmante aprobó una propuesta. |
| `get-proposal` | Devuelve una propuesta almacenada a partir de su ID. |

## Desarrollo

### Requisitos previos

- Una versión LTS vigente de [Node.js](https://nodejs.org/)
- npm
- [Clarinet](https://docs.stacks.co/clarinet) para usar la consola local y los
  flujos de despliegue

Las pruebas utilizan Clarinet SDK mediante Vitest, por lo que no es necesario
instalar el ejecutable independiente de Clarinet únicamente para ejecutar la
suite con npm.

### Instalación

```bash
git clone https://github.com/fabohax/cholo-token
cd cholo-token
npm install
```

### Ejecutar las pruebas

```bash
npm test
```

La suite cubre:

- El conjunto inicial de firmantes y el cálculo del cuórum
- La creación de propuestas restringida a firmantes
- La validación del vencimiento de las propuestas
- Las aprobaciones duplicadas o no autorizadas
- El cumplimiento del cuórum y del bloqueo temporal
- Los depósitos y transferencias de STX de la tesorería
- Las transferencias de tokens SIP-010 desde la tesorería
- Los cambios de cuórum
- La incorporación de firmantes y la consistencia de sus índices
- Los tipos de propuesta desconocidos
- Las compras de CHOLO con STX, sBTC y USDCx
- La protección de salida mínima, la pausa y los permisos administrativos

Para generar informes de cobertura de Clarity y costos de ejecución:

```bash
npm run test:report
```

Para ejecutar las pruebas continuamente mientras se modifican los contratos o
los archivos de prueba:

```bash
npm run test:watch
```

Para comprobar los tipos de la suite de pruebas en TypeScript:

```bash
npx tsc --noEmit
```

## Estructura del proyecto

```text
.
├── Clarinet.toml
├── contracts/
│   ├── cholo.clar
│   ├── cholo-dao.clar
│   ├── cholo-swap.clar
│   └── mock-token.clar
├── deployments/
│   ├── default.mainnet-plan.yaml
│   ├── default.testnet-plan.yaml
│   └── default.simnet-plan.yaml
├── settings/
├── tests/
│   ├── cholo-dao.test.ts
│   └── cholo-swap.test.ts
├── package.json
├── tsconfig.json
└── vitest.config.js
```

## Despliegue

Los planes de despliegue de Clarinet se encuentran en `deployments/`. Antes de
transmitirlos, revisa los emisores esperados, los puntos de conexión de la red,
las comisiones, el orden de los contratos y las transacciones generadas.

Los planes actuales de mainnet y testnet solo publican `cholo.clar`. Regenera o
actualiza estos planes antes de desplegar `cholo-dao.clar` o
`cholo-swap.clar`. El token CHOLO debe publicarse antes que el swap porque este
último realiza llamadas estáticas a `.cholo`. No incluyas `mock-token.clar` en
un despliegue público.

Después del despliegue, y antes de habilitar compras:

1. Verifica los principales oficiales de sBTC y USDCx para la red.
2. Define el principal de la tesorería.
3. Calcula y revisa las tasas usando unidades base.
4. Transfiere al swap únicamente el inventario de CHOLO destinado a la venta.
5. Realiza compras pequeñas de prueba para cada activo.
6. Transfiere la propiedad del swap al principal administrativo definitivo si
   corresponde.

Nunca despliegues directamente desde un árbol de trabajo sin revisar. Ejecuta
la suite completa de pruebas y consigue una auditoría de seguridad
independiente de Clarity antes de administrar activos mediante la tesorería o
el swap.

## Códigos de error de los contratos

### Token

| Código | Significado |
| --- | --- |
| `u100` | Operación exclusiva del propietario |
| `u101` | El emisor no es propietario de los tokens |
| `u102` | Cantidad no válida |
| `u103` | Destinatario no válido |
| `u104` | Suministro máximo excedido |
| `u105` | Propietario no válido |

### DAO

| Código | Significado |
| --- | --- |
| `u100` | La persona que llama no es firmante |
| `u101` | La propuesta ya fue ejecutada |
| `u102` | No hay suficientes aprobaciones |
| `u103` | El firmante ya aprobó la propuesta |
| `u104` | No se encontró la propuesta |
| `u105` | La propuesta venció |
| `u106` | Se alcanzó el número mínimo de firmantes |
| `u107` | Parámetros no válidos o bloqueo temporal aún vigente |
| `u108` | Tipo de propuesta desconocido |

### Swap

| Código | Significado |
| --- | --- |
| `u200` | Operación exclusiva del propietario |
| `u201` | Cantidad de entrada o salida no válida |
| `u202` | Numerador o denominador de tasa no válido |
| `u203` | El swap está pausado |
| `u204` | La salida calculada es menor que `min-cholo-out` |
| `u205` | El contrato SIP-010 recibido no es el token permitido |
| `u206` | El activo solicitado todavía no está configurado |
| `u207` | Principal no válido |

Las operaciones STX o SIP-010 subyacentes pueden devolver sus propios códigos
de error del contrato o del entorno de ejecución.

## Aviso sobre el suministro

El contrato de este repositorio es la fuente oficial del suministro de CHOLO:

**`8,888,888,888.88888888 CHOLO`**

Los metadatos actuales de [cholo.meme](https://cholo.meme/) todavía mencionan
7 000 000 000 de tokens. Esa cifra está desactualizada y debe corregirse para
que coincida con el contrato antes de la distribución pública.

## Comunidad

- Sitio web: [cholo.meme](https://cholo.meme/)
- X/Twitter: [@cholomemecoin](https://x.com/cholomemecoin)

## Aviso legal

$CHOLO es software experimental de código abierto. Nada de lo contenido en este
repositorio constituye asesoría financiera, de inversión, legal o tributaria.
Los contratos inteligentes pueden contener errores y las transacciones en
blockchain son irreversibles. Revisa el código, verifica las direcciones de los
contratos y comprende los riesgos antes de interactuar con cualquier
despliegue.
