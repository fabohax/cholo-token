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

Este proyecto de Clarinet contiene dos contratos en Clarity:

| Contrato | Propósito |
| --- | --- |
| [`cholo.clar`](contracts/cholo.clar) | Token fungible compatible con SIP-010, con suministro fijo, transferencias, administración del propietario y metadatos. |
| [`cholo-dao.clar`](contracts/cholo-dao.clar) | Tesorería multifirma para la DAO, con propuestas, aprobaciones de firmantes, vencimiento, demora de ejecución, transferencias de STX y SIP-010, y administración del cuórum y los firmantes. |

El repositorio también incluye una suite de pruebas para el contrato de la DAO
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
git clone <url-del-repositorio>
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

## Despliegue

Los planes de despliegue de Clarinet se encuentran en `deployments/`. Antes de
transmitirlos, revisa los emisores esperados, los puntos de conexión de la red,
las comisiones, el orden de los contratos y las transacciones generadas.

Los planes actuales de mainnet y testnet solo publican `cholo.clar`. Regenera o
actualiza estos planes antes de desplegar `cholo-dao.clar`.

Nunca despliegues directamente desde un árbol de trabajo sin revisar. Ejecuta
la suite completa de pruebas y consigue una auditoría de seguridad
independiente de Clarity antes de administrar activos mediante la tesorería de
la DAO.

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
