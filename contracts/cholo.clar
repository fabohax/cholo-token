;; title: CHOLO
;; version: 1.1.0
;; summary: $CHOLO is a fungible token with a fixed supply of 8,000,000,000.
;; description: $CHOLO is the first memecoin created in LATAM anchored to Bitcoin L2 Stacks.
;; Complies with SIP-010 standard with no external dependencies.

(define-trait sip-010-trait
  (
    (get-balance (principal) (response uint uint))
    (get-total-supply () (response uint uint))
    (get-decimals () (response uint uint))
    (get-symbol () (response (string-ascii 12) uint))
    (get-name () (response (string-ascii 32) uint))
    (get-token-uri () (response (optional (string-ascii 256)) uint))
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))
    (mint (uint principal) (response bool uint))
  )
)

;; Implement SIP-010
(impl-trait .sip-010-trait)

(define-fungible-token CHOLO)

;; ERROR CODES
(define-constant ERR_DEPLOYER_ONLY        (err u100))
(define-constant ERR_NOT_TOKEN_OWNER      (err u101))
(define-constant ERR_INVALID_AMOUNT       (err u102))
(define-constant ERR_INVALID_RECIPIENT    (err u103))
(define-constant ERR_MAX_SUPPLY_EXCEEDED  (err u104))
(define-constant ERR_INVALID_OWNER        (err u105))
(define-constant ERR_UNAUTHORIZED         (err u106))
(define-constant ERR_INVALID_URI          (err u107))
(define-constant ERR_DUPLICATE_CLAIM      (err u108))

;; CONSTANTS
(define-constant TOKEN_NAME    "CHOLO")
(define-constant TOKEN_SYMBOL  "CHOLO")
(define-constant TOKEN_DECIMALS u8)
(define-constant MAX_SUPPLY    u8000000000)
(define-constant BURN_ADDRESS  'SP000000000000000000002Q6VF78)

;; STATE
(define-data-var TOKEN_URI (string-ascii 256) "https://cholo.meme/bafkreibwuiavedbqjkvksvulm3focfv7ic2kd63c6lu5frtklteiys2mnq")
(define-data-var DEPLOYER principal tx-sender)
(define-data-var ADMIN    principal tx-sender)

;; Bridge role: only this principal can call bridge-in
(define-data-var BRIDGE (optional principal) none)

;; Anti-replay for bridge-in claims (one mint per (src_chain, src_txid))
(define-map bridge-claims
  { src_chain: (string-ascii 16), src_txid: (buff 32) }
  bool)

;; ========== READ-ONLY ==========
(define-read-only (get-balance (who principal))
  (ok (ft-get-balance CHOLO who))
)

(define-read-only (get-total-supply)
  (ok (ft-get-supply CHOLO))
)

(define-read-only (get-name)   (ok TOKEN_NAME))
(define-read-only (get-symbol) (ok TOKEN_SYMBOL))
(define-read-only (get-decimals) (ok TOKEN_DECIMALS))
(define-read-only (get-token-uri) (ok (some (var-get TOKEN_URI))))

;; ========== EVENTS ==========
(define-private (emit-transfer-event (from principal) (to principal) (amount uint) (memo (optional (buff 34))))
  (print (tuple (event "transfer") (from from) (to to) (amount amount) (memo memo) (timestamp block-height)))
)

(define-private (emit-burn-event (from principal) (amount uint))
  (print (tuple (event "burn") (from from) (amount amount) (timestamp block-height)))
)

(define-private (emit-mint-event (to principal) (amount uint))
  (print (tuple (event "mint") (to to) (amount amount) (timestamp block-height)))
)

(define-private (emit-bridge-out (from principal) (amount uint) (dst_chain (string-ascii 16)) (dst_address (string-ascii 128)))
  (print (tuple (event "bridge-out") (from from) (amount amount) (dst_chain dst_chain) (dst_address dst_address) (timestamp block-height)))
)

(define-private (emit-bridge-in (to principal) (amount uint) (src_chain (string-ascii 16)) (src_txid (buff 32)))
  (print (tuple (event "bridge-in") (to to) (amount amount) (src_chain src_chain) (src_txid src_txid) (timestamp block-height)))
)

;; ========== TOKEN LOGIC ==========
;; Mint: capped by MAX_SUPPLY using on-chain supply (burn-aware)
(define-public (mint (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender (var-get DEPLOYER)) ERR_DEPLOYER_ONLY)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (not (is-eq recipient BURN_ADDRESS)) ERR_INVALID_RECIPIENT)
    (let ((supply (ft-get-supply CHOLO)))
      (asserts! (<= (+ supply amount) MAX_SUPPLY) ERR_MAX_SUPPLY_EXCEEDED)
      (try! (ft-mint? CHOLO amount recipient))
      (emit-mint-event recipient amount)
      (ok true))
  )
)

;; Transfer
(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (begin
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (is-eq tx-sender sender) ERR_NOT_TOKEN_OWNER)
    (asserts! (not (is-eq recipient BURN_ADDRESS)) ERR_INVALID_RECIPIENT)
    (match (ft-transfer? CHOLO amount sender recipient)
      success (begin
        (emit-transfer-event sender recipient amount memo)
        (ok true))
      error (err error))
  )
)

;; Burn (deflationary: reduces supply)
(define-public (burn (amount uint))
  (begin
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (ft-burn? CHOLO amount tx-sender))
    (emit-burn-event tx-sender amount)
    (ok true)
  )
)

;; Safe batch transfer (cap: 100). Atomic: si una falla, revierte todo.
(define-public (send-many (recipients (list 100 { to: principal, amount: uint, memo: (optional (buff 34)) })))
  (begin
    (asserts! (> (len recipients) u0) ERR_INVALID_AMOUNT)
    (fold send-many-step recipients (ok true))
  )
)

(define-private (send-many-step
  (entry { to: principal, amount: uint, memo: (optional (buff 34)) })
  (acc (response bool uint)))
  (match acc
    ;; si ya hay error previo, propágalo
    err-prev (err err-prev)
    ;; otherwise procesa siguiente item
    ok-prev
      (begin
        (asserts! (> (get amount entry) u0) ERR_INVALID_AMOUNT)
        (asserts! (not (is-eq (get to entry) BURN_ADDRESS)) ERR_INVALID_RECIPIENT)
        (match (ft-transfer? CHOLO (get amount entry) tx-sender (get to entry))
          ok-tx
            (begin
              (emit-transfer-event tx-sender (get to entry) (get amount entry) (get memo entry))
              (ok true))
          err-tx (err err-tx)))
  )
)

;; ========== METADATA MGMT ==========
;; CHOLO_DAO constant removed - restrict update-token-uri to ADMIN
(define-public (update-token-uri (new-uri (string-ascii 256)))
  (begin
    (asserts! (is-eq tx-sender (var-get ADMIN)) ERR_UNAUTHORIZED)
    (asserts! (> (len new-uri) u0) ERR_INVALID_URI)
    (var-set TOKEN_URI new-uri)
    (ok true)
  )
)

;; Only current ADMIN can set a new ADMIN
(define-public (set-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender (var-get ADMIN)) ERR_UNAUTHORIZED)
    (asserts! (not (is-eq new-admin BURN_ADDRESS)) ERR_INVALID_OWNER)
    (var-set ADMIN new-admin)
    (ok true)
  )
)

;; ========== BRIDGE HOOKS ==========
;; Set/clear bridge authority (off-chain bridge/relayer contract principal)
(define-public (set-bridge (p principal))
  (begin
    (asserts! (is-eq tx-sender (var-get DEPLOYER)) ERR_DEPLOYER_ONLY)
    (var-set BRIDGE (some p))
    (ok true)
  )
)

(define-public (clear-bridge)
  (begin
    (asserts! (is-eq tx-sender (var-get DEPLOYER)) ERR_DEPLOYER_ONLY)
    (var-set BRIDGE none)
    (ok true)
  )
)

;; User burns on Stacks to move to another chain.
(define-public (bridge-out (amount uint) (dst_chain (string-ascii 16)) (dst_address (string-ascii 128)))
  (begin
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (ft-burn? CHOLO amount tx-sender))
    (emit-bridge-out tx-sender amount dst_chain dst_address)
    (ok true)
  )
)

;; Bridge authority mints on Stacks after verifying proof from source chain.
(define-public (bridge-in (amount uint) (recipient principal) (src_chain (string-ascii 16)) (src_txid (buff 32)))
  (begin
    ;; require bridge role
    (match (var-get BRIDGE)
      b (asserts! (is-eq tx-sender b) ERR_UNAUTHORIZED)
      none (err (unwrap-panic ERR_UNAUTHORIZED)))  ;; unreachable, but keeps types tidy

    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    ;; anti-replay (one claim per (src_chain, src_txid))
    (asserts! (is-none (map-get? bridge-claims {src_chain: src_chain, src_txid: src_txid})) ERR_DUPLICATE_CLAIM)

    ;; cap check against MAX_SUPPLY using live supply
    (let ((supply (ft-get-supply CHOLO)))
      (asserts! (<= (+ supply amount) MAX_SUPPLY) ERR_MAX_SUPPLY_EXCEEDED)
      ;; effects first; whole tx reverts if mint fails
      (map-set bridge-claims {src_chain: src_chain, src_txid: src_txid} true)
      (try! (ft-mint? CHOLO amount recipient))
      (emit-bridge-in recipient amount src_chain src_txid)
      (ok true))
  )
)

;; ========== INIT ==========
(begin
  ;; Mint full fixed supply to deployer at launch 
  (try! (ft-mint? CHOLO u8000000000 tx-sender))
  (ok true)
)
