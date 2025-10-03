;; title: CHOLO
;; version: 1.0.1
;; summary: $CHOLO fungible token with fixed supply.
;; description: First memecoin LATAM anchored to Bitcoin L2 Stacks.
;; SIP-010 compliant.

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

(define-fungible-token CHOLO)

;; ERROR CODES
(define-constant ERR_DEPLOYER_ONLY        (err u100))
(define-constant ERR_NOT_ALLOWED          (err u101))
(define-constant ERR_INVALID_AMOUNT       (err u102))
(define-constant ERR_INVALID_RECIPIENT    (err u103))
(define-constant ERR_MAX_SUPPLY_EXCEEDED  (err u104))
(define-constant ERR_INVALID_OWNER        (err u105))
(define-constant ERR_UNAUTHORIZED         (err u106))
(define-constant ERR_INVALID_URI          (err u107))
(define-constant ERR_ALREADY_INITIALIZED  (err u108))

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
(define-data-var INITIALIZED bool false)

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

;; ========== TOKEN LOGIC ==========
;; Mint
(define-public (mint (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender (var-get DEPLOYER)) ERR_DEPLOYER_ONLY)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (not (is-eq recipient BURN_ADDRESS)) ERR_INVALID_RECIPIENT)
    (let ((supply (ft-get-supply CHOLO)))
      (asserts! (<= (+ supply amount) MAX_SUPPLY) ERR_MAX_SUPPLY_EXCEEDED)
      (try! (ft-mint? CHOLO amount recipient))
      (ok true))
  )
)

;; Transfer (ahora contratos pueden mover fondos si tienen tokens)
(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (begin
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (not (is-eq recipient BURN_ADDRESS)) ERR_INVALID_RECIPIENT)
    (match (ft-transfer? CHOLO amount sender recipient)
      ok-val (begin
        (emit-transfer-event sender recipient amount memo)
        (ok true))
      err-val (err err-val))
  )
)

;; Burn
(define-public (burn (amount uint))
  (begin
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (ft-burn? CHOLO amount tx-sender))
    (emit-burn-event tx-sender amount)
    (ok true)
  )
)

;; ========== METADATA MGMT ==========
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

;; ========== INIT ==========
(define-public (initialize)
  (begin
    (asserts! (is-eq tx-sender (var-get DEPLOYER)) ERR_DEPLOYER_ONLY)
    (asserts! (not (var-get INITIALIZED)) ERR_ALREADY_INITIALIZED)
    (try! (ft-mint? CHOLO MAX_SUPPLY tx-sender))
    (var-set INITIALIZED true)
    (ok true)
  )
)
