;; title: CHOLO
;; version: 0.1.0
;; summary: $CHOLO fungible token with fixed supply.
;; description: First memecoin created in LATAM anchored to Bitcoin L2 Stacks.
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
  )
)

(define-fungible-token cholo)
(define-constant cholo-deployer tx-sender)

;; CONSTANTS
(define-constant MAX_SUPPLY u800000000000000000) ;; 8B supply
(define-data-var token-uri (optional (string-ascii 256)) none)

;; ERROR CODES
(define-constant ERR_UNAUTHORIZED (err u100))

;; ========== TRANSFER ==========
(define-public (transfer
  (amount uint)
  (sender principal)
  (recipient principal)
  (memo (optional (buff 34)))
)
  (begin
    (asserts! (is-eq tx-sender sender) ERR_UNAUTHORIZED)
    (try! (ft-transfer? cholo amount sender recipient))
    (ok true)
  )
)

;; ========== READ-ONLY ==========
(define-read-only (get-balance (owner principal))
  (ok (ft-get-balance cholo owner))
)

(define-read-only (get-name)
  (ok "CHOLO")
)

(define-read-only (get-symbol)
  (ok "CHOLO")
)

(define-read-only (get-decimals)
  (ok u8)
)

(define-read-only (get-total-supply)
  (ok (ft-get-supply cholo))
)

(define-read-only (get-token-uri)
  (ok (var-get token-uri))
)

;; ========== METADATA MGMT ==========
(define-public (set-token-uri (value (string-ascii 256)))
  (if (is-eq tx-sender cholo-deployer)
    (ok (var-set token-uri (some value)))
    (err ERR_UNAUTHORIZED)
  )
)

;; ========== UTILITY ==========
;; Batch-send
(define-public (send-many (recipients (list 200 { to: principal, amount: uint, memo: (optional (buff 34)) })))
  (fold check-err (map send-token recipients) (ok true))
)

(define-private (check-err (result (response bool uint)) (prior (response bool uint)))
  (match prior
    prior-ok result
    prior-err (err prior-err)))

(define-private (send-token (recipient { to: principal, amount: uint, memo: (optional (buff 34)) }))
  (transfer (get amount recipient) tx-sender (get to recipient) (get memo recipient))
)

;; ========== INIT ==========
;; Mint full fixed supply to deployer at launch (one-time)
(begin
  (try! (ft-mint? cholo MAX_SUPPLY cholo-deployer))
  (ok true)
)
