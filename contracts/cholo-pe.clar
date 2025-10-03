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

(define-fungible-token cholo)
(define-constant contract-owner tx-sender)

;; ---------------------------------------------------------
;; Constants/Variables
;; ---------------------------------------------------------
(define-data-var token-uri (optional (string-utf8 256)) none)

;; ---------------------------------------------------------
;; Errors
;; ---------------------------------------------------------
(define-constant ERR_UNAUTHORIZED (err u100))

;; ---------------------------------------------------------
;; SIP-10 Functions
;; ---------------------------------------------------------
(define-public (transfer
  (amount uint)
  (sender principal)
  (recipient principal)
  (memo (optional (buff 34)))
)
  (begin
    ;; #[filter(amount, recipient)]
    (asserts! (is-eq tx-sender sender) ERR_UNAUTHORIZED)
    (try! (ft-transfer? cholo amount sender recipient))
    (match memo to-print (print to-print) 0x)
    (ok true)
  )
)

(define-read-only (get-balance (owner principal))
  (ok (ft-get-balance cholo owner))
)

(define-read-only (get-name)
  (ok "cholo")
)

(define-read-only (get-symbol)
  (ok "cholo")
)

(define-read-only (get-decimals)
  (ok u6)
)

(define-read-only (get-total-supply)
  (ok (ft-get-supply cholo))
)

(define-read-only (get-token-uri)
    (ok (var-get token-uri)
    )
)

(define-public (set-token-uri (value (string-utf8 256)))
  ;; #[filter(value)]
  (if (is-eq tx-sender contract-owner)
    (ok (var-set token-uri (some value)))
    (err ERR_UNAUTHORIZED)
  )
)

;; ---------------------------------------------------------
;; Utility Functions
;; ---------------------------------------------------------
(define-public (send-many (recipients (list 200 { to: principal, amount: uint, memo: (optional (buff 34)) })))
  (fold check-err (map send-token recipients) (ok true))
)

(define-private (check-err (result (response bool uint)) (prior (response bool uint)))
  (match prior ok-value result err-value (err err-value))
)

(define-private (send-token (recipient { to: principal, amount: uint, memo: (optional (buff 34)) }))
  (send-token-with-memo (get amount recipient) (get to recipient) (get memo recipient))
)

(define-private (send-token-with-memo (amount uint) (to principal) (memo (optional (buff 34))))
  (let ((transferOk (try! (transfer amount tx-sender to memo))))
    (ok transferOk)
  )
)

;; ---------------------------------------------------------
;; Mint
;; ---------------------------------------------------------
(begin
  (try! (ft-mint? cholo u8000000000 contract-owner)) 
)