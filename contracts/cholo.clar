;; title: CHOLO
;; version: 1.1.0
;; summary: $CHOLO es un token fungible con suministro fijo de 8,000,000,000 para financiar iniciativas DeSci y comunidad.
;; description: El token $CHOLO impulsa proyectos descentralizados de ciencia y tecnolog1a (DeSci) y cultura comunitaria en Stacks. 
;; Sirve como unidad de valor est4ndar para transacciones, incentivos y gobernanza dentro del ecosistema CHOLO y la CHOLODAO. 
;; Tiene 8 decimales, total de 8,000,000,000 unidades. 
;; Cumple con el est4ndar SIP-010 sin dependencias externas.

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

;; Error Constants
 (define-constant ERR_OWNER_ONLY u100)
 (define-constant ERR_NOT_TOKEN_OWNER u101)
 (define-constant ERR_INVALID_AMOUNT u102)
 (define-constant ERR_INVALID_RECIPIENT u103)
 (define-constant ERR_MAX_SUPPLY_EXCEEDED u104)
 (define-constant ERR_INVALID_OWNER u105)
 (define-constant ERR_UNAUTHORIZED u106)

;; Transfer-many error codes
(define-constant ERR_TRANSFER_FAILED_INVALID_SENDER u201)
(define-constant ERR_TRANSFER_FAILED_INSUFFICIENT_BALANCE u202)
(define-constant ERR_TRANSFER_FAILED_INVALID_RECIPIENT u203)
(define-constant ERR_TRANSFER_FAILED_AMOUNT u204)

;; Event nonce for tracking
 (define-data-var last-event-nonce uint u0)

;; Variables y Constantes
(define-data-var contract-owner principal tx-sender)
(define-data-var total-minted uint u0)
(define-data-var token-uri (string-ascii 256) "https://cholo.meme/metadata")
(define-constant DAO_CONTRACT .cholo-dao)
(define-constant TOKEN_NAME "CHOLO")
(define-constant TOKEN_SYMBOL "CHOLO")
(define-constant TOKEN_DECIMALS u8)
(define-constant MAX_SUPPLY u8000000000)
(define-constant BURN_ADDRESS 'SP000000000000000000002Q6VF78)

;; Event functions
(define-read-only (get-last-event-nonce)
  (ok (var-get last-event-nonce))
)

(define-private (emit-event (event-type (string-ascii 32)))
  (let ((new-nonce (+ (var-get last-event-nonce) u1)))
    (var-set last-event-nonce new-nonce)
    new-nonce))

(define-private (emit-transfer-event (from principal) (to principal) (amount uint))
  (emit-event "transfer"))

(define-private (emit-mint-event (to principal) (amount uint))
  (emit-event "mint"))

(define-private (emit-burn-event (from principal) (amount uint))
  (emit-event "burn"))

;; Funciones Read-Only
(define-read-only (get-balance (who principal))
  (ok (ft-get-balance CHOLO who))
)

(define-read-only (get-total-supply)
  (ok (ft-get-supply CHOLO))
)

(define-read-only (get-name)
  (ok TOKEN_NAME)
)

(define-read-only (get-symbol)
  (ok TOKEN_SYMBOL)
)

(define-read-only (get-decimals)
  (ok TOKEN_DECIMALS)
)

(define-read-only (get-token-uri)
  (ok (some (var-get token-uri)))
)

;; Allow DAO to update token URI
(define-public (update-token-uri (new-uri (string-ascii 256)))
  (begin
    ;; Only the DAO contract can call this function
  (asserts! (is-eq contract-caller DAO_CONTRACT) (err ERR_UNAUTHORIZED))
    ;; Ensure the new URI is not empty
  (asserts! (> (len new-uri) u0) (err ERR_INVALID_AMOUNT))
    ;; Update the URI
    (var-set token-uri new-uri)
    (ok true)
  )
)

;; Funciones Pvblicas
;; @desc Mint tokens to the caller (tx-sender). Only contract owner can mint. Recipient is always tx-sender.
(define-public (mint (amount uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR_OWNER_ONLY))
    (asserts! (> amount u0) (err ERR_INVALID_AMOUNT))
    (let (
      (current-minted (var-get total-minted))
      (new-total (+ current-minted amount))
    )
      (asserts! (<= new-total MAX_SUPPLY) (err ERR_MAX_SUPPLY_EXCEEDED))
      (try! (ft-mint? CHOLO amount tx-sender))
      (var-set total-minted new-total)
      (ok true)
    )
  )
)

;; @desc Transfers tokens from one principal to another
(define-public (transfer
  (amount uint)
  (sender principal)
  (recipient principal)
  (memo (optional (buff 34)))
)
  (begin
  (asserts! (> amount u0) (err ERR_INVALID_AMOUNT))
  (asserts! (is-eq tx-sender sender) (err ERR_NOT_TOKEN_OWNER))
  (asserts! (not (is-eq recipient BURN_ADDRESS)) (err ERR_INVALID_RECIPIENT))
    (match (ft-transfer? CHOLO amount sender recipient)
      success
        (begin
          (emit-transfer-event sender recipient amount)
          (ok true))
      error (err error))
  )
)

(define-public (set-owner (new-owner principal))
  (begin
  (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR_OWNER_ONLY))
  (asserts! (not (is-eq new-owner BURN_ADDRESS)) (err ERR_INVALID_OWNER))
    (var-set contract-owner new-owner)
    (ok true)
  )
)

;; @desc Burns tokens by sending them to the burn address
(define-public (burn (amount uint))
  (begin
  (asserts! (> amount u0) (err ERR_INVALID_AMOUNT))
    (try! (ft-transfer? CHOLO amount tx-sender BURN_ADDRESS))
  (emit-burn-event tx-sender amount)
    (ok true)
  )
)

;; --- Public functions
;; @desc Performs multiple transfers in a single transaction
(define-public (transfer-many
    (recipients (list 200 {
        amount: uint,
        sender: principal,
        to: principal,
        memo: (optional (buff 34))
    })))
    (fold transfer-many-iter recipients (ok u0))
)

 (define-private (transfer-many-iter (individual-transfer {
    amount: uint,
    sender: principal,
    to: principal,
    memo: (optional (buff 34))
  }) (acc (response uint uint)))
  (match acc
    ok-count
      (let ((amount (get amount individual-transfer))
            (sender (get sender individual-transfer))
            (recipient (get to individual-transfer)))
        (begin
          ;; validate inputs and sender
                    (asserts! (is-eq tx-sender sender) (err ERR_TRANSFER_FAILED_INVALID_SENDER))
                    (asserts! (> amount u0) (err ERR_TRANSFER_FAILED_AMOUNT))
                    (asserts! (not (is-eq recipient BURN_ADDRESS)) (err ERR_TRANSFER_FAILED_INVALID_RECIPIENT))
          ;; attempt transfer
          (match (ft-transfer? CHOLO amount sender recipient)
            success (begin
                      ;; emit event (returns uint nonce)
                      (emit-transfer-event sender recipient amount)
                      (ok (+ ok-count u1)))
            error (err ERR_TRANSFER_FAILED_INSUFFICIENT_BALANCE))))
    err-code
      (err err-code)))

;; INIT
(begin
    ;; Initial token distribution
  (try! (ft-mint? CHOLO u8000000000 tx-sender))
  (var-set total-minted MAX_SUPPLY)
  (emit-event "YOLO-CHOLO")
  (ok true)
)

