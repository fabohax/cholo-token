;; title: CHOLO SWAP
;; version: 1.0.0
;; summary: Inventory-backed sale of CHOLO for STX, sBTC, or USDCx.

(define-trait sip-010-trait
  (
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))
  ))

(define-constant ERR_OWNER_ONLY          (err u200))
(define-constant ERR_INVALID_AMOUNT      (err u201))
(define-constant ERR_INVALID_RATE        (err u202))
(define-constant ERR_SWAP_PAUSED         (err u203))
(define-constant ERR_SLIPPAGE             (err u204))
(define-constant ERR_WRONG_TOKEN         (err u205))
(define-constant ERR_ASSET_NOT_CONFIGURED (err u206))
(define-constant ERR_INVALID_PRINCIPAL   (err u207))

(define-constant BURN_ADDRESS 'SP000000000000000000002Q6VF78)
(define-constant OFFICIAL_SBTC 'SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4.sbtc-token)
(define-constant OFFICIAL_USDCX 'SP120SBRBQJ00MCWS7TM5R8WJNTTKD5K0HFRC2CNE.usdcx)

(define-data-var owner principal tx-sender)
(define-data-var treasury principal tx-sender)
(define-data-var paused bool false)

;; Rates are expressed as:
;; payment base units * numerator / denominator = CHOLO base units.
(define-data-var stx-rate-numerator uint u0)
(define-data-var stx-rate-denominator uint u1)
(define-data-var sbtc-rate-numerator uint u0)
(define-data-var sbtc-rate-denominator uint u1)
(define-data-var usdcx-rate-numerator uint u0)
(define-data-var usdcx-rate-denominator uint u1)

(define-data-var sbtc-contract (optional principal) none)
(define-data-var usdcx-contract (optional principal) none)

(define-read-only (get-owner) (var-get owner))
(define-read-only (get-treasury) (var-get treasury))
(define-read-only (is-paused) (var-get paused))

(define-read-only (get-stx-rate)
  {numerator: (var-get stx-rate-numerator), denominator: (var-get stx-rate-denominator)})

(define-read-only (get-sbtc-rate)
  {numerator: (var-get sbtc-rate-numerator), denominator: (var-get sbtc-rate-denominator)})

(define-read-only (get-usdcx-rate)
  {numerator: (var-get usdcx-rate-numerator), denominator: (var-get usdcx-rate-denominator)})

(define-read-only (get-sbtc-contract) (var-get sbtc-contract))
(define-read-only (get-usdcx-contract) (var-get usdcx-contract))

(define-read-only (quote-stx (amount uint))
  (quote amount (var-get stx-rate-numerator) (var-get stx-rate-denominator)))

(define-read-only (quote-sbtc (amount uint))
  (quote amount (var-get sbtc-rate-numerator) (var-get sbtc-rate-denominator)))

(define-read-only (quote-usdcx (amount uint))
  (quote amount (var-get usdcx-rate-numerator) (var-get usdcx-rate-denominator)))

(define-private (quote (amount uint) (numerator uint) (denominator uint))
  (if (or (is-eq amount u0) (is-eq numerator u0))
      u0
      (/ (* amount numerator) denominator)))

(define-private (assert-owner)
  (begin
    (asserts! (is-eq tx-sender (var-get owner)) ERR_OWNER_ONLY)
    (ok true)))

(define-private (validate-rate (numerator uint) (denominator uint))
  (begin
    (asserts! (> numerator u0) ERR_INVALID_RATE)
    (asserts! (> denominator u0) ERR_INVALID_RATE)
    (ok true)))

(define-private (validate-swap (amount uint) (output uint) (min-output uint))
  (begin
    (asserts! (not (var-get paused)) ERR_SWAP_PAUSED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> output u0) ERR_INVALID_AMOUNT)
    (asserts! (>= output min-output) ERR_SLIPPAGE)
    (ok true)))

(define-private (send-cholo (amount uint) (recipient principal))
  (as-contract
    (contract-call? .cholo transfer amount tx-sender recipient none)))

(define-public (buy-with-stx (amount uint) (min-cholo-out uint))
  (let ((buyer tx-sender)
        (output (quote amount (var-get stx-rate-numerator) (var-get stx-rate-denominator))))
    (try! (validate-swap amount output min-cholo-out))
    (try! (stx-transfer? amount buyer (var-get treasury)))
    (try! (send-cholo output buyer))
    (print {event: "swap", asset: "STX", buyer: buyer, amount-in: amount, cholo-out: output})
    (ok output)))

(define-public (buy-with-sbtc
    (amount uint)
    (min-cholo-out uint)
    (token <sip-010-trait>))
  (buy-with-token
    amount
    min-cholo-out
    token
    (var-get sbtc-contract)
    (var-get sbtc-rate-numerator)
    (var-get sbtc-rate-denominator)
    "sBTC"))

(define-public (buy-with-usdcx
    (amount uint)
    (min-cholo-out uint)
    (token <sip-010-trait>))
  (buy-with-token
    amount
    min-cholo-out
    token
    (var-get usdcx-contract)
    (var-get usdcx-rate-numerator)
    (var-get usdcx-rate-denominator)
    "USDCx"))

(define-private (buy-with-token
    (amount uint)
    (min-cholo-out uint)
    (token <sip-010-trait>)
    (configured-token (optional principal))
    (numerator uint)
    (denominator uint)
    (asset (string-ascii 5)))
  (let ((buyer tx-sender)
        (output (quote amount numerator denominator)))
    (try! (validate-swap amount output min-cholo-out))
    (try! (match configured-token
      expected (begin
        (asserts! (is-eq expected (contract-of token)) ERR_WRONG_TOKEN)
        (ok true))
      ERR_ASSET_NOT_CONFIGURED))
    (try! (contract-call? token transfer amount buyer (var-get treasury) none))
    (try! (send-cholo output buyer))
    (print {event: "swap", asset: asset, buyer: buyer, amount-in: amount, cholo-out: output})
    (ok output)))

(define-public (set-stx-rate (numerator uint) (denominator uint))
  (begin
    (try! (assert-owner))
    (try! (validate-rate numerator denominator))
    (var-set stx-rate-numerator numerator)
    (var-set stx-rate-denominator denominator)
    (ok true)))

(define-public (set-sbtc-config (token principal) (numerator uint) (denominator uint))
  (begin
    (try! (assert-owner))
    (asserts! (is-eq token OFFICIAL_SBTC) ERR_WRONG_TOKEN)
    (try! (validate-rate numerator denominator))
    (var-set sbtc-contract (some token))
    (var-set sbtc-rate-numerator numerator)
    (var-set sbtc-rate-denominator denominator)
    (ok true)))

(define-public (set-usdcx-config (token principal) (numerator uint) (denominator uint))
  (begin
    (try! (assert-owner))
    (asserts! (is-eq token OFFICIAL_USDCX) ERR_WRONG_TOKEN)
    (try! (validate-rate numerator denominator))
    (var-set usdcx-contract (some token))
    (var-set usdcx-rate-numerator numerator)
    (var-set usdcx-rate-denominator denominator)
    (ok true)))

(define-public (set-treasury (new-treasury principal))
  (begin
    (try! (assert-owner))
    (asserts! (not (is-eq new-treasury BURN_ADDRESS)) ERR_INVALID_PRINCIPAL)
    (var-set treasury new-treasury)
    (ok true)))

(define-public (set-paused (new-paused bool))
  (begin
    (try! (assert-owner))
    (var-set paused new-paused)
    (ok true)))

(define-public (set-owner (new-owner principal))
  (begin
    (try! (assert-owner))
    (asserts! (not (is-eq new-owner BURN_ADDRESS)) ERR_INVALID_PRINCIPAL)
    (var-set owner new-owner)
    (ok true)))

(define-public (withdraw-cholo (amount uint) (recipient principal))
  (begin
    (try! (assert-owner))
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (not (is-eq recipient BURN_ADDRESS)) ERR_INVALID_PRINCIPAL)
    (try! (send-cholo amount recipient))
    (ok true)))
