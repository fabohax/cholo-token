(define-trait sip-010-trait
  (
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))
  ))

(define-fungible-token mock)

(define-read-only (get-balance (who principal))
  (ok (ft-get-balance mock who)))

(define-read-only (get-total-supply)
  (ok (ft-get-supply mock)))

(define-read-only (get-name)
  (ok "Mock Token"))

(define-read-only (get-symbol)
  (ok "MOCK"))

(define-read-only (get-decimals)
  (ok u8))

(define-read-only (get-token-uri)
  (ok none))

(define-public (mint (amount uint) (recipient principal))
  (ft-mint? mock amount recipient))

(define-public (transfer
    (amount uint)
    (sender principal)
    (recipient principal)
    (memo (optional (buff 34))))
  (begin
    (asserts! (is-eq tx-sender sender) (err u1))
    (ft-transfer? mock amount sender recipient)))
