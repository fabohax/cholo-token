;; STX escrow with immutable recipient, tag and Bitcoin-block duration.
;; Release requires a transaction; anyone may trigger payment to the recipient.
(define-constant ERR_BAD_PARAMS (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_STILL_LOCKED (err u102))
(define-constant ERR_ALREADY_RELEASED (err u103))
(define-constant ERR_INDIRECT_CALL (err u104))
(define-constant MAX_UINT u340282366920938463463374607431768211455)

(define-data-var next-id uint u0)
(define-map locks uint {
  depositor: principal,
  recipient: principal,
  amount: uint,
  tag: (string-utf8 64),
  created-at: uint,
  unlock-at: uint,
  released: bool
})

(define-read-only (get-lock (id uint)) (map-get? locks id))
(define-read-only (get-next-id) (var-get next-id))
(define-read-only (get-current-height) burn-block-height)

;; Amount is in micro-STX. Empty and duplicate tags are allowed.
;; Direct calls prevent intermediaries from locking a user's STX unexpectedly.
(define-public (lock-funds (amount uint) (duration uint) (recipient principal) (tag (string-utf8 64)))
  (let ((id (var-get next-id)) (vault (as-contract tx-sender)))
    (asserts! (is-eq tx-sender contract-caller) ERR_INDIRECT_CALL)
    (asserts! (and (> amount u0) (> duration u0)
      (<= duration (- MAX_UINT burn-block-height)) (< id MAX_UINT)
      (not (is-eq recipient vault))) ERR_BAD_PARAMS)
    (try! (stx-transfer? amount tx-sender vault))
    (map-set locks id {
      depositor: tx-sender, recipient: recipient, amount: amount, tag: tag,
      created-at: burn-block-height, unlock-at: (+ burn-block-height duration), released: false
    })
    (var-set next-id (+ id u1))
    (print {event: "funds-locked", id: id, depositor: tx-sender,
      recipient: recipient, amount: amount, tag: tag, unlock-at: (+ burn-block-height duration)})
    (ok id)))

(define-public (release (id uint))
  (let ((entry (unwrap! (map-get? locks id) ERR_NOT_FOUND)))
    (asserts! (not (get released entry)) ERR_ALREADY_RELEASED)
    (asserts! (>= burn-block-height (get unlock-at entry)) ERR_STILL_LOCKED)
    ;; A failed transfer rolls back this update atomically.
    (map-set locks id (merge entry {released: true}))
    (try! (as-contract (stx-transfer? (get amount entry) tx-sender (get recipient entry))))
    (print {event: "funds-released", id: id, recipient: (get recipient entry),
      amount: (get amount entry), tag: (get tag entry)})
    (ok true)))
