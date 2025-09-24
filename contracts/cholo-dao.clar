;; title: CHOLO DAO
;; version: 2.0.1
;; summary: Tesorería multisig para CHOLO DAO en Stacks, con gestión dinámica de signers.
;; description: Contrato multisig que almacena fondos y permite agregar, remover o reemplazar signers mediante propuestas y votos.

;; Dynamic signer set
(define-map signers {idx: uint} principal)
(define-data-var signer-count uint u0)
(define-data-var required-sigs uint u3)
(define-constant MIN_SIGNERS uint u3)

;; Error Codes
(define-constant ERR_NOT_SIGNER (err u100))
(define-constant ERR_ALREADY_EXECUTED (err u101))
(define-constant ERR_ALREADY_APPROVED (err u103))
(define-constant ERR_NOT_FOUND (err u104))
(define-constant ERR_NOT_ENOUGH_APPROVALS (err u102))
(define-constant ERR_PROPOSAL_EXPIRED (err u105))
(define-constant ERR_MIN_SIGNERS (err u106))

(define-map proposals 
  {id: uint}
  {recipient: principal, amount: uint, approvals: uint, executed: bool, proposal-type: (string-ascii 16), new-signer: (optional principal), old-signer: (optional principal), token: (optional principal), description: (string-utf8 256), expiration: uint})
(define-map approvals
  {id: uint, signer: principal}
  bool)

(define-data-var next-id uint u0)

;; Inicialización de signers 
(begin
  (map-set signers {idx: u0} 'SP193GXQTNHVV9WSAPHAB89M6R9QSEXZKS3774CMD) ;; @fabohax
  (map-set signers {idx: u1} 'ST2YDY8H45J5HTN5M0H2XQH0JFCR4RWCA92QCZ7W6) ;; @anthozg
  (map-set signers {idx: u2} 'ST4ZB0M2ZKP1HRZPVAPE4X14K689X22N29YQQBG2) ;; @sirohxi
  (map-set signers {idx: u3} 'ST9E6QNWPX7WVYJWTDAJ4WMXDNFHFSFKF91N68Z7) ;; @navynox
  (map-set signers {idx: u4} 'SP555555555555555555555555555555555555555) ;; @marsetti
  (var-set signer-count u5) ;; inicializar contador
)

;; Depositar STX al contrato
(define-public (deposit (amount uint))
  (stx-transfer? amount tx-sender (as-contract tx-sender))
)

;; Crear propuesta
(define-public (create-proposal (recipient principal) (amount uint) (proposal-type (string-ascii 16)) (new-signer (optional principal)) (old-signer (optional principal)) (token (optional principal)) (description (string-utf8 256)) (expiration uint))
  (begin
    (asserts! (is-signer tx-sender) ERR_NOT_SIGNER)
    (let ((id (var-get next-id)))
      (map-set proposals {id: id}
        {recipient: recipient,
         amount: amount,
         approvals: u0,
         executed: false,
         proposal-type: proposal-type,
         new-signer: new-signer,
         old-signer: old-signer,
         token: token,
         description: description,
         expiration: expiration})
      (var-set next-id (+ id u1))
      (ok id)
    )
  )
)

;; Aprobar propuesta
(define-public (approve-proposal (id uint))
  (let ((prop (map-get? proposals {id: id})))
    (match prop
      proposal
      (begin
        (asserts! (not (get executed proposal)) ERR_ALREADY_EXECUTED)
        (asserts! (is-signer tx-sender) ERR_NOT_SIGNER)
        (asserts! (is-none (map-get? approvals {id: id, signer: tx-sender})) ERR_ALREADY_APPROVED)
        (asserts! (> (get expiration proposal) block-height) ERR_PROPOSAL_EXPIRED)
        (map-set approvals {id: id, signer: tx-sender} true)
        (map-set proposals {id: id}
          {recipient: (get recipient proposal),
           amount: (get amount proposal),
           approvals: (+ (get approvals proposal) u1),
           executed: false,
           proposal-type: (get proposal-type proposal),
           new-signer: (get new-signer proposal),
           old-signer: (get old-signer proposal),
           token: (get token proposal),
           description: (get description proposal),
           expiration: (get expiration proposal)})
        (ok true))
  ERR_NOT_FOUND
    )
  )
)

;; Helper: check if principal is a signer
(define-read-only (is-signer (who principal))
  (let ((count (var-get signer-count)))
    (let loop ((i u0))
      (if (>= i count)
          false
          (match (map-get? signers {idx: i})
            signer (if (is-eq who signer) true (loop (+ i u1)))
            none (loop (+ i u1)))))))
)

;; Ejecutar propuesta si alcanza quorum
(define-public (execute-proposal (id uint))
  (let ((prop (map-get? proposals {id: id})))
    (match prop
      proposal
      (begin
        (asserts! (>= (get approvals proposal) (var-get required-sigs)) ERR_NOT_ENOUGH_APPROVALS)
        (asserts! (not (get executed proposal)) ERR_ALREADY_EXECUTED)
        (asserts! (> (get expiration proposal) block-height) ERR_PROPOSAL_EXPIRED)
        (map-set proposals {id: id}
          {recipient: (get recipient proposal),
           amount: (get amount proposal),
           approvals: (get approvals proposal),
           executed: true,
           proposal-type: (get proposal-type proposal),
           new-signer: (get new-signer proposal),
           old-signer: (get old-signer proposal),
           token: (get token proposal),
           description: (get description proposal),
           expiration: (get expiration proposal)})
        (if (is-eq (get proposal-type proposal) "transfer")
          (stx-transfer? (get amount proposal) (as-contract tx-sender) (get recipient proposal))
          (if (is-eq (get proposal-type proposal) "token-transfer")
            (token-transfer (get token proposal) (get amount proposal) (get recipient proposal))
            (if (is-eq (get proposal-type proposal) "add-signer")
              (add-signer (get new-signer proposal))
              (if (is-eq (get proposal-type proposal) "remove-signer")
                (remove-signer (get old-signer proposal))
                (if (is-eq (get proposal-type proposal) "replace-signer")
                  (replace-signer (get old-signer proposal) (get new-signer proposal))
                  (ok false)
                )
              )
            )
          )
        ))
      ERR_NOT_FOUND
    )
  )
)

;; SIP-010 token transfer helper con validación
(define-private (token-transfer (token-contract (optional principal)) (amount uint) (recipient principal))
  (match token-contract
    some-token
      (as-contract
        (let ((res (contract-call? some-token transfer amount (as-contract tx-sender) recipient none)))
          (match res
            ok-val (ok ok-val)
            err-val (err err-val))))
    none (ok false)))

;; Agregar un signer
(define-private (add-signer (new (optional principal)))
  (match new
    some-new
      (let ((count (var-get signer-count)))
        (map-set signers {idx: count} some-new)
        (var-set signer-count (+ count u1))
        (ok true))
    none (ok false)))

;; Remover un signer
(define-private (remove-signer (old (optional principal)))
  (match old
    some-old
      (let ((count (var-get signer-count)))
        (asserts! (> count MIN_SIGNERS) ERR_MIN_SIGNERS)
        (let ((found-idx (find-signer-index some-old)))
          (match found-idx
            some-idx
              (begin
                ;; Shift all signers after the removed one to fill the gap
                (compact-signers some-idx)
                (var-set signer-count (- count u1))
                (ok true))
            none (ok false))))
    none (ok false)))

;; Reemplazar un signer
(define-private (replace-signer (old (optional principal)) (new (optional principal)))
  (match old
    some-old
      (match new
        some-new
          (let ((found-idx (find-signer-index some-old)))
            (match found-idx
              some-idx
                (begin
                  (map-set signers {idx: some-idx} some-new)
                  (ok true))
              none (ok false)))
        none (ok false))
    none (ok false)))

;; Helper function to find the index of a signer
(define-private (find-signer-index (target principal))
  (let ((count (var-get signer-count)))
    (let loop ((i u0))
      (if (>= i count)
          none
          (let ((current (default-to 'SP193GXQTNHVV9WSAPHAB89M6R9QSEXZKS3774CMD (map-get? signers {idx: i}))))
            (if (is-eq current target)
                (some i)
                (loop (+ i u1))))))))

;; Helper function to compact signers array after removal
(define-private (compact-signers (removed-idx uint))
  (let ((count (var-get signer-count)))
    (let loop ((i removed-idx))
      (if (>= i (- count u1))
          (map-delete signers {idx: (- count u1)})
          (let ((next-signer (default-to 'SP193GXQTNHVV9WSAPHAB89M6R9QSEXZKS3774CMD (map-get? signers {idx: (+ i u1)}))))
            (begin
              (map-set signers {idx: i} next-signer)
              (loop (+ i u1))))))))

;; Read-only functions for querying contract state
(define-read-only (get-signer-count)
  (var-get signer-count))

(define-read-only (get-required-sigs)
  (var-get required-sigs))

(define-read-only (get-signer (idx uint))
  (map-get? signers {idx: idx}))

(define-read-only (get-proposal (id uint))
  (map-get? proposals {id: id}))

(define-read-only (has-approved (id uint) (signer principal))
  (default-to false (map-get? approvals {id: id, signer: signer})))
