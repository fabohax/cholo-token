;; title: CHOLO DAO
;; version: 1.1.0
;; summary: Multisig treasury for CHOLO DAO on Stacks, with timelock, events and safer signer indexing.
;; description: Holds funds and executes actions via proposals approved by a dynamic signer set.

;; =========================
;; Constants / "Enums"
;; =========================
(define-constant PROPOSAL_TRANSFER          "transfer")           ;; STX transfer
(define-constant PROPOSAL_TOKEN_TRANSFER    "token-transfer")     ;; SIP-010 transfer
(define-constant PROPOSAL_ADD_SIGNER        "add-signer")
(define-constant PROPOSAL_REMOVE_SIGNER     "remove-signer")
(define-constant PROPOSAL_REPLACE_SIGNER    "replace-signer")
(define-constant PROPOSAL_SET_REQUIRED      "set-required-sigs")
(define-constant PROPOSAL_SET_DELAY         "set-exec-delay")

(define-constant MAX_EXECUTION_DELAY u1000) ;; tenures; leaves approval time within MAX_TTL_BLOCKS

(define-constant MIN_SIGNERS                u1)
(define-constant MIN_TTL_BLOCKS             u10)     ;; min blocks until expiration
(define-constant MAX_TTL_BLOCKS             u10000) ;; max blocks until expiration

(define-trait sip-010-trait
  (
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))
  ))

;; =========================
;; Errors
;; =========================
(define-constant ERR_NOT_SIGNER               (err u100))
(define-constant ERR_ALREADY_EXECUTED         (err u101))
(define-constant ERR_NOT_ENOUGH_APPROVALS     (err u102))
(define-constant ERR_ALREADY_APPROVED         (err u103))
(define-constant ERR_NOT_FOUND                (err u104))
(define-constant ERR_PROPOSAL_EXPIRED         (err u105))
(define-constant ERR_MIN_SIGNERS              (err u106))
(define-constant ERR_BAD_PARAMS               (err u107))
(define-constant ERR_UNKNOWN_TYPE             (err u108))
(define-constant ERR_STALE_PROPOSAL           (err u109))
(define-constant ERR_INDIRECT_CALL            (err u110))

;; =========================
;; Storage
;; =========================
(define-map signers        {idx: uint} principal)
(define-map signer-index   {signer: principal} {idx: uint})
(define-data-var signer-count uint u0)

;; quorum: if >0, use fixed value; if =0, compute 51%
(define-data-var required-sigs uint u0)

(define-data-var execution-delay uint u10) ;; timelock (tenures after quorum)

(define-data-var next-id uint u0)

(define-map proposals 
  {id: uint}
  {
    recipient: principal,
    amount: uint,
    approvals: uint,
    executed: bool,
    proposal-type: (string-ascii 20),
    new-signer: (optional principal),
    old-signer: (optional principal),
    token: (optional principal),
    description: (string-utf8 256),
    expiration: uint,
    created: uint,
  new-required: (optional uint),  ;; for set-required-sigs
  new-delay: (optional uint)      ;; for set-exec-delay
  })

;; Any membership, quorum, or delay change invalidates pending proposals.
(define-data-var governance-version uint u0)
(define-map proposal-versions uint uint)
(define-map executable-at uint uint)
(define-map approvals {id: uint, signer: principal} bool)

;; =========================
;; Bootstrap with the deployer. The signer set grows through approved proposals.
;; =========================
(begin
  (map-set signers {idx: u0} tx-sender)
  (map-set signer-index {signer: tx-sender} {idx: u0})
  (var-set signer-count u1)
)

;; =========================
;; Helpers
;; =========================
(define-read-only (is-signer (who principal))
  (is-some (map-get? signer-index {signer: who})))

(define-read-only (get-execution-delay) (var-get execution-delay))
(define-read-only (get-governance-version) (var-get governance-version))
(define-read-only (get-executable-at (id uint)) (map-get? executable-at id))

(define-read-only (get-signer-count) (var-get signer-count))

(define-private (compute-required-sigs-51)
  (let ((count (var-get signer-count)))
    (if (<= count u0)
        u0
        (let ((num (* count u51)))
          (let ((base (/ num u100)) (rem (mod num u100)))
            (let ((res (if (> rem u0) (+ base u1) base)))
              (if (> res u0) res u1)))))))

(define-read-only (get-required-sigs)
  (let ((cfg (var-get required-sigs)))
    (if (> cfg u0) cfg (compute-required-sigs-51))))

(define-read-only (get-signer (idx uint))
  (map-get? signers {idx: idx}))

;; signer slice helpers removed (not used). If you need pagination, reintroduce a non-recursive approach.

(define-read-only (has-approved (id uint) (signer principal))
  (default-to false (map-get? approvals {id: id, signer: signer})))

(define-read-only (get-proposal (id uint))
  (map-get? proposals {id: id}))

;; signer-indexing atomic helpers
(define-private (add-signer-internal (p principal))
  (let ((count (var-get signer-count)))
    (map-set signers {idx: count} p)
    (map-set signer-index {signer: p} {idx: count})
    (var-set signer-count (+ count u1))
    (ok true)))

 (define-private (remove-signer-internal (p principal))
  (let ((count (var-get signer-count)))
    (asserts! (> count MIN_SIGNERS) ERR_MIN_SIGNERS)
    (asserts! (or (is-eq (var-get required-sigs) u0) (<= (var-get required-sigs) (- count u1))) ERR_BAD_PARAMS)
    (match (map-get? signer-index {signer: p})
      e
        (let ((idx (get idx e)) (last-idx (- count u1)))
          (if (is-eq idx last-idx)
              (begin
                (map-delete signers {idx: idx})
                (map-delete signer-index {signer: p})
                (var-set signer-count (- count u1))
                (ok true))
              (match (map-get? signers {idx: last-idx})
                last-p
                  (begin
                    (map-set signers {idx: idx} last-p)
                    (map-set signer-index {signer: last-p} {idx: idx})
                    (map-delete signers {idx: last-idx})
                    (map-delete signer-index {signer: p})
                    (var-set signer-count (- count u1))
                    (ok true))
                ERR_BAD_PARAMS)))
      ERR_BAD_PARAMS)))

(define-private (replace-signer-internal (oldp principal) (newp principal))
  (match (map-get? signer-index {signer: oldp})
    e
      (let ((idx (get idx e)))
        (map-set signers {idx: idx} newp)
        (map-delete signer-index {signer: oldp})
        (map-set signer-index {signer: newp} {idx: idx})
        (ok true))
    ERR_BAD_PARAMS))

;; =========================
;; Public: Funds
;; =========================
(define-public (deposit (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-caller) ERR_INDIRECT_CALL)
    (asserts! (> amount u0) ERR_BAD_PARAMS)
    (stx-transfer? amount tx-sender (as-contract tx-sender))))

;; Reject malformed proposals before they consume an id or enter governance.
(define-private (validate-proposal-params
  (amount uint)
  (proposal-type (string-ascii 20))
  (new-signer (optional principal))
  (old-signer (optional principal))
  (token (optional principal))
  (new-required (optional uint))
  (new-delay (optional uint))
)
  (if (is-eq proposal-type PROPOSAL_TRANSFER)
      (begin
        (asserts! (> amount u0) ERR_BAD_PARAMS)
        (ok true))
  (if (is-eq proposal-type PROPOSAL_TOKEN_TRANSFER)
      (begin
        (asserts! (> amount u0) ERR_BAD_PARAMS)
        (asserts! (is-some token) ERR_BAD_PARAMS)
        (ok true))
  (if (is-eq proposal-type PROPOSAL_ADD_SIGNER)
      (match new-signer
        signer (begin
          (asserts! (not (is-signer signer)) ERR_BAD_PARAMS)
          (ok true))
        ERR_BAD_PARAMS)
  (if (is-eq proposal-type PROPOSAL_REMOVE_SIGNER)
      (match old-signer
        signer (begin
          (asserts! (is-signer signer) ERR_BAD_PARAMS)
          (asserts! (> (var-get signer-count) MIN_SIGNERS) ERR_MIN_SIGNERS)
          (ok true))
        ERR_BAD_PARAMS)
  (if (is-eq proposal-type PROPOSAL_REPLACE_SIGNER)
      (match old-signer
        oldp (match new-signer
          newp (begin
            (asserts! (is-signer oldp) ERR_BAD_PARAMS)
            (asserts! (not (is-signer newp)) ERR_BAD_PARAMS)
            (ok true))
          ERR_BAD_PARAMS)
        ERR_BAD_PARAMS)
  (if (is-eq proposal-type PROPOSAL_SET_REQUIRED)
      (match new-required
        required (begin
          (asserts! (> required u0) ERR_BAD_PARAMS)
          (asserts! (<= required (var-get signer-count)) ERR_BAD_PARAMS)
          (ok true))
        ERR_BAD_PARAMS)
  (if (is-eq proposal-type PROPOSAL_SET_DELAY)
      (match new-delay
        delay (begin
          (asserts! (and (> delay u0) (<= delay MAX_EXECUTION_DELAY)) ERR_BAD_PARAMS)
          (ok true))
        ERR_BAD_PARAMS)
      ERR_UNKNOWN_TYPE))))))))

;; =========================
;; Proposals
;; =========================
(define-public (create-proposal
  (recipient principal)
  (amount uint)
  (proposal-type (string-ascii 20))
  (new-signer (optional principal))
  (old-signer (optional principal))
  (token (optional principal))
  (description (string-utf8 256))
  (expiration uint)               ;; absolute block height
  (new-required (optional uint))  ;; only for PROPOSAL_SET_REQUIRED
  (new-delay (optional uint))     ;; only for PROPOSAL_SET_DELAY
)
  (begin
    (asserts! (is-eq tx-sender contract-caller) ERR_INDIRECT_CALL)
    (asserts! (is-signer tx-sender) ERR_NOT_SIGNER)
    ;; expiration sanity: now + MIN_TTL <= expiration <= now + MAX_TTL
    (asserts! (>= expiration (+ block-height MIN_TTL_BLOCKS)) ERR_BAD_PARAMS)
    (asserts! (<= (- expiration block-height) MAX_TTL_BLOCKS) ERR_BAD_PARAMS)
    (asserts! (> expiration (+ block-height (var-get execution-delay))) ERR_BAD_PARAMS)
    (try! (validate-proposal-params amount proposal-type new-signer old-signer token new-required new-delay))

    (let ((id (var-get next-id)))
      (map-set proposals {id: id}
        {
          recipient: recipient,
          amount: amount,
          approvals: u0,
          executed: false,
          proposal-type: proposal-type,
          new-signer: new-signer,
          old-signer: old-signer,
          token: token,
          description: description,
          expiration: expiration,
          created: block-height,
          new-required: new-required,
          new-delay: new-delay
        })
  (map-set proposal-versions id (var-get governance-version))
  (var-set next-id (+ id u1))
  (print (tuple (event "proposal-created") (id id) (by tx-sender) (type proposal-type)))
  (ok id))))

(define-public (approve-proposal (id uint))
  (begin
    (asserts! (is-eq tx-sender contract-caller) ERR_INDIRECT_CALL)
    (asserts! (is-signer tx-sender) ERR_NOT_SIGNER)
    (let ((p (map-get? proposals {id: id})))
      (match p
        prop
          (begin
          (asserts! (not (get executed prop)) ERR_ALREADY_EXECUTED)
          (asserts! (is-eq (map-get? proposal-versions id) (some (var-get governance-version))) ERR_STALE_PROPOSAL)
          ;; Do not accept votes once there is no time left to complete the timelock.
          (asserts! (or (is-some (map-get? executable-at id))
                        (> (get expiration prop) (+ block-height (var-get execution-delay)))) ERR_BAD_PARAMS)
          (asserts! (is-none (map-get? approvals {id: id, signer: tx-sender})) ERR_ALREADY_APPROVED)
          (asserts! (> (get expiration prop) block-height) ERR_PROPOSAL_EXPIRED)

          (if (and (is-none (map-get? executable-at id))
                   (>= (+ (get approvals prop) u1) (get-required-sigs)))
              (map-set executable-at id (+ block-height (var-get execution-delay)))
              false)
          (map-set approvals {id: id, signer: tx-sender} true)
          (map-set proposals {id: id}
            (merge-proposal-approvals prop (+ (get approvals prop) u1)))
          (print (tuple (event "proposal-approved") (id id) (by tx-sender)))
            (ok true))
        ERR_NOT_FOUND))))

(define-private (merge-proposal-approvals (p {recipient: principal, amount: uint, approvals: uint, executed: bool, proposal-type: (string-ascii 20), new-signer: (optional principal), old-signer: (optional principal), token: (optional principal), description: (string-utf8 256), expiration: uint, created: uint, new-required: (optional uint), new-delay: (optional uint)}) (new-approvals uint))
  {
    recipient: (get recipient p),
    amount: (get amount p),
    approvals: new-approvals,
    executed: (get executed p),
    proposal-type: (get proposal-type p),
    new-signer: (get new-signer p),
    old-signer: (get old-signer p),
    token: (get token p),
    description: (get description p),
    expiration: (get expiration p),
    created: (get created p),
    new-required: (get new-required p),
    new-delay: (get new-delay p)
  })

;; =========================
;; Execute (checks-effects-interactions + timelock)
;; =========================
(define-public (execute-proposal (id uint) (token-contract (optional <sip-010-trait>)))
  (let ((p? (map-get? proposals {id: id})))
    (match p?
      p
        (let ((need (get-required-sigs)))
          (asserts! (>= (get approvals p) need) ERR_NOT_ENOUGH_APPROVALS)
          (asserts! (not (get executed p)) ERR_ALREADY_EXECUTED)
          (asserts! (> (get expiration p) block-height) ERR_PROPOSAL_EXPIRED)
          (asserts! (is-eq (map-get? proposal-versions id) (some (var-get governance-version))) ERR_STALE_PROPOSAL)
          (asserts! (>= block-height (unwrap! (map-get? executable-at id) ERR_BAD_PARAMS)) ERR_BAD_PARAMS)

          ;; effects: mark executed first; if anything fails next, tx reverts atomically
          (map-set proposals {id: id} (set-executed p true))

          ;; interactions:
          (try! (dispatch-execution p token-contract))
          (if (or (is-eq (get proposal-type p) PROPOSAL_TRANSFER)
                  (is-eq (get proposal-type p) PROPOSAL_TOKEN_TRANSFER))
              false
              (var-set governance-version (+ (var-get governance-version) u1)))
          (print (tuple (event "proposal-executed") (id id)))
          (ok true))
      ERR_NOT_FOUND)))

(define-private (set-executed (p {recipient: principal, amount: uint, approvals: uint, executed: bool, proposal-type: (string-ascii 20), new-signer: (optional principal), old-signer: (optional principal), token: (optional principal), description: (string-utf8 256), expiration: uint, created: uint, new-required: (optional uint), new-delay: (optional uint)}) (flag bool))
  {
    recipient: (get recipient p),
    amount: (get amount p),
    approvals: (get approvals p),
    executed: flag,
    proposal-type: (get proposal-type p),
    new-signer: (get new-signer p),
    old-signer: (get old-signer p),
    token: (get token p),
    description: (get description p),
    expiration: (get expiration p),
    created: (get created p),
    new-required: (get new-required p),
    new-delay: (get new-delay p)
  })

(define-private (dispatch-execution (p {recipient: principal, amount: uint, approvals: uint, executed: bool, proposal-type: (string-ascii 20), new-signer: (optional principal), old-signer: (optional principal), token: (optional principal), description: (string-utf8 256), expiration: uint, created: uint, new-required: (optional uint), new-delay: (optional uint)}) (token-contract (optional <sip-010-trait>)))
  (let ((t (get proposal-type p)))
    (if (is-eq t PROPOSAL_TRANSFER)
        (as-contract (stx-transfer? (get amount p) tx-sender (get recipient p)))
    (if (is-eq t PROPOSAL_TOKEN_TRANSFER)
        (token-transfer (get token p) token-contract (get amount p) (get recipient p))
    (if (is-eq t PROPOSAL_ADD_SIGNER)
        (match (get new-signer p)
          some-p (add-signer-safe some-p)
          ERR_BAD_PARAMS)
    (if (is-eq t PROPOSAL_REMOVE_SIGNER)
        (match (get old-signer p)
          some-p (remove-signer-safe some-p)
          ERR_BAD_PARAMS)
    (if (is-eq t PROPOSAL_REPLACE_SIGNER)
        (match (get old-signer p)
          oldp
            (match (get new-signer p)
              newp (replace-signer-safe oldp newp)
              ERR_BAD_PARAMS)
          ERR_BAD_PARAMS)
    (if (is-eq t PROPOSAL_SET_REQUIRED)
        (match (get new-required p)
          nr (begin (asserts! (> nr u0) ERR_BAD_PARAMS)
                    (asserts! (<= nr (var-get signer-count)) ERR_BAD_PARAMS)
                    (var-set required-sigs nr)
                    (ok true))
          ERR_BAD_PARAMS)
    (if (is-eq t PROPOSAL_SET_DELAY)
        (match (get new-delay p)
          nd (begin (asserts! (and (> nd u0) (<= nd MAX_EXECUTION_DELAY)) ERR_BAD_PARAMS)
                    (var-set execution-delay nd)
                    (ok true))
          ERR_BAD_PARAMS)
        ERR_UNKNOWN_TYPE)))))))))

;; =========================
;; Token (SIP-010) transfer helper
;; =========================
(define-private (token-transfer (expected-token (optional principal)) (token-contract (optional <sip-010-trait>)) (amount uint) (recipient principal))
  (match expected-token
    expected
      (match token-contract
        some-token
          (begin
            (asserts! (is-eq expected (contract-of some-token)) ERR_BAD_PARAMS)
            (let ((res (as-contract (contract-call? some-token transfer amount tx-sender recipient none))))
              (match res
                ok-val (if ok-val (ok true) ERR_BAD_PARAMS)
                err-val (err err-val))))
        ERR_BAD_PARAMS)
    ERR_BAD_PARAMS))

;; =========================
;; Signer ops (safe)
;; =========================
(define-private (add-signer-safe (p principal))
  (match (map-get? signer-index {signer: p})
    entry ERR_BAD_PARAMS
    (add-signer-internal p)))

(define-private (remove-signer-safe (p principal))
  (remove-signer-internal p))

(define-private (replace-signer-safe (oldp principal) (newp principal))
  (match (map-get? signer-index {signer: newp})
    entry ERR_BAD_PARAMS
    (replace-signer-internal oldp newp)))
