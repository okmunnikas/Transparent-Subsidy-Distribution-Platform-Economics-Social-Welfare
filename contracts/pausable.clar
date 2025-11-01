(define-data-var owner (optional principal) none)
(define-data-var paused bool false)

(define-read-only (get-owner)
  (var-get owner)
)

(define-read-only (is-paused)
  (var-get paused)
)

(define-private (is-owner (who principal))
  (match (var-get owner)
    owner-principal (is-eq owner-principal who)
    false)
)

(define-public (init-owner)
  (if (is-none (var-get owner))
      (begin
        (var-set owner (some tx-sender))
        (ok true))
      (err u1))
)

(define-public (transfer-ownership (new-owner principal))
  (if (is-owner tx-sender)
      (begin
        (var-set owner (some new-owner))
        (ok true))
      (err u2))
)

(define-public (pause)
  (if (is-owner tx-sender)
      (if (var-get paused)
          (err u3)
          (begin
            (var-set paused true)
            (ok true)))
      (err u2))
)

(define-public (unpause)
  (if (is-owner tx-sender)
      (if (var-get paused)
          (begin
            (var-set paused false)
            (ok true))
          (err u4))
      (err u2))
)

(define-public (assert-not-paused)
  (if (var-get paused)
      (err u10)
      (ok true))
)
