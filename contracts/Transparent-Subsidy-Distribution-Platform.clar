(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-registered (err u101))
(define-constant err-already-registered (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-insufficient-balance (err u104))
(define-constant err-not-eligible (err u105))

(define-data-var treasury-balance uint u0)
(define-data-var total-beneficiaries uint u0)
(define-data-var total-distributed uint u0)

(define-map beneficiaries
    principal
    {
        eligible: bool,
        subsidy-type: (string-ascii 20),
        received-amount: uint,
        last-distribution: uint,
    }
)

(define-map subsidy-types
    (string-ascii 20)
    {
        amount: uint,
        period: uint,
        active: bool,
    }
)

(define-public (register-beneficiary
        (beneficiary principal)
        (subsidy-type (string-ascii 20))
    )
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-none (map-get? beneficiaries beneficiary))
            err-already-registered
        )
        (map-set beneficiaries beneficiary {
            eligible: true,
            subsidy-type: subsidy-type,
            received-amount: u0,
            last-distribution: u0,
        })
        (var-set total-beneficiaries (+ (var-get total-beneficiaries) u1))
        (ok true)
    )
)

(define-public (add-subsidy-type
        (name (string-ascii 20))
        (amount uint)
        (period uint)
    )
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set subsidy-types name {
            amount: amount,
            period: period,
            active: true,
        })
        (ok true)
    )
)

(define-public (fund-treasury (amount uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set treasury-balance (+ (var-get treasury-balance) amount))
        (ok true)
    )
)

(define-public (distribute-subsidy (beneficiary principal))
    (let (
            (beneficiary-data (unwrap! (map-get? beneficiaries beneficiary) err-not-registered))
            (subsidy-type (unwrap! (map-get? subsidy-types (get subsidy-type beneficiary-data))
                err-not-eligible
            ))
            (current-time stacks-block-height)
        )
        (asserts! (get eligible beneficiary-data) err-not-eligible)
        (asserts! (get active subsidy-type) err-not-eligible)
        (asserts!
            (>= (- current-time (get last-distribution beneficiary-data))
                (get period subsidy-type)
            )
            err-not-eligible
        )
        (asserts! (>= (var-get treasury-balance) (get amount subsidy-type))
            err-insufficient-balance
        )
        (map-set beneficiaries beneficiary
            (merge beneficiary-data {
                received-amount: (+ (get received-amount beneficiary-data)
                    (get amount subsidy-type)
                ),
                last-distribution: current-time,
            })
        )
        (var-set treasury-balance
            (- (var-get treasury-balance) (get amount subsidy-type))
        )
        (var-set total-distributed
            (+ (var-get total-distributed) (get amount subsidy-type))
        )
        (ok true)
    )
)

(define-read-only (get-beneficiary-info (beneficiary principal))
    (map-get? beneficiaries beneficiary)
)

(define-read-only (get-subsidy-type-info (name (string-ascii 20)))
    (map-get? subsidy-types name)
)

(define-read-only (get-treasury-balance)
    (ok (var-get treasury-balance))
)

(define-read-only (get-platform-stats)
    (ok {
        total-beneficiaries: (var-get total-beneficiaries),
        total-distributed: (var-get total-distributed),
        treasury-balance: (var-get treasury-balance),
    })
)
