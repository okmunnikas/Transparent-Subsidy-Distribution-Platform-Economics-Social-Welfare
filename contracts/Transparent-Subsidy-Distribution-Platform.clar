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

;; Multi-signature governance
(define-constant err-not-authorized (err u200))
(define-constant err-already-approved (err u201))
(define-constant err-proposal-not-found (err u202))
(define-constant err-insufficient-approvals (err u203))
(define-constant err-proposal-expired (err u204))

(define-data-var required-approvals uint u2)
(define-data-var proposal-counter uint u0)

(define-map authorized-signers
    principal
    bool
)

(define-map proposals
    uint
    {
        proposal-type: (string-ascii 20),
        beneficiary: principal,
        subsidy-type: (string-ascii 20),
        amount: uint,
        approvals: uint,
        executed: bool,
        expiry: uint,
        proposer: principal,
    }
)

(define-map proposal-approvals
    {
        proposal-id: uint,
        signer: principal,
    }
    bool
)

(define-public (add-authorized-signer (signer principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set authorized-signers signer true)
        (ok true)
    )
)

(define-public (remove-authorized-signer (signer principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-delete authorized-signers signer)
        (ok true)
    )
)

(define-public (propose-beneficiary-registration
        (beneficiary principal)
        (subsidy-type (string-ascii 20))
    )
    (let ((proposal-id (+ (var-get proposal-counter) u1)))
        (asserts! (default-to false (map-get? authorized-signers tx-sender))
            err-not-authorized
        )
        (map-set proposals proposal-id {
            proposal-type: "register",
            beneficiary: beneficiary,
            subsidy-type: subsidy-type,
            amount: u0,
            approvals: u0,
            executed: false,
            expiry: (+ stacks-block-height u144),
            proposer: tx-sender,
        })
        (var-set proposal-counter proposal-id)
        (ok proposal-id)
    )
)

(define-public (approve-proposal (proposal-id uint))
    (let (
            (proposal (unwrap! (map-get? proposals proposal-id) err-proposal-not-found))
            (approval-key {
                proposal-id: proposal-id,
                signer: tx-sender,
            })
        )
        (asserts! (default-to false (map-get? authorized-signers tx-sender))
            err-not-authorized
        )
        (asserts! (< stacks-block-height (get expiry proposal))
            err-proposal-expired
        )
        (asserts! (not (get executed proposal)) err-proposal-not-found)
        (asserts! (is-none (map-get? proposal-approvals approval-key))
            err-already-approved
        )
        (map-set proposal-approvals approval-key true)
        (map-set proposals proposal-id
            (merge proposal { approvals: (+ (get approvals proposal) u1) })
        )
        (ok true)
    )
)

(define-public (execute-proposal (proposal-id uint))
    (let ((proposal (unwrap! (map-get? proposals proposal-id) err-proposal-not-found)))
        (asserts! (>= (get approvals proposal) (var-get required-approvals))
            err-insufficient-approvals
        )
        (asserts! (< stacks-block-height (get expiry proposal))
            err-proposal-expired
        )
        (asserts! (not (get executed proposal)) err-proposal-not-found)
        (if (is-eq (get proposal-type proposal) "register")
            (begin
                (try! (register-beneficiary (get beneficiary proposal)
                    (get subsidy-type proposal)
                ))
                (map-set proposals proposal-id
                    (merge proposal { executed: true })
                )
                (ok true)
            )
            (ok false)
        )
    )
)

(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals proposal-id)
)

(define-read-only (is-authorized-signer (signer principal))
    (default-to false (map-get? authorized-signers signer))
)

;; Recurring distribution schedules
(define-constant err-schedule-not-found (err u300))
(define-constant err-invalid-schedule (err u301))
(define-constant err-distribution-not-due (err u302))

(define-data-var schedule-counter uint u0)

(define-map recurring-schedules
    uint
    {
        beneficiary: principal,
        subsidy-type: (string-ascii 20),
        frequency: uint,
        next-distribution: uint,
        total-distributions: uint,
        max-distributions: uint,
        active: bool,
    }
)

(define-map beneficiary-schedules
    principal
    (list 10 uint)
)

(define-public (create-recurring-schedule
        (beneficiary principal)
        (subsidy-type (string-ascii 20))
        (frequency uint)
        (max-distributions uint)
    )
    (let (
            (schedule-id (+ (var-get schedule-counter) u1))
            (beneficiary-data (unwrap! (map-get? beneficiaries beneficiary) err-not-registered))
            (current-schedules (default-to (list) (map-get? beneficiary-schedules beneficiary)))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (get eligible beneficiary-data) err-not-eligible)
        (asserts! (> frequency u0) err-invalid-schedule)
        (asserts! (> max-distributions u0) err-invalid-schedule)
        (map-set recurring-schedules schedule-id {
            beneficiary: beneficiary,
            subsidy-type: subsidy-type,
            frequency: frequency,
            next-distribution: (+ stacks-block-height frequency),
            total-distributions: u0,
            max-distributions: max-distributions,
            active: true,
        })
        (map-set beneficiary-schedules beneficiary
            (unwrap! (as-max-len? (append current-schedules schedule-id) u10)
                err-invalid-schedule
            ))
        (var-set schedule-counter schedule-id)
        (ok schedule-id)
    )
)

(define-public (execute-recurring-distribution (schedule-id uint))
    (let (
            (schedule (unwrap! (map-get? recurring-schedules schedule-id)
                err-schedule-not-found
            ))
            (beneficiary-data (unwrap! (map-get? beneficiaries (get beneficiary schedule))
                err-not-registered
            ))
            (subsidy-type-data (unwrap! (map-get? subsidy-types (get subsidy-type schedule))
                err-not-eligible
            ))
        )
        (asserts! (get active schedule) err-invalid-schedule)
        (asserts! (<= (get next-distribution schedule) stacks-block-height)
            err-distribution-not-due
        )
        (asserts!
            (< (get total-distributions schedule)
                (get max-distributions schedule)
            )
            err-invalid-schedule
        )
        (asserts! (>= (var-get treasury-balance) (get amount subsidy-type-data))
            err-insufficient-balance
        )
        (map-set beneficiaries (get beneficiary schedule)
            (merge beneficiary-data {
                received-amount: (+ (get received-amount beneficiary-data)
                    (get amount subsidy-type-data)
                ),
                last-distribution: stacks-block-height,
            })
        )
        (var-set treasury-balance
            (- (var-get treasury-balance) (get amount subsidy-type-data))
        )
        (var-set total-distributed
            (+ (var-get total-distributed) (get amount subsidy-type-data))
        )
        (let (
                (new-total-distributions (+ (get total-distributions schedule) u1))
                (schedule-still-active (< new-total-distributions (get max-distributions schedule)))
            )
            (map-set recurring-schedules schedule-id
                (merge schedule {
                    next-distribution: (+ stacks-block-height (get frequency schedule)),
                    total-distributions: new-total-distributions,
                    active: schedule-still-active,
                })
            )
        )
        (ok true)
    )
)

(define-public (deactivate-schedule (schedule-id uint))
    (let ((schedule (unwrap! (map-get? recurring-schedules schedule-id)
            err-schedule-not-found
        )))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set recurring-schedules schedule-id
            (merge schedule { active: false })
        )
        (ok true)
    )
)

(define-public (batch-execute-due-distributions (schedule-ids (list 20 uint)))
    (let ((results (map execute-recurring-distribution schedule-ids)))
        (ok results)
    )
)

(define-private (execute-single-if-due (schedule-id uint))
    (match (map-get? recurring-schedules schedule-id)
        schedule (if (and
                (get active schedule)
                (<= (get next-distribution schedule) stacks-block-height)
                (< (get total-distributions schedule)
                    (get max-distributions schedule)
                )
            )
            (execute-recurring-distribution schedule-id)
            (ok false)
        )
        (ok false)
    )
)

(define-read-only (get-schedule (schedule-id uint))
    (map-get? recurring-schedules schedule-id)
)

(define-read-only (get-beneficiary-schedules (beneficiary principal))
    (map-get? beneficiary-schedules beneficiary)
)

(define-read-only (get-due-schedules-count)
    (ok (var-get schedule-counter))
)

;; Audit and transparency features
(define-constant err-invalid-filter (err u400))
(define-constant err-log-not-found (err u401))

(define-data-var transaction-counter uint u0)

(define-map transaction-logs
    uint
    {
        beneficiary: principal,
        subsidy-type: (string-ascii 20),
        amount: uint,
        timestamp: uint,
        transaction-type: (string-ascii 15),
        executor: principal,
        schedule-id: (optional uint),
    }
)

(define-map beneficiary-transaction-history
    principal
    (list 50 uint)
)

(define-map monthly-summaries
    {
        year: uint,
        month: uint,
        subsidy-type: (string-ascii 20),
    }
    {
        total-amount: uint,
        transaction-count: uint,
        unique-beneficiaries: uint,
    }
)

(define-private (update-monthly-summary
        (subsidy-type (string-ascii 20))
        (amount uint)
    )
    (let (
            (current-year (/ stacks-block-height u52560))
            (current-month (/ (mod stacks-block-height u52560) u4380))
            (summary-key {
                year: current-year,
                month: current-month,
                subsidy-type: subsidy-type,
            })
            (existing-summary (default-to {
                total-amount: u0,
                transaction-count: u0,
                unique-beneficiaries: u0,
            }
                (map-get? monthly-summaries summary-key)
            ))
        )
        (map-set monthly-summaries summary-key {
            total-amount: (+ (get total-amount existing-summary) amount),
            transaction-count: (+ (get transaction-count existing-summary) u1),
            unique-beneficiaries: (get unique-beneficiaries existing-summary),
        })
        (ok true)
    )
)

(define-public (log-distribution-transaction
        (beneficiary principal)
        (subsidy-type (string-ascii 20))
        (amount uint)
        (transaction-type (string-ascii 15))
        (schedule-id (optional uint))
    )
    (let (
            (transaction-id (+ (var-get transaction-counter) u1))
            (current-history (default-to (list)
                (map-get? beneficiary-transaction-history beneficiary)
            ))
        )
        (map-set transaction-logs transaction-id {
            beneficiary: beneficiary,
            subsidy-type: subsidy-type,
            amount: amount,
            timestamp: stacks-block-height,
            transaction-type: transaction-type,
            executor: tx-sender,
            schedule-id: schedule-id,
        })
        (map-set beneficiary-transaction-history beneficiary
            (unwrap! (as-max-len? (append current-history transaction-id) u50)
                (err u1)
            ))
        (var-set transaction-counter transaction-id)
        (unwrap! (update-monthly-summary subsidy-type amount) err-invalid-filter)
        (ok transaction-id)
    )
)

(define-read-only (get-transaction-log (transaction-id uint))
    (map-get? transaction-logs transaction-id)
)

(define-read-only (get-beneficiary-history (beneficiary principal))
    (map-get? beneficiary-transaction-history beneficiary)
)

(define-read-only (get-monthly-summary
        (year uint)
        (month uint)
        (subsidy-type (string-ascii 20))
    )
    (map-get? monthly-summaries {
        year: year,
        month: month,
        subsidy-type: subsidy-type,
    })
)

(define-read-only (get-recent-transactions)
    (ok (var-get transaction-counter))
)

(define-read-only (get-audit-statistics)
    (ok {
        total-transactions: (var-get transaction-counter),
        current-block: stacks-block-height,
    })
)
(define-constant err-contract-paused (err u500))
(define-constant err-withdrawal-limit-exceeded (err u501))
(define-constant err-suspicious-activity (err u502))
(define-constant err-emergency-only (err u503))
(define-constant err-cooldown-active (err u504))

(define-data-var contract-paused bool false)
(define-data-var emergency-mode bool false)
(define-data-var daily-withdrawal-limit uint u1000000)
(define-data-var hourly-withdrawal-limit uint u100000)
(define-data-var pause-reason (string-ascii 50) "")

(define-map daily-withdrawals
    uint
    uint
)

(define-map hourly-withdrawals
    uint
    uint
)

(define-map beneficiary-activity
    principal
    {
        last-distribution: uint,
        distributions-today: uint,
        total-received-today: uint,
        suspicious-flags: uint,
    }
)

(define-map emergency-operators
    principal
    bool
)

(define-public (pause-contract (reason (string-ascii 50)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set contract-paused true)
        (var-set pause-reason reason)
        (ok true)
    )
)

(define-public (unpause-contract)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set contract-paused false)
        (var-set pause-reason "")
        (ok true)
    )
)

(define-public (activate-emergency-mode)
    (begin
        (asserts!
            (or
                (is-eq tx-sender contract-owner)
                (default-to false (map-get? emergency-operators tx-sender))
            )
            err-not-authorized
        )
        (var-set emergency-mode true)
        (var-set contract-paused true)
        (var-set pause-reason "EMERGENCY_MODE_ACTIVATED")
        (ok true)
    )
)

(define-public (deactivate-emergency-mode)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set emergency-mode false)
        (var-set contract-paused false)
        (var-set pause-reason "")
        (ok true)
    )
)

(define-public (add-emergency-operator (operator principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set emergency-operators operator true)
        (ok true)
    )
)

(define-public (remove-emergency-operator (operator principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-delete emergency-operators operator)
        (ok true)
    )
)

(define-public (set-withdrawal-limits
        (daily-limit uint)
        (hourly-limit uint)
    )
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set daily-withdrawal-limit daily-limit)
        (var-set hourly-withdrawal-limit hourly-limit)
        (ok true)
    )
)

(define-public (emergency-withdraw (amount uint))
    (begin
        (asserts! (var-get emergency-mode) err-emergency-only)
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (>= (var-get treasury-balance) amount) err-insufficient-balance)
        (var-set treasury-balance (- (var-get treasury-balance) amount))
        (ok true)
    )
)

(define-private (check-withdrawal-limits (amount uint))
    (let (
            (current-day (/ stacks-block-height u144))
            (current-hour (/ stacks-block-height u6))
            (daily-total (default-to u0 (map-get? daily-withdrawals current-day)))
            (hourly-total (default-to u0 (map-get? hourly-withdrawals current-hour)))
        )
        (asserts! (<= (+ daily-total amount) (var-get daily-withdrawal-limit))
            err-withdrawal-limit-exceeded
        )
        (asserts! (<= (+ hourly-total amount) (var-get hourly-withdrawal-limit))
            err-withdrawal-limit-exceeded
        )
        (map-set daily-withdrawals current-day (+ daily-total amount))
        (map-set hourly-withdrawals current-hour (+ hourly-total amount))
        (ok true)
    )
)

(define-private (check-suspicious-activity
        (beneficiary principal)
        (amount uint)
    )
    (let (
            (current-day (/ stacks-block-height u144))
            (activity (default-to {
                last-distribution: u0,
                distributions-today: u0,
                total-received-today: u0,
                suspicious-flags: u0,
            }
                (map-get? beneficiary-activity beneficiary)
            ))
            (last-distribution-day (/ (get last-distribution activity) u144))
            (is-same-day (is-eq current-day last-distribution-day))
            (new-distributions-today (if is-same-day
                (+ (get distributions-today activity) u1)
                u1
            ))
            (new-total-today (if is-same-day
                (+ (get total-received-today activity) amount)
                amount
            ))
        )
        (asserts! (< new-distributions-today u10) err-suspicious-activity)
        (asserts! (< new-total-today u500000) err-suspicious-activity)
        (map-set beneficiary-activity beneficiary {
            last-distribution: stacks-block-height,
            distributions-today: new-distributions-today,
            total-received-today: new-total-today,
            suspicious-flags: (get suspicious-flags activity),
        })
        (ok true)
    )
)

(define-private (pre-distribution-checks
        (beneficiary principal)
        (amount uint)
    )
    (begin
        (asserts! (not (var-get contract-paused)) err-contract-paused)
        (try! (check-withdrawal-limits amount))
        (try! (check-suspicious-activity beneficiary amount))
        (ok true)
    )
)

(define-read-only (get-contract-status)
    (ok {
        paused: (var-get contract-paused),
        emergency-mode: (var-get emergency-mode),
        pause-reason: (var-get pause-reason),
        daily-limit: (var-get daily-withdrawal-limit),
        hourly-limit: (var-get hourly-withdrawal-limit),
    })
)

(define-read-only (get-withdrawal-status)
    (let (
            (current-day (/ stacks-block-height u144))
            (current-hour (/ stacks-block-height u6))
        )
        (ok {
            daily-used: (default-to u0 (map-get? daily-withdrawals current-day)),
            hourly-used: (default-to u0 (map-get? hourly-withdrawals current-hour)),
            daily-remaining: (- (var-get daily-withdrawal-limit)
                (default-to u0 (map-get? daily-withdrawals current-day))
            ),
            hourly-remaining: (- (var-get hourly-withdrawal-limit)
                (default-to u0 (map-get? hourly-withdrawals current-hour))
            ),
        })
    )
)

(define-read-only (get-beneficiary-activity (beneficiary principal))
    (map-get? beneficiary-activity beneficiary)
)

(define-read-only (is-emergency-operator (operator principal))
    (default-to false (map-get? emergency-operators operator))
)

(define-constant err-verification-failed (err u600))
(define-constant err-verification-not-found (err u601))
(define-constant err-insufficient-verification-score (err u602))
(define-constant err-verification-expired (err u603))
(define-constant err-kyc-required (err u604))

(define-constant err-invalid-indicator (err u700))
(define-constant err-adjustment-factor-out-of-range (err u701))
(define-constant err-indicator-not-found (err u702))

(define-data-var minimum-verification-score uint u70)
(define-data-var verification-expiry-period uint u2016)
(define-data-var verification-counter uint u0)

(define-data-var economic-adjustment-enabled bool false)
(define-data-var base-adjustment-factor uint u100)
(define-data-var indicator-counter uint u0)

(define-map economic-indicators
    (string-ascii 20)
    {
        value: uint,
        baseline: uint,
        weight: uint,
        last-updated: uint,
        update-frequency: uint,
        active: bool,
    }
)

(define-map adjustment-history
    uint
    {
        indicator: (string-ascii 20),
        old-value: uint,
        new-value: uint,
        adjustment-factor: uint,
        timestamp: uint,
        updater: principal,
    }
)

(define-map verification-records
    principal
    {
        kyc-status: bool,
        identity-verified: bool,
        income-verified: bool,
        address-verified: bool,
        verification-score: uint,
        last-verification: uint,
        verification-expiry: uint,
        fraud-flags: uint,
        reputation-score: uint,
        verification-level: uint,
    }
)

(define-map verification-criteria
    (string-ascii 20)
    {
        weight: uint,
        required: bool,
        max-score: uint,
        active: bool,
    }
)

(define-map verification-history
    principal
    (list 20 uint)
)

(define-map verification-sessions
    uint
    {
        beneficiary: principal,
        verifier: principal,
        timestamp: uint,
        criteria-checked: (list 10 (string-ascii 20)),
        score-awarded: uint,
        session-type: (string-ascii 15),
        notes: (string-ascii 100),
    }
)

(define-public (initialize-verification-criteria)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set verification-criteria "kyc" {
            weight: u30,
            required: true,
            max-score: u30,
            active: true,
        })
        (map-set verification-criteria "identity" {
            weight: u25,
            required: true,
            max-score: u25,
            active: true,
        })
        (map-set verification-criteria "income" {
            weight: u20,
            required: false,
            max-score: u20,
            active: true,
        })
        (map-set verification-criteria "address" {
            weight: u15,
            required: false,
            max-score: u15,
            active: true,
        })
        (map-set verification-criteria "reputation" {
            weight: u10,
            required: false,
            max-score: u10,
            active: true,
        })
        (ok true)
    )
)

(define-public (update-verification-criteria
        (criteria-name (string-ascii 20))
        (weight uint)
        (required bool)
        (max-score uint)
    )
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set verification-criteria criteria-name {
            weight: weight,
            required: required,
            max-score: max-score,
            active: true,
        })
        (ok true)
    )
)

(define-public (verify-beneficiary-identity
        (beneficiary principal)
        (kyc-status bool)
        (identity-verified bool)
        (income-verified bool)
        (address-verified bool)
    )
    (let (
            (session-id (+ (var-get verification-counter) u1))
            (calculated-score (calculate-verification-score kyc-status identity-verified
                income-verified address-verified u0
            ))
            (current-history (default-to (list) (map-get? verification-history beneficiary)))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set verification-records beneficiary {
            kyc-status: kyc-status,
            identity-verified: identity-verified,
            income-verified: income-verified,
            address-verified: address-verified,
            verification-score: calculated-score,
            last-verification: stacks-block-height,
            verification-expiry: (+ stacks-block-height (var-get verification-expiry-period)),
            fraud-flags: u0,
            reputation-score: u50,
            verification-level: (if (>= calculated-score u90)
                u3
                (if (>= calculated-score u70)
                    u2
                    u1
                )
            ),
        })
        (map-set verification-sessions session-id {
            beneficiary: beneficiary,
            verifier: tx-sender,
            timestamp: stacks-block-height,
            criteria-checked: (list "kyc" "identity" "income" "address" "" "" "" "" "" ""),
            score-awarded: calculated-score,
            session-type: "full-verify",
            notes: "Complete verification session performed successfully with all required criteria checked",
        })
        (map-set verification-history beneficiary
            (unwrap! (as-max-len? (append current-history session-id) u20)
                err-verification-failed
            ))
        (var-set verification-counter session-id)
        (ok calculated-score)
    )
)

(define-private (calculate-verification-score
        (kyc-status bool)
        (identity-verified bool)
        (income-verified bool)
        (address-verified bool)
        (reputation-bonus uint)
    )
    (let (
            (kyc-score (if kyc-status
                u30
                u0
            ))
            (identity-score (if identity-verified
                u25
                u0
            ))
            (income-score (if income-verified
                u20
                u0
            ))
            (address-score (if address-verified
                u15
                u0
            ))
            (base-score (+ kyc-score (+ identity-score (+ income-score address-score))))
        )
        (+ base-score reputation-bonus)
    )
)

(define-public (update-reputation-score
        (beneficiary principal)
        (adjustment int)
        (reason (string-ascii 50))
    )
    (let (
            (verification-data (unwrap! (map-get? verification-records beneficiary)
                err-verification-not-found
            ))
            (current-reputation (get reputation-score verification-data))
            (new-reputation (if (< adjustment 0)
                (if (>= current-reputation (to-uint (- 0 adjustment)))
                    (- current-reputation (to-uint (- 0 adjustment)))
                    u0
                )
                (+ current-reputation (to-uint adjustment))
            ))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set verification-records beneficiary
            (merge verification-data {
                reputation-score: new-reputation,
                verification-score: (+ (get verification-score verification-data)
                    (if (> new-reputation current-reputation)
                        (- new-reputation current-reputation)
                        u0
                    )),
            })
        )
        (ok new-reputation)
    )
)

(define-public (flag-suspicious-beneficiary
        (beneficiary principal)
        (flag-type (string-ascii 30))
    )
    (let ((verification-data (unwrap! (map-get? verification-records beneficiary)
            err-verification-not-found
        )))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set verification-records beneficiary
            (merge verification-data {
                fraud-flags: (+ (get fraud-flags verification-data) u1),
                reputation-score: (if (>= (get reputation-score verification-data) u10)
                    (- (get reputation-score verification-data) u10)
                    u0
                ),
            })
        )
        (ok true)
    )
)

(define-public (verify-eligibility-for-distribution (beneficiary principal))
    (let (
            (verification-data (unwrap! (map-get? verification-records beneficiary)
                err-verification-not-found
            ))
            (beneficiary-data (unwrap! (map-get? beneficiaries beneficiary) err-not-registered))
        )
        (asserts! (get kyc-status verification-data) err-kyc-required)
        (asserts!
            (>= (get verification-score verification-data)
                (var-get minimum-verification-score)
            )
            err-insufficient-verification-score
        )
        (asserts!
            (< stacks-block-height (get verification-expiry verification-data))
            err-verification-expired
        )
        (asserts! (< (get fraud-flags verification-data) u3)
            err-verification-failed
        )
        (asserts! (get eligible beneficiary-data) err-not-eligible)
        (ok true)
    )
)

(define-public (batch-verify-eligibility (beneficiaries-list (list 10 principal)))
    (let ((results (map verify-eligibility-for-distribution beneficiaries-list)))
        (ok results)
    )
)

(define-public (renew-verification (beneficiary principal))
    (let ((verification-data (unwrap! (map-get? verification-records beneficiary)
            err-verification-not-found
        )))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set verification-records beneficiary
            (merge verification-data {
                last-verification: stacks-block-height,
                verification-expiry: (+ stacks-block-height (var-get verification-expiry-period)),
            })
        )
        (ok true)
    )
)

(define-public (set-verification-parameters
        (minimum-score uint)
        (expiry-period uint)
    )
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set minimum-verification-score minimum-score)
        (var-set verification-expiry-period expiry-period)
        (ok true)
    )
)

(define-public (enhanced-distribute-subsidy (beneficiary principal))
    (begin
        (try! (verify-eligibility-for-distribution beneficiary))
        (try! (pre-distribution-checks beneficiary
            (get amount
                (unwrap!
                    (map-get? subsidy-types
                        (get subsidy-type
                            (unwrap! (map-get? beneficiaries beneficiary)
                                err-not-registered
                            ))
                    )
                    err-not-eligible
                ))
        ))
        (distribute-subsidy beneficiary)
    )
)

(define-read-only (get-verification-record (beneficiary principal))
    (map-get? verification-records beneficiary)
)

(define-read-only (get-verification-criteria (criteria-name (string-ascii 20)))
    (map-get? verification-criteria criteria-name)
)

(define-read-only (get-verification-session (session-id uint))
    (map-get? verification-sessions session-id)
)

(define-read-only (get-verification-history (beneficiary principal))
    (map-get? verification-history beneficiary)
)

(define-read-only (check-verification-status (beneficiary principal))
    (match (map-get? verification-records beneficiary)
        verification-data (ok {
            is-verified: (>= (get verification-score verification-data)
                (var-get minimum-verification-score)
            ),
            score: (get verification-score verification-data),
            expires-at: (get verification-expiry verification-data),
            is-expired: (>= stacks-block-height (get verification-expiry verification-data)),
            fraud-flags: (get fraud-flags verification-data),
            reputation: (get reputation-score verification-data),
        })
        (err err-verification-not-found)
    )
)

(define-read-only (get-verification-stats)
    (ok {
        total-sessions: (var-get verification-counter),
        minimum-required-score: (var-get minimum-verification-score),
        expiry-period: (var-get verification-expiry-period),
        current-block: stacks-block-height,
    })
)

(define-public (initialize-economic-indicators)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set economic-indicators "inflation" {
            value: u100,
            baseline: u100,
            weight: u50,
            last-updated: stacks-block-height,
            update-frequency: u2016,
            active: true,
        })
        (map-set economic-indicators "cost-of-living" {
            value: u100,
            baseline: u100,
            weight: u30,
            last-updated: stacks-block-height,
            update-frequency: u2016,
            active: true,
        })
        (map-set economic-indicators "poverty-line" {
            value: u100,
            baseline: u100,
            weight: u20,
            last-updated: stacks-block-height,
            update-frequency: u4032,
            active: true,
        })
        (var-set economic-adjustment-enabled true)
        (ok true)
    )
)

(define-public (update-economic-indicator
        (indicator-name (string-ascii 20))
        (new-value uint)
    )
    (let (
            (indicator (unwrap! (map-get? economic-indicators indicator-name)
                err-indicator-not-found
            ))
            (adjustment-id (+ (var-get indicator-counter) u1))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (> new-value u0) err-invalid-indicator)
        (asserts! (< new-value u500) err-invalid-indicator)
        (asserts!
            (>= (- stacks-block-height (get last-updated indicator))
                (get update-frequency indicator)
            )
            err-invalid-indicator
        )
        (map-set adjustment-history adjustment-id {
            indicator: indicator-name,
            old-value: (get value indicator),
            new-value: new-value,
            adjustment-factor: (var-get base-adjustment-factor),
            timestamp: stacks-block-height,
            updater: tx-sender,
        })
        (map-set economic-indicators indicator-name
            (merge indicator {
                value: new-value,
                last-updated: stacks-block-height,
            })
        )
        (var-set indicator-counter adjustment-id)
        (ok new-value)
    )
)

(define-public (toggle-economic-adjustments)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set economic-adjustment-enabled
            (not (var-get economic-adjustment-enabled))
        )
        (ok (var-get economic-adjustment-enabled))
    )
)

(define-private (calculate-adjustment-factor)
    (if (var-get economic-adjustment-enabled)
        (let (
                (inflation-data (unwrap! (map-get? economic-indicators "inflation") (ok u100)))
                (col-data (unwrap! (map-get? economic-indicators "cost-of-living")
                    (ok u100)
                ))
                (poverty-data (unwrap! (map-get? economic-indicators "poverty-line") (ok u100)))
                (inflation-factor (* (/ (get value inflation-data) (get baseline inflation-data))
                    (get weight inflation-data)
                ))
                (col-factor (* (/ (get value col-data) (get baseline col-data))
                    (get weight col-data)
                ))
                (poverty-factor (* (/ (get value poverty-data) (get baseline poverty-data))
                    (get weight poverty-data)
                ))
                (total-factor (+ inflation-factor (+ col-factor poverty-factor)))
                (normalized-factor (/ total-factor u100))
            )
            (ok (if (> normalized-factor u50)
                normalized-factor
                u50
            ))
        )
        (ok u100)
    )
)

(define-private (apply-economic-adjustment (base-amount uint))
    (let ((adjustment-factor (unwrap-panic (calculate-adjustment-factor))))
        (* base-amount (/ adjustment-factor u100))
    )
)

(define-public (get-adjusted-subsidy-amount (subsidy-type (string-ascii 20)))
    (let ((subsidy-data (unwrap! (map-get? subsidy-types subsidy-type) err-not-eligible)))
        (ok (apply-economic-adjustment (get amount subsidy-data)))
    )
)

(define-public (enhanced-distribute-subsidy-with-adjustments (beneficiary principal))
    (let (
            (beneficiary-data (unwrap! (map-get? beneficiaries beneficiary) err-not-registered))
            (subsidy-type-data (unwrap! (map-get? subsidy-types (get subsidy-type beneficiary-data))
                err-not-eligible
            ))
            (adjusted-amount (apply-economic-adjustment (get amount subsidy-type-data)))
            (current-time stacks-block-height)
        )
        (try! (verify-eligibility-for-distribution beneficiary))
        (try! (pre-distribution-checks beneficiary adjusted-amount))
        (asserts! (get eligible beneficiary-data) err-not-eligible)
        (asserts! (get active subsidy-type-data) err-not-eligible)
        (asserts!
            (>= (- current-time (get last-distribution beneficiary-data))
                (get period subsidy-type-data)
            )
            err-not-eligible
        )
        (asserts! (>= (var-get treasury-balance) adjusted-amount)
            err-insufficient-balance
        )
        (map-set beneficiaries beneficiary
            (merge beneficiary-data {
                received-amount: (+ (get received-amount beneficiary-data) adjusted-amount),
                last-distribution: current-time,
            })
        )
        (var-set treasury-balance (- (var-get treasury-balance) adjusted-amount))
        (var-set total-distributed
            (+ (var-get total-distributed) adjusted-amount)
        )
        (ok adjusted-amount)
    )
)

(define-read-only (get-economic-indicator (indicator-name (string-ascii 20)))
    (map-get? economic-indicators indicator-name)
)

(define-read-only (get-adjustment-history (adjustment-id uint))
    (map-get? adjustment-history adjustment-id)
)

(define-read-only (get-current-adjustment-factor)
    (calculate-adjustment-factor)
)

(define-read-only (get-economic-status)
    (ok {
        adjustments-enabled: (var-get economic-adjustment-enabled),
        base-factor: (var-get base-adjustment-factor),
        total-adjustments: (var-get indicator-counter),
    })
)
