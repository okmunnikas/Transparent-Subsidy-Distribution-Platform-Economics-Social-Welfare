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
