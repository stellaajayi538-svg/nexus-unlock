;; NexusUnlock - Decentralized Microfinance Contract

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-already-exists (err u104))
(define-constant err-invalid-amount (err u105))
(define-constant err-milestone-not-ready (err u106))

;; Data Variables
(define-data-var lending-pool-balance uint u0)
(define-data-var total-loans-issued uint u0)

;; Data Maps
(define-map borrowers 
    principal 
    {
        social-capital-score: uint,
        total-borrowed: uint,
        total-repaid: uint,
        active-loan: bool,
        endorsements: uint
    }
)

(define-map loans
    uint ;; loan-id
    {
        borrower: principal,
        amount: uint,
        interest-rate: uint, ;; basis points (e.g., 500 = 5%)
        milestones-total: uint,
        milestones-completed: uint,
        amount-unlocked: uint,
        amount-repaid: uint,
        status: (string-ascii 20), ;; "active", "completed", "defaulted"
        created-at: uint
    }
)

(define-map endorsements
    {endorser: principal, borrower: principal}
    {
        stake-amount: uint,
        active: bool
    }
)

(define-map milestones
    {loan-id: uint, milestone-id: uint}
    {
        description: (string-ascii 100),
        unlock-amount: uint,
        completed: bool,
        verified-by: (optional principal)
    }
)

;; Read-only functions
(define-read-only (get-borrower-info (borrower principal))
    (ok (default-to 
        {
            social-capital-score: u0,
            total-borrowed: u0,
            total-repaid: u0,
            active-loan: false,
            endorsements: u0
        }
        (map-get? borrowers borrower)
    ))
)

(define-read-only (get-loan-info (loan-id uint))
    (ok (map-get? loans loan-id))
)

(define-read-only (get-lending-pool-balance)
    (ok (var-get lending-pool-balance))
)

(define-read-only (get-endorsement (endorser principal) (borrower principal))
    (ok (map-get? endorsements {endorser: endorser, borrower: borrower}))
)

(define-read-only (get-milestone (loan-id uint) (milestone-id uint))
    (ok (map-get? milestones {loan-id: loan-id, milestone-id: milestone-id}))
)

;; Public functions

;; Register as a borrower
(define-public (register-borrower)
    (let ((existing (map-get? borrowers tx-sender)))
        (if (is-some existing)
            err-already-exists
            (ok (map-set borrowers tx-sender {
                social-capital-score: u100,
                total-borrowed: u0,
                total-repaid: u0,
                active-loan: false,
                endorsements: u0
            }))
        )
    )
)

;; Deposit funds to lending pool
(define-public (deposit-to-pool (amount uint))
    (begin
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (var-set lending-pool-balance (+ (var-get lending-pool-balance) amount))
        (ok true)
    )
)

;; Endorse a borrower with stake
(define-public (endorse-borrower (borrower principal) (stake-amount uint))
    (let (
        (borrower-data (unwrap! (map-get? borrowers borrower) err-not-found))
        (existing-endorsement (map-get? endorsements {endorser: tx-sender, borrower: borrower}))
    )
        (asserts! (is-none existing-endorsement) err-already-exists)
        (asserts! (> stake-amount u0) err-invalid-amount)
        
        ;; Transfer stake to contract
        (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
        
        ;; Record endorsement
        (map-set endorsements 
            {endorser: tx-sender, borrower: borrower}
            {stake-amount: stake-amount, active: true}
        )
        
        ;; Update borrower's endorsement count and social capital score
        (map-set borrowers borrower (merge borrower-data {
            endorsements: (+ (get endorsements borrower-data) u1),
            social-capital-score: (+ (get social-capital-score borrower-data) u50)
        }))
        
        (ok true)
    )
)

;; Request a loan
(define-public (request-loan (amount uint) (interest-rate uint) (num-milestones uint))
    (let (
        (borrower-data (unwrap! (map-get? borrowers tx-sender) err-not-found))
        (loan-id (+ (var-get total-loans-issued) u1))
        (pool-balance (var-get lending-pool-balance))
    )
        (asserts! (not (get active-loan borrower-data)) err-already-exists)
        (asserts! (>= pool-balance amount) err-insufficient-funds)
        (asserts! (>= (get social-capital-score borrower-data) u100) err-unauthorized)
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (> num-milestones u0) err-invalid-amount)
        
        ;; Create loan
        (map-set loans loan-id {
            borrower: tx-sender,
            amount: amount,
            interest-rate: interest-rate,
            milestones-total: num-milestones,
            milestones-completed: u0,
            amount-unlocked: u0,
            amount-repaid: u0,
            status: "active",
            created-at: block-height
        })
        
        ;; Update borrower status
        (map-set borrowers tx-sender (merge borrower-data {
            active-loan: true,
            total-borrowed: (+ (get total-borrowed borrower-data) amount)
        }))
        
        ;; Update counters
        (var-set total-loans-issued loan-id)
        
        (ok loan-id)
    )
)

;; Add milestone to loan
(define-public (add-milestone (loan-id uint) (milestone-id uint) (description (string-ascii 100)) (unlock-amount uint))
    (let (
        (loan-data (unwrap! (map-get? loans loan-id) err-not-found))
    )
        (asserts! (is-eq (get borrower loan-data) tx-sender) err-unauthorized)
        (asserts! (is-eq (get status loan-data) "active") err-unauthorized)
        
        (ok (map-set milestones 
            {loan-id: loan-id, milestone-id: milestone-id}
            {
                description: description,
                unlock-amount: unlock-amount,
                completed: false,
                verified-by: none
            }
        ))
    )
)

;; Verify milestone completion (validator function)
(define-public (verify-milestone (loan-id uint) (milestone-id uint))
    (let (
        (loan-data (unwrap! (map-get? loans loan-id) err-not-found))
        (milestone-data (unwrap! (map-get? milestones {loan-id: loan-id, milestone-id: milestone-id}) err-not-found))
        (unlock-amount (get unlock-amount milestone-data))
    )
        (asserts! (not (get completed milestone-data)) err-already-exists)
        
        ;; Update milestone
        (map-set milestones 
            {loan-id: loan-id, milestone-id: milestone-id}
            (merge milestone-data {
                completed: true,
                verified-by: (some tx-sender)
            })
        )
        
        ;; Unlock funds to borrower
        (try! (as-contract (stx-transfer? unlock-amount tx-sender (get borrower loan-data))))
        
        ;; Update loan
        (map-set loans loan-id (merge loan-data {
            milestones-completed: (+ (get milestones-completed loan-data) u1),
            amount-unlocked: (+ (get amount-unlocked loan-data) unlock-amount)
        }))
        
        ;; Update pool balance
        (var-set lending-pool-balance (- (var-get lending-pool-balance) unlock-amount))
        
        (ok true)
    )
)

;; Repay loan
(define-public (repay-loan (loan-id uint) (amount uint))
    (let (
        (loan-data (unwrap! (map-get? loans loan-id) err-not-found))
        (borrower-data (unwrap! (map-get? borrowers tx-sender) err-not-found))
    )
        (asserts! (is-eq (get borrower loan-data) tx-sender) err-unauthorized)
        (asserts! (is-eq (get status loan-data) "active") err-unauthorized)
        (asserts! (> amount u0) err-invalid-amount)
        
        ;; Transfer repayment to contract
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        ;; Update loan
        (let ((new-repaid (+ (get amount-repaid loan-data) amount)))
            (map-set loans loan-id (merge loan-data {
                amount-repaid: new-repaid,
                status: (if (>= new-repaid (get amount loan-data)) "completed" "active")
            }))
            
            ;; Update borrower
            (map-set borrowers tx-sender (merge borrower-data {
                total-repaid: (+ (get total-repaid borrower-data) amount),
                active-loan: (< new-repaid (get amount loan-data)),
                social-capital-score: (+ (get social-capital-score borrower-data) u10)
            }))
            
            ;; Add to pool
            (var-set lending-pool-balance (+ (var-get lending-pool-balance) amount))
        )
        
        (ok true)
    )
)