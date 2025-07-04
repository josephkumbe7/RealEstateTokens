
;; title: RealEstateTokens


;; RealEstateTokens
;; A property investment platform enabling fractional ownership, rental income distribution, and property management

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROPERTY-NOT-FOUND (err u101))
(define-constant ERR-INSUFFICIENT-FUNDS (err u102))
(define-constant ERR-INSUFFICIENT-SHARES (err u103))
(define-constant ERR-ALREADY-EXISTS (err u104))


(define-constant ERR-PAYMENT-FAILED (err u105))
(define-constant ERR-PAYMENT-ALREADY-MADE (err u106))
(define-constant ERR-INVALID-PAYMENT-AMOUNT (err u107))

(define-map rental-payment-schedule
  { property-id: uint }
  {
    tenant: principal,
    monthly-rent: uint,
    due-day: uint,
    next-payment-due: uint,
    auto-pay-enabled: bool
  }
)

(define-map payment-history
  { property-id: uint, payment-month: uint }
  {
    amount-paid: uint,
    payment-date: uint,
    late-fee: uint,
    paid-by: principal
  }
)


;; Data structures

;; Property details
(define-map properties
  { id: uint }
  {
    name: (string-ascii 100),
    location: (string-ascii 100),
    owner: principal,
    total-shares: uint,
    available-shares: uint,
    price-per-share: uint,
    rental-income: uint,
    maintenance-cost: uint
  }
)

(define-map property-analytics
  { property-id: uint }
  {
    total-rental-collected: uint,
    total-maintenance-spent: uint,
    total-investment: uint,
    occupancy-days: uint,
    vacancy-days: uint,
    last-updated: uint
  }
)

(define-map monthly-performance
  { property-id: uint, month: uint }
  {
    rental-income: uint,
    maintenance-costs: uint,
    occupancy-rate: uint,
    net-income: uint
  }
)

;; Track ownership of shares
(define-map share-ownership
  { property-id: uint, owner: principal }
  { shares: uint }
)

;; Track property ID counter
(define-data-var next-property-id uint u1)

;; Functions

;; Create a new property listing
(define-public (list-property 
    (name (string-ascii 100)) 
    (location (string-ascii 100)) 
    (total-shares uint) 
    (price-per-share uint)
  )
  (let ((property-id (var-get next-property-id)))
    (begin
      (map-set properties 
        { id: property-id }
        {
          name: name,
          location: location,
          owner: tx-sender,
          total-shares: total-shares,
          available-shares: total-shares,
          price-per-share: price-per-share,
          rental-income: u0,
          maintenance-cost: u0
        }
      )
      (var-set next-property-id (+ property-id u1))
      (ok property-id)
    )
  )
)

;; Buy shares in a property
(define-public (buy-shares (property-id uint) (share-count uint))
  (let (
    (property (unwrap! (map-get? properties { id: property-id }) ERR-PROPERTY-NOT-FOUND))
    (current-shares (default-to { shares: u0 } (map-get? share-ownership { property-id: property-id, owner: tx-sender })))
    (total-cost (* share-count (get price-per-share property)))
  )
    (begin
      ;; Check if enough shares are available
      (asserts! (<= share-count (get available-shares property)) ERR-INSUFFICIENT-SHARES)
      
      ;; Transfer STX from buyer to property owner
      (try! (stx-transfer? total-cost tx-sender (get owner property)))
      
      ;; Update share ownership
      (map-set share-ownership 
        { property-id: property-id, owner: tx-sender }
        { shares: (+ (get shares current-shares) share-count) }
      )
      
      ;; Update available shares
      (map-set properties
        { id: property-id }
        (merge property { available-shares: (- (get available-shares property) share-count) })
      )
      
      (ok true)
    )
  )
)

;; Distribute rental income to shareholders
(define-public (distribute-rental-income (property-id uint) (amount uint))
  (let (
    (property (unwrap! (map-get? properties { id: property-id }) ERR-PROPERTY-NOT-FOUND))
  )
    (begin
      ;; Only property owner can distribute rental income
      (asserts! (is-eq tx-sender (get owner property)) ERR-NOT-AUTHORIZED)
      
      ;; Update rental income
      (map-set properties
        { id: property-id }
        (merge property { rental-income: (+ (get rental-income property) amount) })
      )
      
      (ok true)
    )
  )
)

;; Claim rental income as a shareholder
(define-public (claim-rental-income (property-id uint))
  (let (
    (property (unwrap! (map-get? properties { id: property-id }) ERR-PROPERTY-NOT-FOUND))
    (shares-owned (unwrap! (map-get? share-ownership { property-id: property-id, owner: tx-sender }) ERR-NOT-AUTHORIZED))
    (total-shares (get total-shares property))
    (rental-income (get rental-income property))
    (share-of-income (/ (* rental-income (get shares shares-owned)) total-shares))
  )
    (begin
      ;; Must own shares to claim income
      (asserts! (> (get shares shares-owned) u0) ERR-NOT-AUTHORIZED)
      
      ;; Transfer income to shareholder
      (try! (as-contract (stx-transfer? share-of-income contract-caller tx-sender)))
      
      ;; Update rental income
      (map-set properties
        { id: property-id }
        (merge property { rental-income: (- rental-income share-of-income) })
      )
      
      (ok share-of-income)
    )
  )
)

;; Record maintenance costs
(define-public (record-maintenance-cost (property-id uint) (cost uint))
  (let (
    (property (unwrap! (map-get? properties { id: property-id }) ERR-PROPERTY-NOT-FOUND))
  )
    (begin
      ;; Only property owner can record maintenance costs
      (asserts! (is-eq tx-sender (get owner property)) ERR-NOT-AUTHORIZED)
      
      ;; Update maintenance costs
      (map-set properties
        { id: property-id }
        (merge property { maintenance-cost: (+ (get maintenance-cost property) cost) })
      )
      
      (ok true)
    )
  )
)

;; Read-only functions

;; Get property details
(define-read-only (get-property (property-id uint))
  (map-get? properties { id: property-id })
)

;; Get share ownership
(define-read-only (get-shares (property-id uint) (owner principal))
  (default-to { shares: u0 } (map-get? share-ownership { property-id: property-id, owner: owner }))
)

;; Calculate expected rental income for a shareholder
(define-read-only (calculate-income (property-id uint) (owner principal))
  (let (
    (property (unwrap-panic (map-get? properties { id: property-id })))
    (shares-owned (get shares (default-to { shares: u0 } (map-get? share-ownership { property-id: property-id, owner: owner }))))
    (total-shares (get total-shares property))
    (rental-income (get rental-income property))
  )
    (/ (* rental-income shares-owned) total-shares)
  )
)

(define-public (sell-shares (property-id uint) (share-count uint) (price-per-share uint))
    (let (
        (property (unwrap! (map-get? properties { id: property-id }) ERR-PROPERTY-NOT-FOUND))
        (seller-shares (unwrap! (map-get? share-ownership { property-id: property-id, owner: tx-sender }) ERR-INSUFFICIENT-SHARES))
    )
        (begin
            (asserts! (>= (get shares seller-shares) share-count) ERR-INSUFFICIENT-SHARES)
            (map-set share-ownership 
                { property-id: property-id, owner: tx-sender }
                { shares: (- (get shares seller-shares) share-count) }
            )
            (map-set properties
                { id: property-id }
                (merge property { 
                    available-shares: (+ (get available-shares property) share-count),
                    price-per-share: price-per-share 
                })
            )
            (ok true)
        )
    )
)


(define-map property-ratings
    { property-id: uint, rater: principal }
    { rating: uint }
)

(define-public (rate-property (property-id uint) (rating uint))
    (begin
        (asserts! (<= rating u5) (err u300))
        (map-set property-ratings
            { property-id: property-id, rater: tx-sender }
            { rating: rating }
        )
        (ok true)
    )
)

(define-read-only (get-property-rating (property-id uint))
    (default-to { rating: u0 } 
        (map-get? property-ratings { property-id: property-id, rater: tx-sender })
    )
)


(define-map maintenance-requests
    { request-id: uint }
    {
        property-id: uint,
        requester: principal,
        description: (string-ascii 200),
        status: (string-ascii 20)
    }
)

(define-data-var next-request-id uint u1)

(define-public (submit-maintenance-request (property-id uint) (description (string-ascii 200)))
    (let (
        (request-id (var-get next-request-id))
    )
        (begin
            (map-set maintenance-requests
                { request-id: request-id }
                {
                    property-id: property-id,
                    requester: tx-sender,
                    description: description,
                    status: "pending"
                }
            )
            (var-set next-request-id (+ request-id u1))
            (ok request-id)
        )
    )
)


(define-map property-insurance
    { property-id: uint }
    {
        insurer: principal,
        coverage-amount: uint,
        premium: uint,
        expiry: uint
    }
)

(define-public (add-insurance (property-id uint) (coverage uint) (premium uint) (duration uint))
    (let (
        (property (unwrap! (map-get? properties { id: property-id }) ERR-PROPERTY-NOT-FOUND))
        (expiry (+ stacks-block-height duration))
    )
        (begin
            (asserts! (is-eq tx-sender (get owner property)) ERR-NOT-AUTHORIZED)
            (map-set property-insurance
                { property-id: property-id }
                {
                    insurer: tx-sender,
                    coverage-amount: coverage,
                    premium: premium,
                    expiry: expiry
                }
            )
            (ok true)
        )
    )
)


(define-map property-valuations
    { property-id: uint, valuation-date: uint }
    { value: uint }
)

(define-public (record-property-valuation (property-id uint) (value uint))
    (let (
        (property (unwrap! (map-get? properties { id: property-id }) ERR-PROPERTY-NOT-FOUND))
    )
        (begin
            (asserts! (is-eq tx-sender (get owner property)) ERR-NOT-AUTHORIZED)
            (map-set property-valuations
                { property-id: property-id, valuation-date: stacks-block-height }
                { value: value }
            )
            (ok true)
        )
    )
)


(define-map property-votes
    { vote-id: uint }
    {
        property-id: uint,
        proposal: (string-ascii 200),
        votes-for: uint,
        votes-against: uint,
        end-block: uint
    }
)

(define-data-var next-vote-id uint u1)

(define-public (create-vote (property-id uint) (proposal (string-ascii 200)) (duration uint))
    (let (
        (vote-id (var-get next-vote-id))
    )
        (begin
            (map-set property-votes
                { vote-id: vote-id }
                {
                    property-id: property-id,
                    proposal: proposal,
                    votes-for: u0,
                    votes-against: u0,
                    end-block: (+ stacks-block-height duration)
                }
            )
            (var-set next-vote-id (+ vote-id u1))
            (ok vote-id)
        )
    )
)


(define-map property-occupancy
    { property-id: uint }
    {
        tenant: principal,
        lease-start: uint,
        lease-end: uint,
        monthly-rent: uint
    }
)

(define-public (register-tenant (property-id uint) (tenant principal) (duration uint) (rent uint))
    (let (
        (property (unwrap! (map-get? properties { id: property-id }) ERR-PROPERTY-NOT-FOUND))
    )
        (begin
            (asserts! (is-eq tx-sender (get owner property)) ERR-NOT-AUTHORIZED)
            (map-set property-occupancy
                { property-id: property-id }
                {
                    tenant: tenant,
                    lease-start: stacks-block-height,
                    lease-end: (+ stacks-block-height duration),
                    monthly-rent: rent
                }
            )
            (ok true)
        )
    )
)



(define-map maintenance-funds
    { property-id: uint }
    {
        balance: uint,
        monthly-contribution: uint,
        last-collection: uint
    }
)

(define-public (setup-maintenance-fund (property-id uint) (monthly-amount uint))
    (let (
        (property (unwrap! (map-get? properties { id: property-id }) ERR-PROPERTY-NOT-FOUND))
    )
        (begin
            (asserts! (is-eq tx-sender (get owner property)) ERR-NOT-AUTHORIZED)
            (map-set maintenance-funds
                { property-id: property-id }
                {
                    balance: u0,
                    monthly-contribution: monthly-amount,
                    last-collection: stacks-block-height
                }
            )
            (ok true)
        )
    )
)

(define-public (contribute-maintenance-fund (property-id uint))
    (let (
        (fund (unwrap! (map-get? maintenance-funds { property-id: property-id }) ERR-PROPERTY-NOT-FOUND))
        (shares (unwrap! (map-get? share-ownership { property-id: property-id, owner: tx-sender }) ERR-NOT-AUTHORIZED))
        (property (unwrap! (map-get? properties { id: property-id }) ERR-PROPERTY-NOT-FOUND))
        (share-contribution (/ (* (get monthly-contribution fund) (get shares shares)) (get total-shares property)))
    )
        (begin
            (try! (stx-transfer? share-contribution tx-sender (as-contract tx-sender)))
            (map-set maintenance-funds
                { property-id: property-id }
                (merge fund { balance: (+ (get balance fund) share-contribution) })
            )
            (ok true)
        )
    )
)



(define-public (withdraw-maintenance-fund (property-id uint))
    (let (
        (fund (unwrap! (map-get? maintenance-funds { property-id: property-id }) ERR-PROPERTY-NOT-FOUND))
        (property (unwrap! (map-get? properties { id: property-id }) ERR-PROPERTY-NOT-FOUND))
    )
        (begin
            (asserts! (is-eq tx-sender (get owner property)) ERR-NOT-AUTHORIZED)
            (try! (stx-transfer? (get balance fund) tx-sender tx-sender))
            (map-set maintenance-funds
                { property-id: property-id }
                { balance: u0, monthly-contribution: u0, last-collection: stacks-block-height }
            )
            (ok true)
        )
    )
)


(define-map share-listings
    { listing-id: uint }
    {
        property-id: uint,
        seller: principal,
        share-count: uint,
        price-per-share: uint,
        active: bool
    }
)

(define-data-var next-listing-id uint u1)

(define-public (create-share-listing (property-id uint) (share-count uint) (price-per-share uint))
    (let (
        (listing-id (var-get next-listing-id))
        (seller-shares (unwrap! (map-get? share-ownership { property-id: property-id, owner: tx-sender }) ERR-INSUFFICIENT-SHARES))
    )
        (begin
            (asserts! (>= (get shares seller-shares) share-count) ERR-INSUFFICIENT-SHARES)
            (map-set share-listings
                { listing-id: listing-id }
                {
                    property-id: property-id,
                    seller: tx-sender,
                    share-count: share-count,
                    price-per-share: price-per-share,
                    active: true
                }
            )
            (var-set next-listing-id (+ listing-id u1))
            (ok listing-id)
        )
    )
)

(define-public (transfer-shares (property-id uint) (from principal) (to principal) (amount uint))
    (let (
        (from-shares (unwrap! (map-get? share-ownership { property-id: property-id, owner: from }) ERR-INSUFFICIENT-SHARES))
        (to-shares (default-to { shares: u0 } (map-get? share-ownership { property-id: property-id, owner: to })))
    )
        (begin
            (asserts! (>= (get shares from-shares) amount) ERR-INSUFFICIENT-SHARES)
            (map-set share-ownership
                { property-id: property-id, owner: from }
                { shares: (- (get shares from-shares) amount) }
            )
            (map-set share-ownership
                { property-id: property-id, owner: to }
                { shares: (+ (get shares to-shares) amount) }
            )
            (ok true)
        )
    )
)

(define-public (purchase-listed-shares (listing-id uint))
    (let (
        (listing (unwrap! (map-get? share-listings { listing-id: listing-id }) (err u200)))
        (total-cost (* (get share-count listing) (get price-per-share listing)))
    )
        (begin
            (asserts! (get active listing) (err u200)))
            (try! (stx-transfer? total-cost tx-sender (get seller listing)))
            (try! (transfer-shares (get property-id listing) (get seller listing) tx-sender (get share-count listing)))
            (map-set share-listings
                { listing-id: listing-id }
                (merge listing { active: false })
            )
            (ok true)
        )
    )




(define-public (initialize-analytics (property-id uint))
  (let (
    (property (unwrap! (map-get? properties { id: property-id }) ERR-PROPERTY-NOT-FOUND))
  )
    (begin
      (asserts! (is-eq tx-sender (get owner property)) ERR-NOT-AUTHORIZED)
      (map-set property-analytics
        { property-id: property-id }
        {
          total-rental-collected: u0,
          total-maintenance-spent: u0,
          total-investment: (* (get total-shares property) (get price-per-share property)),
          occupancy-days: u0,
          vacancy-days: u0,
          last-updated: stacks-block-height
        }
      )
      (ok true)
    )
  )
)

(define-public (update-monthly-performance (property-id uint) (month uint) (rental uint) (maintenance uint) (occupancy-rate uint))
  (let (
    (property (unwrap! (map-get? properties { id: property-id }) ERR-PROPERTY-NOT-FOUND))
    (net-income (- rental maintenance))
  )
    (begin
      (asserts! (is-eq tx-sender (get owner property)) ERR-NOT-AUTHORIZED)
      (asserts! (<= occupancy-rate u100) (err u301))
      (map-set monthly-performance
        { property-id: property-id, month: month }
        {
          rental-income: rental,
          maintenance-costs: maintenance,
          occupancy-rate: occupancy-rate,
          net-income: net-income
        }
      )
      (ok true)
    )
  )
)

(define-public (record-occupancy-change (property-id uint) (is-occupied bool))
  (let (
    (property (unwrap! (map-get? properties { id: property-id }) ERR-PROPERTY-NOT-FOUND))
    (analytics (unwrap! (map-get? property-analytics { property-id: property-id }) ERR-PROPERTY-NOT-FOUND))
    (days-since-update (- stacks-block-height (get last-updated analytics)))
  )
    (begin
      (asserts! (is-eq tx-sender (get owner property)) ERR-NOT-AUTHORIZED)
      (if is-occupied
        (map-set property-analytics
          { property-id: property-id }
          (merge analytics {
            occupancy-days: (+ (get occupancy-days analytics) days-since-update),
            last-updated: stacks-block-height
          })
        )
        (map-set property-analytics
          { property-id: property-id }
          (merge analytics {
            vacancy-days: (+ (get vacancy-days analytics) days-since-update),
            last-updated: stacks-block-height
          })
        )
      )
      (ok true)
    )
  )
)

(define-read-only (get-property-analytics (property-id uint))
  (map-get? property-analytics { property-id: property-id })
)

(define-read-only (get-monthly-performance (property-id uint) (month uint))
  (map-get? monthly-performance { property-id: property-id, month: month })
)

(define-read-only (calculate-roi (property-id uint))
  (let (
    (analytics (unwrap-panic (map-get? property-analytics { property-id: property-id })))
    (total-investment (get total-investment analytics))
    (net-profit (- (get total-rental-collected analytics) (get total-maintenance-spent analytics)))
  )
    (if (> total-investment u0)
      (/ (* net-profit u10000) total-investment)
      u0
    )
  )
)

(define-read-only (calculate-occupancy-rate (property-id uint))
  (let (
    (analytics (unwrap-panic (map-get? property-analytics { property-id: property-id })))
    (total-days (+ (get occupancy-days analytics) (get vacancy-days analytics)))
  )
    (if (> total-days u0)
      (/ (* (get occupancy-days analytics) u100) total-days)
      u0
    )
  )
)

(define-read-only (calculate-rental-yield (property-id uint))
  (let (
    (analytics (unwrap-panic (map-get? property-analytics { property-id: property-id })))
    (annual-rental (get total-rental-collected analytics))
    (property-value (get total-investment analytics))
  )
    (if (> property-value u0)
      (/ (* annual-rental u10000) property-value)
      u0
    )
  )
)

(define-read-only (get-performance-summary (property-id uint))
  (let (
    (analytics (unwrap-panic (map-get? property-analytics { property-id: property-id })))
  )
    {
      roi: (calculate-roi property-id),
      occupancy-rate: (calculate-occupancy-rate property-id),
      rental-yield: (calculate-rental-yield property-id),
      net-income: (- (get total-rental-collected analytics) (get total-maintenance-spent analytics)),
      total-investment: (get total-investment analytics)
    }
  )
)

(define-public (setup-rental-payments (property-id uint) (tenant principal) (monthly-rent uint) (due-day uint))
  (let (
    (property (unwrap! (map-get? properties { id: property-id }) ERR-PROPERTY-NOT-FOUND))
    (current-month (/ stacks-block-height u144))
    (next-due-block (+ (* current-month u144) (* due-day u6)))
  )
    (begin
      (asserts! (is-eq tx-sender (get owner property)) ERR-NOT-AUTHORIZED)
      (asserts! (<= due-day u30) (err u400))
      (map-set rental-payment-schedule
        { property-id: property-id }
        {
          tenant: tenant,
          monthly-rent: monthly-rent,
          due-day: due-day,
          next-payment-due: next-due-block,
          auto-pay-enabled: false
        }
      )
      (ok true)
    )
  )
)

(define-public (enable-auto-pay (property-id uint))
  (let (
    (schedule (unwrap! (map-get? rental-payment-schedule { property-id: property-id }) ERR-PROPERTY-NOT-FOUND))
  )
    (begin
      (asserts! (is-eq tx-sender (get tenant schedule)) ERR-NOT-AUTHORIZED)
      (map-set rental-payment-schedule
        { property-id: property-id }
        (merge schedule { auto-pay-enabled: true })
      )
      (ok true)
    )
  )
)

(define-public (pay-rent (property-id uint))
  (let (
    (schedule (unwrap! (map-get? rental-payment-schedule { property-id: property-id }) ERR-PROPERTY-NOT-FOUND))
    (property (unwrap! (map-get? properties { id: property-id }) ERR-PROPERTY-NOT-FOUND))
    (current-month (/ stacks-block-height u144))
    (existing-payment (map-get? payment-history { property-id: property-id, payment-month: current-month }))
    (is-late (> stacks-block-height (get next-payment-due schedule)))
    (late-fee (if is-late (/ (get monthly-rent schedule) u20) u0))
    (total-amount (+ (get monthly-rent schedule) late-fee))
  )
    (begin
      (asserts! (is-eq tx-sender (get tenant schedule)) ERR-NOT-AUTHORIZED)
      (asserts! (is-none existing-payment) ERR-PAYMENT-ALREADY-MADE)
      
      (try! (stx-transfer? total-amount tx-sender (get owner property)))
      
      (map-set payment-history
        { property-id: property-id, payment-month: current-month }
        {
          amount-paid: total-amount,
          payment-date: stacks-block-height,
          late-fee: late-fee,
          paid-by: tx-sender
        }
      )
      
      (map-set rental-payment-schedule
        { property-id: property-id }
        (merge schedule { 
          next-payment-due: (+ (get next-payment-due schedule) u4320)
        })
      )
      
      (ok total-amount)
    )
  )
)

(define-public (process-auto-payment (property-id uint))
  (let (
    (schedule (unwrap! (map-get? rental-payment-schedule { property-id: property-id }) ERR-PROPERTY-NOT-FOUND))
    (current-month (/ stacks-block-height u144))
    (existing-payment (map-get? payment-history { property-id: property-id, payment-month: current-month }))
  )
    (begin
      (asserts! (get auto-pay-enabled schedule) ERR-NOT-AUTHORIZED)
      (asserts! (>= stacks-block-height (get next-payment-due schedule)) ERR-PAYMENT-FAILED)
      (asserts! (is-none existing-payment) ERR-PAYMENT-ALREADY-MADE)
      
      (as-contract (pay-rent property-id))
    )
  )
)

(define-read-only (get-rental-schedule (property-id uint))
  (map-get? rental-payment-schedule { property-id: property-id })
)

(define-read-only (get-payment-history (property-id uint) (month uint))
  (map-get? payment-history { property-id: property-id, payment-month: month })
)

(define-read-only (calculate-payment-status (property-id uint))
  (let (
    (schedule (unwrap-panic (map-get? rental-payment-schedule { property-id: property-id })))
    (current-month (/ stacks-block-height u144))
    (payment-record (map-get? payment-history { property-id: property-id, payment-month: current-month }))
    (is-overdue (> stacks-block-height (get next-payment-due schedule)))
  )
    {
      payment-made: (is-some payment-record),
      is-overdue: is-overdue,
      days-late: (if is-overdue (/ (- stacks-block-height (get next-payment-due schedule)) u144) u0),
      next-due: (get next-payment-due schedule)
    }
  )
)