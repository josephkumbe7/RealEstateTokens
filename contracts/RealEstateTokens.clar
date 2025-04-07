
;; title: RealEstateTokens


;; RealEstateTokens
;; A property investment platform enabling fractional ownership, rental income distribution, and property management

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROPERTY-NOT-FOUND (err u101))
(define-constant ERR-INSUFFICIENT-FUNDS (err u102))
(define-constant ERR-INSUFFICIENT-SHARES (err u103))
(define-constant ERR-ALREADY-EXISTS (err u104))

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

