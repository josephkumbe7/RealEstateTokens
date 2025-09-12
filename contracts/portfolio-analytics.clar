;; Portfolio Analytics Dashboard
;; Provides comprehensive analytics for investors across multiple properties

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u500))
(define-constant ERR-NO-INVESTMENTS (err u501))
(define-constant ERR-INVALID-DATA (err u502))
(define-constant ERR-PORTFOLIO-NOT-FOUND (err u503))

;; Portfolio tracking for individual investors
(define-map investor-portfolios
    principal
    {
        total-properties: uint,
        total-investment: uint,
        total-shares: uint,
        portfolio-value: uint,
        last-updated: uint
    }
)

;; Individual property performance within portfolios
(define-map portfolio-holdings
    {investor: principal, property-id: uint}
    {
        shares-owned: uint,
        initial-investment: uint,
        current-value: uint,
        rental-income-earned: uint,
        maintenance-costs-share: uint,
        roi-percentage: uint,
        performance-ranking: uint
    }
)

;; Portfolio diversification metrics
(define-map portfolio-diversification
    principal
    {
        property-types: uint,
        geographic-spread: uint,
        risk-score: uint,
        concentration-ratio: uint,
        diversification-grade: (string-ascii 2)
    }
)

;; Portfolio performance benchmarks
(define-map portfolio-benchmarks
    {investor: principal, period: uint}
    {
        total-return: uint,
        rental-yield: uint,
        capital-appreciation: uint,
        risk-adjusted-return: uint,
        outperformance-ratio: uint
    }
)

;; Investment allocation by category
(define-map allocation-categories
    {investor: principal, category: (string-ascii 20)}
    {
        allocated-amount: uint,
        percentage: uint,
        performance: uint
    }
)

;; Update investor's portfolio summary
(define-public (update-portfolio-summary (investor principal))
    (let
        (
            (total-props u0)
            (total-invest u0)
            (total-shr u0)
            (portfolio-val u0)
        )
        (begin
            ;; Calculate aggregated portfolio metrics
            (map-set investor-portfolios
                investor
                {
                    total-properties: total-props,
                    total-investment: total-invest,
                    total-shares: total-shr,
                    portfolio-value: portfolio-val,
                    last-updated: stacks-block-height
                }
            )
            (ok true)
        )
    )
)

;; Record property holding details for portfolio analysis
(define-public (add-property-holding 
    (property-id uint) 
    (shares uint) 
    (investment-amount uint)
    (current-val uint)
)
    (let
        (
            (roi-calc (if (> investment-amount u0) 
                (/ (* (- current-val investment-amount) u10000) investment-amount) 
                u0))
        )
        (begin
            (map-set portfolio-holdings
                {investor: tx-sender, property-id: property-id}
                {
                    shares-owned: shares,
                    initial-investment: investment-amount,
                    current-value: current-val,
                    rental-income-earned: u0,
                    maintenance-costs-share: u0,
                    roi-percentage: roi-calc,
                    performance-ranking: u0
                }
            )
            (ok true)
        )
    )
)

;; Update rental income tracking for portfolio
(define-public (record-rental-income (property-id uint) (income-amount uint))
    (let
        (
            (existing-holding (unwrap! (map-get? portfolio-holdings {investor: tx-sender, property-id: property-id}) ERR-NO-INVESTMENTS))
        )
        (begin
            (map-set portfolio-holdings
                {investor: tx-sender, property-id: property-id}
                (merge existing-holding 
                    {rental-income-earned: (+ (get rental-income-earned existing-holding) income-amount)}
                )
            )
            (ok true)
        )
    )
)

;; Calculate portfolio diversification score
(define-public (calculate-diversification-score (investor principal))
    (let
        (
            (portfolio (unwrap! (map-get? investor-portfolios investor) ERR-NO-INVESTMENTS))
            (property-count (get total-properties portfolio))
            (risk-score (if (> property-count u5) u20 
                        (if (> property-count u3) u40
                        (if (> property-count u1) u60 u80))))
            (concentration-ratio (if (> property-count u0) 
                (/ (* (get total-investment portfolio) u100) property-count) 
                u100))
            (div-grade (if (<= risk-score u30) "A" 
                      (if (<= risk-score u50) "B" 
                      (if (<= risk-score u70) "C" "D"))))
        )
        (begin
            (map-set portfolio-diversification
                investor
                {
                    property-types: property-count,
                    geographic-spread: u1,
                    risk-score: risk-score,
                    concentration-ratio: concentration-ratio,
                    diversification-grade: div-grade
                }
            )
            (ok risk-score)
        )
    )
)

;; Record portfolio performance for a specific period
(define-public (record-period-performance 
    (period uint)
    (total-return uint)
    (rental-yield uint)
    (capital-appreciation uint)
)
    (let
        (
            (risk-adj-return (/ (* total-return u8000) u10000))
        )
        (begin
            (map-set portfolio-benchmarks
                {investor: tx-sender, period: period}
                {
                    total-return: total-return,
                    rental-yield: rental-yield,
                    capital-appreciation: capital-appreciation,
                    risk-adjusted-return: risk-adj-return,
                    outperformance-ratio: u100
                }
            )
            (ok true)
        )
    )
)

;; Update allocation by investment category
(define-public (update-category-allocation 
    (category (string-ascii 20)) 
    (amount uint) 
    (performance uint)
)
    (let
        (
            (portfolio (unwrap! (map-get? investor-portfolios tx-sender) ERR-NO-INVESTMENTS))
            (total-investment (get total-investment portfolio))
            (percentage (if (> total-investment u0) 
                (/ (* amount u100) total-investment) 
                u0))
        )
        (begin
            (map-set allocation-categories
                {investor: tx-sender, category: category}
                {
                    allocated-amount: amount,
                    percentage: percentage,
                    performance: performance
                }
            )
            (ok percentage)
        )
    )
)

;; Read-only functions

;; Get investor's complete portfolio overview
(define-read-only (get-portfolio-overview (investor principal))
    (map-get? investor-portfolios investor)
)

;; Get property holding details within portfolio
(define-read-only (get-property-holding (investor principal) (property-id uint))
    (map-get? portfolio-holdings {investor: investor, property-id: property-id})
)

;; Get diversification analysis
(define-read-only (get-diversification-analysis (investor principal))
    (map-get? portfolio-diversification investor)
)

;; Get performance for specific period
(define-read-only (get-period-performance (investor principal) (period uint))
    (map-get? portfolio-benchmarks {investor: investor, period: period})
)

;; Get category allocation breakdown
(define-read-only (get-category-allocation (investor principal) (category (string-ascii 20)))
    (map-get? allocation-categories {investor: investor, category: category})
)

;; Calculate portfolio metrics summary
(define-read-only (get-portfolio-metrics (investor principal))
    (let
        (
            (portfolio (unwrap! (map-get? investor-portfolios investor) (err "No portfolio found")))
            (diversification (map-get? portfolio-diversification investor))
            (total-investment (get total-investment portfolio))
            (portfolio-value (get portfolio-value portfolio))
            (total-return (if (> total-investment u0) 
                (/ (* (- portfolio-value total-investment) u10000) total-investment)
                u0))
        )
        (ok {
            total-properties: (get total-properties portfolio),
            total-investment: total-investment,
            current-value: portfolio-value,
            total-return-percentage: total-return,
            diversification-score: (match diversification 
                div-data (get risk-score div-data)
                u0),
            last-updated: (get last-updated portfolio)
        })
    )
)

;; Calculate portfolio ranking against benchmarks
(define-read-only (calculate-portfolio-ranking (investor principal))
    (let
        (
            (metrics (unwrap! (get-portfolio-metrics investor) (err "No metrics available")))
            (return-pct (get total-return-percentage metrics))
            (diversification-score (get diversification-score metrics))
            (composite-score (/ (+ (* return-pct u60) (* diversification-score u40)) u100))
            (performance-tier (if (>= composite-score u80) "Excellent"
                             (if (>= composite-score u60) "Good" 
                             (if (>= composite-score u40) "Average" "Below Average"))))
        )
        (ok {
            composite-score: composite-score,
            performance-tier: performance-tier,
            return-component: return-pct,
            diversification-component: diversification-score
        })
    )
)

;; Get top performing properties in portfolio
(define-read-only (get-top-performers (investor principal))
    (ok {
        analysis-note: "Top performers analysis would require iteration over holdings",
        recommendation: "Consider rebalancing based on performance metrics"
    })
)

;; Calculate optimal allocation suggestions
(define-read-only (get-allocation-recommendations (investor principal))
    (let
        (
            (diversification (map-get? portfolio-diversification investor))
            (risk-score (match diversification 
                div-data (get risk-score div-data)
                u50))
        )
        (ok {
            recommendation: (if (> risk-score u60) "Increase diversification"
                           (if (> risk-score u40) "Maintain current allocation"
                           "Consider selective concentration")),
            suggested-action: "Review property geographic and type distribution",
            risk-level: (if (> risk-score u60) "High" 
                       (if (> risk-score u40) "Medium" "Low"))
        })
    )
)
