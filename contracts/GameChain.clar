;; GameChain: Decentralized Gaming Asset Marketplace
;; Version: 1.0.0
;; Trade and manage gaming assets with player ownership on-chain

;; Marketplace Statistics
(define-data-var total-assets uint u0)

;; Core Gaming Data
(define-map asset-registry
  { asset-id: uint }
  {
    name: (string-ascii 64),
    owner: principal,
    rarity: uint,
    creation-block: uint,
    description: (string-ascii 128),
    attributes: (list 10 (string-ascii 32))
  })

(define-map trading-permissions
  { asset-id: uint, trader: principal }
  { can-trade: bool })

;; Marketplace Error Codes
(define-constant asset-not-found (err u401))
(define-constant asset-already-exists (err u402))
(define-constant invalid-name (err u403))
(define-constant invalid-rarity (err u404))
(define-constant unauthorized-access (err u405))
(define-constant not-owner (err u406))
(define-constant admin-only (err u400))
(define-constant trading-restricted (err u407))
(define-constant invalid-attributes (err u408))

;; Marketplace Administrator
(define-constant marketplace-admin tx-sender)

;; ===== Gaming Helper Functions =====

;; Check if asset exists in marketplace
(define-private (asset-exists (asset-id uint))
  (is-some (map-get? asset-registry { asset-id: asset-id })))

;; Verify owner ownership
(define-private (is-owner (asset-id uint) (caller principal))
  (match (map-get? asset-registry { asset-id: asset-id })
    asset-data (is-eq (get owner asset-data) caller)
    false
  ))

;; Get asset rarity level
(define-private (get-rarity-level (asset-id uint))
  (default-to u0
    (get rarity
      (map-get? asset-registry { asset-id: asset-id })
    )
  ))

;; Validate attribute format
(define-private (is-valid-attribute (attribute (string-ascii 32)))
  (and
    (> (len attribute) u0)
    (< (len attribute) u33)
  ))

;; Validate all attributes in collection
(define-private (validate-attributes (attributes (list 10 (string-ascii 32))))
  (and
    (> (len attributes) u0)
    (<= (len attributes) u10)
    (is-eq (len (filter is-valid-attribute attributes)) (len attributes))
  ))

;; ===== Gaming Management Functions =====

;; Add new asset to marketplace
(define-public (add-asset
  (name (string-ascii 64))
  (rarity uint)
  (description (string-ascii 128))
  (attributes (list 10 (string-ascii 32))))
  (let
    (
      (next-id (+ (var-get total-assets) u1))
    )
    ;; Validate inputs
    (asserts! (> (len name) u0) invalid-name)
    (asserts! (< (len name) u65) invalid-name)
    (asserts! (> rarity u0) invalid-rarity)
    (asserts! (< rarity u6) invalid-rarity)
    (asserts! (> (len description) u0) invalid-name)
    (asserts! (< (len description) u129) invalid-name)
    (asserts! (validate-attributes attributes) invalid-attributes)
    
    ;; Register asset
    (map-insert asset-registry
      { asset-id: next-id }
      {
        name: name,
        owner: tx-sender,
        rarity: rarity,
        creation-block: stacks-block-height,
        description: description,
        attributes: attributes
      }
    )
    
    ;; Grant owner trading permission
    (map-insert trading-permissions
      { asset-id: next-id, trader: tx-sender }
      { can-trade: true }
    )
    
    ;; Update counter
    (var-set total-assets next-id)
    (ok next-id)
  ))

;; Update existing asset details
(define-public (update-asset
  (asset-id uint)
  (new-name (string-ascii 64))
  (new-rarity uint)
  (new-description (string-ascii 128))
  (new-attributes (list 10 (string-ascii 32))))
  (let
    (
      (asset-data (unwrap! (map-get? asset-registry { asset-id: asset-id }) asset-not-found))
    )
    ;; Verify permissions and inputs
    (asserts! (asset-exists asset-id) asset-not-found)
    (asserts! (is-eq (get owner asset-data) tx-sender) not-owner)
    (asserts! (> (len new-name) u0) invalid-name)
    (asserts! (< (len new-name) u65) invalid-name)
    (asserts! (> new-rarity u0) invalid-rarity)
    (asserts! (< new-rarity u6) invalid-rarity)
    (asserts! (> (len new-description) u0) invalid-name)
    (asserts! (< (len new-description) u129) invalid-name)
    (asserts! (validate-attributes new-attributes) invalid-attributes)
    
    ;; Update asset information
    (map-set asset-registry
      { asset-id: asset-id }
      (merge asset-data {
        name: new-name,
        rarity: new-rarity,
        description: new-description,
        attributes: new-attributes
      })
    )
    (ok true)
  ))

;; Remove asset from marketplace
(define-public (remove-asset (asset-id uint))
  (let
    (
      (asset-data (unwrap! (map-get? asset-registry { asset-id: asset-id }) asset-not-found))
    )
    ;; Verify owner ownership
    (asserts! (asset-exists asset-id) asset-not-found)
    (asserts! (is-eq (get owner asset-data) tx-sender) not-owner)
    
    ;; Remove from marketplace
    (map-delete asset-registry { asset-id: asset-id })
    (ok true)
  ))

;; Transfer asset to new owner
(define-public (transfer-asset (asset-id uint) (new-owner principal))
  (let
    (
      (asset-data (unwrap! (map-get? asset-registry { asset-id: asset-id }) asset-not-found))
    )
    ;; Verify current owner
    (asserts! (asset-exists asset-id) asset-not-found)
    (asserts! (is-eq (get owner asset-data) tx-sender) not-owner)
    
    ;; Transfer ownership
    (map-set asset-registry
      { asset-id: asset-id }
      (merge asset-data { owner: new-owner })
    )
    (ok true)
  ))

;; ===== Read-Only Functions =====

;; Get total assets in marketplace
(define-read-only (get-total-assets)
  (var-get total-assets))

;; Get asset details
(define-read-only (get-asset-info (asset-id uint))
  (map-get? asset-registry { asset-id: asset-id }))

;; Get trading permission
(define-read-only (get-trading-permission (asset-id uint) (trader principal))
  (map-get? trading-permissions { asset-id: asset-id, trader: trader }))

;; Get marketplace stats
(define-read-only (get-marketplace-stats)
  {
    admin: marketplace-admin,
    total-assets: (var-get total-assets)
  })