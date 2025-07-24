;; Blockchain-Based Therapeutic Tree Communication Program
;; Tree Communication Registry Contract
;; Manages tree registration, ecological data, and interspecies communication records

;; ===== CONSTANTS =====
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_TREE_NOT_FOUND (err u101))
(define-constant ERR_INVALID_COORDINATES (err u102))
(define-constant ERR_TREE_ALREADY_EXISTS (err u103))
(define-constant ERR_INVALID_PARAMETERS (err u104))
(define-constant ERR_COMMUNICATION_NOT_FOUND (err u105))

;; ===== DATA STRUCTURES =====

;; Tree registration data
(define-map trees
  { tree-id: uint }
  {
    species: (string-ascii 64),
    location-lat: int, ;; Stored as integer (multiply by 1000000 for precision)
    location-lng: int,
    age-estimate: uint,
    height-cm: uint,
    circumference-cm: uint,
    health-status: (string-ascii 32), ;; "healthy", "stressed", "diseased", "thriving"
    ecological-role: (string-ascii 128),
    indigenous-name: (optional (string-ascii 64)),
    indigenous-significance: (optional (string-ascii 256)),
    accessibility-features: (string-ascii 256),
    registered-by: principal,
    registration-date: uint
  }
)

;; Communication session records
(define-map communication-sessions
  { session-id: uint }
  {
    tree-id: uint,
    participant: principal,
    session-type: (string-ascii 32), ;; "meditation", "observation", "therapeutic", "educational"
    duration-minutes: uint,
    emotional-state-before: (string-ascii 32),
    emotional-state-after: (string-ascii 32),
    observations: (string-ascii 512),
    environmental-conditions: (string-ascii 256),
    accessibility-accommodations: (optional (string-ascii 256)),
    session-date: uint,
    verified: bool
  }
)

;; Educational content mapping
(define-map dendrology-content
  { tree-id: uint }
  {
    botanical-info: (string-ascii 512),
    ecological-benefits: (string-ascii 512),
    traditional-uses: (optional (string-ascii 512)),
    conservation-status: (string-ascii 64),
    seasonal-changes: (string-ascii 512),
    wildlife-relationships: (string-ascii 512),
    added-by: principal,
    last-updated: uint
  }
)

;; User progress tracking
(define-map user-profiles
  { user: principal }
  {
    total-sessions: uint,
    trees-visited: uint,
    favorite-tree-id: (optional uint),
    accessibility-needs: (optional (string-ascii 256)),
    preferred-session-types: (string-ascii 128),
    indigenous-connection: (optional (string-ascii 128)),
    stewardship-contributions: uint,
    join-date: uint
  }
)

;; ===== DATA VARIABLES =====
(define-data-var next-tree-id uint u1)
(define-data-var next-session-id uint u1)
(define-data-var total-trees uint u0)
(define-data-var total-sessions uint u0)

;; Program administrators
(define-map administrators principal bool)

;; ===== PRIVATE FUNCTIONS =====

(define-private (is-admin (user principal))
  (or
    (is-eq user CONTRACT_OWNER)
    (default-to false (map-get? administrators user))
  )
)

(define-private (validate-coordinates (lat int) (lng int))
  (and
    (>= lat -90000000) (<= lat 90000000)   ;; -90 to 90 degrees
    (>= lng -180000000) (<= lng 180000000) ;; -180 to 180 degrees
  )
)

(define-private (increment-user-stats (user principal) (tree-id uint))
  (let ((current-profile (default-to
    { total-sessions: u0, trees-visited: u0, favorite-tree-id: none,
      accessibility-needs: none, preferred-session-types: "",
      indigenous-connection: none, stewardship-contributions: u0,
      join-date: stacks-block-height }
    (map-get? user-profiles { user: user }))))
    (map-set user-profiles { user: user }
      (merge current-profile {
        total-sessions: (+ (get total-sessions current-profile) u1),
        trees-visited: (if (is-none (get favorite-tree-id current-profile))
                         (+ (get trees-visited current-profile) u1)
                         (get trees-visited current-profile))
      })
    )
  )
)

;; ===== PUBLIC FUNCTIONS =====

;; Administrative functions
(define-public (add-administrator (new-admin principal))
  (begin
    (asserts! (is-admin tx-sender) ERR_UNAUTHORIZED)
    (map-set administrators new-admin true)
    (ok true)
  )
)

(define-public (remove-administrator (admin principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-delete administrators admin)
    (ok true)
  )
)

;; Tree registration and management
(define-public (register-tree
  (species (string-ascii 64))
  (location-lat int)
  (location-lng int)
  (age-estimate uint)
  (height-cm uint)
  (circumference-cm uint)
  (health-status (string-ascii 32))
  (ecological-role (string-ascii 128))
  (indigenous-name (optional (string-ascii 64)))
  (indigenous-significance (optional (string-ascii 256)))
  (accessibility-features (string-ascii 256))
)
  (let ((tree-id (var-get next-tree-id)))
    (asserts! (validate-coordinates location-lat location-lng) ERR_INVALID_COORDINATES)
    (asserts! (> (len species) u0) ERR_INVALID_PARAMETERS)
    (asserts! (> height-cm u0) ERR_INVALID_PARAMETERS)

    (map-set trees { tree-id: tree-id }
      {
        species: species,
        location-lat: location-lat,
        location-lng: location-lng,
        age-estimate: age-estimate,
        height-cm: height-cm,
        circumference-cm: circumference-cm,
        health-status: health-status,
        ecological-role: ecological-role,
        indigenous-name: indigenous-name,
        indigenous-significance: indigenous-significance,
        accessibility-features: accessibility-features,
        registered-by: tx-sender,
        registration-date: stacks-block-height
      }
    )

    (var-set next-tree-id (+ tree-id u1))
    (var-set total-trees (+ (var-get total-trees) u1))
    (ok tree-id)
  )
)

;; Update tree information (only by registrar or admin)
(define-public (update-tree-health
  (tree-id uint)
  (new-health-status (string-ascii 32))
  (updated-ecological-role (string-ascii 128))
)
  (let ((tree-data (unwrap! (map-get? trees { tree-id: tree-id }) ERR_TREE_NOT_FOUND)))
    (asserts! (or
      (is-eq tx-sender (get registered-by tree-data))
      (is-admin tx-sender)
    ) ERR_UNAUTHORIZED)

    (map-set trees { tree-id: tree-id }
      (merge tree-data {
        health-status: new-health-status,
        ecological-role: updated-ecological-role
      })
    )
    (ok true)
  )
)

;; Record communication session
(define-public (record-communication-session
  (tree-id uint)
  (session-type (string-ascii 32))
  (duration-minutes uint)
  (emotional-state-before (string-ascii 32))
  (emotional-state-after (string-ascii 32))
  (observations (string-ascii 512))
  (environmental-conditions (string-ascii 256))
  (accessibility-accommodations (optional (string-ascii 256)))
)
  (let ((session-id (var-get next-session-id)))
    (asserts! (is-some (map-get? trees { tree-id: tree-id })) ERR_TREE_NOT_FOUND)
    (asserts! (> duration-minutes u0) ERR_INVALID_PARAMETERS)

    (map-set communication-sessions { session-id: session-id }
      {
        tree-id: tree-id,
        participant: tx-sender,
        session-type: session-type,
        duration-minutes: duration-minutes,
        emotional-state-before: emotional-state-before,
        emotional-state-after: emotional-state-after,
        observations: observations,
        environmental-conditions: environmental-conditions,
        accessibility-accommodations: accessibility-accommodations,
        session-date: stacks-block-height,
        verified: false
      }
    )

    (var-set next-session-id (+ session-id u1))
    (var-set total-sessions (+ (var-get total-sessions) u1))
    (increment-user-stats tx-sender tree-id)
    (ok session-id)
  )
)

;; Verify communication session (by admin or tree registrar)
(define-public (verify-session (session-id uint))
  (let ((session-data (unwrap! (map-get? communication-sessions { session-id: session-id }) ERR_COMMUNICATION_NOT_FOUND))
        (tree-data (unwrap! (map-get? trees { tree-id: (get tree-id session-data) }) ERR_TREE_NOT_FOUND)))
    (asserts! (or
      (is-admin tx-sender)
      (is-eq tx-sender (get registered-by tree-data))
    ) ERR_UNAUTHORIZED)

    (map-set communication-sessions { session-id: session-id }
      (merge session-data { verified: true })
    )
    (ok true)
  )
)

;; Add or update dendrology content
(define-public (add-dendrology-content
  (tree-id uint)
  (botanical-info (string-ascii 512))
  (ecological-benefits (string-ascii 512))
  (traditional-uses (optional (string-ascii 512)))
  (conservation-status (string-ascii 64))
  (seasonal-changes (string-ascii 512))
  (wildlife-relationships (string-ascii 512))
)
  (begin
    (asserts! (is-some (map-get? trees { tree-id: tree-id })) ERR_TREE_NOT_FOUND)
    (asserts! (is-admin tx-sender) ERR_UNAUTHORIZED)

    (map-set dendrology-content { tree-id: tree-id }
      {
        botanical-info: botanical-info,
        ecological-benefits: ecological-benefits,
        traditional-uses: traditional-uses,
        conservation-status: conservation-status,
        seasonal-changes: seasonal-changes,
        wildlife-relationships: wildlife-relationships,
        added-by: tx-sender,
        last-updated: stacks-block-height
      }
    )
    (ok true)
  )
)

;; Update user profile
(define-public (update-user-profile
  (accessibility-needs (optional (string-ascii 256)))
  (preferred-session-types (string-ascii 128))
  (indigenous-connection (optional (string-ascii 128)))
  (favorite-tree-id (optional uint))
)
  (let ((current-profile (default-to
    { total-sessions: u0, trees-visited: u0, favorite-tree-id: none,
      accessibility-needs: none, preferred-session-types: "",
      indigenous-connection: none, stewardship-contributions: u0,
      join-date: stacks-block-height }
    (map-get? user-profiles { user: tx-sender }))))

    (if (is-some favorite-tree-id)
      (asserts! (is-some (map-get? trees { tree-id: (unwrap-panic favorite-tree-id) })) ERR_TREE_NOT_FOUND)
      true
    )

    (map-set user-profiles { user: tx-sender }
      (merge current-profile {
        accessibility-needs: accessibility-needs,
        preferred-session-types: preferred-session-types,
        indigenous-connection: indigenous-connection,
        favorite-tree-id: favorite-tree-id
      })
    )
    (ok true)
  )
)

;; ===== READ-ONLY FUNCTIONS =====

(define-read-only (get-tree-info (tree-id uint))
  (map-get? trees { tree-id: tree-id })
)

(define-read-only (get-communication-session (session-id uint))
  (map-get? communication-sessions { session-id: session-id })
)

(define-read-only (get-dendrology-content (tree-id uint))
  (map-get? dendrology-content { tree-id: tree-id })
)

(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles { user: user })
)

(define-read-only (get-program-stats)
  {
    total-trees: (var-get total-trees),
    total-sessions: (var-get total-sessions),
    next-tree-id: (var-get next-tree-id),
    next-session-id: (var-get next-session-id)
  }
)

(define-read-only (is-administrator (user principal))
  (is-admin user)
)

;; Get trees within a coordinate range (basic proximity check)
(define-read-only (get-trees-in-range (center-lat int) (center-lng int) (range-degrees int))
  (let ((min-lat (- center-lat range-degrees))
        (max-lat (+ center-lat range-degrees))
        (min-lng (- center-lng range-degrees))
        (max-lng (+ center-lng range-degrees)))
    ;; Note: In a full implementation, this would iterate through trees
    ;; For now, returns the range parameters for client-side filtering
    {
      search-center-lat: center-lat,
      search-center-lng: center-lng,
      search-range: range-degrees,
      min-lat: min-lat,
      max-lat: max-lat,
      min-lng: min-lng,
      max-lng: max-lng
    }
  )
)
