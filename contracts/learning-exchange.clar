;; ===========================================
;; CONTRACT 1: language-exchange-core.clar
;; ===========================================

;; Core contract for the decentralized language learning exchange platform
;; Handles user profiles, language pairing, sessions, and cultural exchange

;; ===== CONSTANTS =====
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-USER-NOT-FOUND (err u101))
(define-constant ERR-INVALID-LANGUAGE (err u102))
(define-constant ERR-ALREADY-PAIRED (err u103))
(define-constant ERR-SESSION-NOT-FOUND (err u104))
(define-constant ERR-INVALID-RATING (err u105))
(define-constant ERR-INSUFFICIENT-BALANCE (err u106))
(define-constant ERR-INVALID-PROFICIENCY (err u107))

;; Session fee in microSTX (0.1 STX)
(define-constant SESSION-FEE u100000)

;; ===== DATA STRUCTURES =====

;; User profile structure
(define-map user-profiles
  { user: principal }
  {
    native-language: (string-ascii 10),
    learning-languages: (list 5 (string-ascii 10)),
    proficiency-levels: (list 5 uint), ;; 1-5 scale for each learning language
    bio: (string-utf8 500),
    country: (string-ascii 50),
    timezone: (string-ascii 10),
    total-sessions: uint,
    average-rating: uint, ;; 1-100 scale
    total-ratings: uint,
    is-active: bool,
    joined-at: uint
  }
)

;; Language pair matching
(define-map language-pairs
  { user1: principal, user2: principal }
  {
    user1-native: (string-ascii 10),
    user1-learning: (string-ascii 10),
    user2-native: (string-ascii 10),
    user2-learning: (string-ascii 10),
    compatibility-score: uint,
    paired-at: uint,
    is-active: bool
  }
)

;; Learning sessions
(define-map learning-sessions
  { session-id: uint }
  {
    user1: principal,
    user2: principal,
    language-focus: (string-ascii 10),
    scheduled-time: uint,
    duration: uint, ;; in minutes
    status: (string-ascii 20), ;; "scheduled", "completed", "cancelled"
    user1-rating: (optional uint),
    user2-rating: (optional uint),
    notes: (string-utf8 1000),
    created-at: uint
  }
)

;; Cultural exchange posts
(define-map cultural-posts
  { post-id: uint }
  {
    author: principal,
    title: (string-utf8 200),
    content: (string-utf8 2000),
    language: (string-ascii 10),
    category: (string-ascii 50), ;; "food", "tradition", "history", "lifestyle"
    likes: uint,
    created-at: uint
  }
)

;; Lesson plans
(define-map lesson-plans
  { plan-id: uint }
  {
    creator: principal,
    title: (string-utf8 100),
    description: (string-utf8 500),
    language: (string-ascii 10),
    difficulty-level: uint, ;; 1-5
    duration: uint, ;; in minutes
    topics: (list 10 (string-utf8 50)),
    materials: (string-utf8 1000),
    usage-count: uint,
    average-rating: uint,
    created-at: uint
  }
)

;; ===== COUNTERS =====
(define-data-var session-counter uint u0)
(define-data-var post-counter uint u0)
(define-data-var plan-counter uint u0)

;; ===== AUTHORIZATION =====
(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT-OWNER)
)

;; ===== USER PROFILE FUNCTIONS =====

;; Create or update user profile
(define-public (create-profile
  (native-language (string-ascii 10))
  (learning-languages (list 5 (string-ascii 10)))
  (proficiency-levels (list 5 uint))
  (bio (string-utf8 500))
  (country (string-ascii 50))
  (timezone (string-ascii 10))
)
  (let ((current-block burn-block-height))
    ;; Validate proficiency levels (1-5)
    (asserts! (fold validate-proficiency proficiency-levels true) ERR-INVALID-PROFICIENCY)

    (map-set user-profiles
      { user: tx-sender }
      {
        native-language: native-language,
        learning-languages: learning-languages,
        proficiency-levels: proficiency-levels,
        bio: bio,
        country: country,
        timezone: timezone,
        total-sessions: u0,
        average-rating: u0,
        total-ratings: u0,
        is-active: true,
        joined-at: current-block
      }
    )
    (ok true)
  )
)

;; Helper function to validate proficiency levels
(define-private (validate-proficiency (level uint) (valid bool))
  (and valid (and (>= level u1) (<= level u5)))
)

;; Get user profile
(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles { user: user })
)

;; Update user active status
(define-public (set-active-status (active bool))
  (match (map-get? user-profiles { user: tx-sender })
    current-profile
    (begin
      (map-set user-profiles
        { user: tx-sender }
        (merge current-profile { is-active: active })
      )
      (ok true)
    )
    ERR-USER-NOT-FOUND
  )
)

;; ===== LANGUAGE PAIRING FUNCTIONS =====

;; Create language pair
(define-public (create-language-pair
  (partner principal)
  (my-learning-language (string-ascii 10))
  (partner-learning-language (string-ascii 10))
)
  (let (
    (my-profile (unwrap! (get-user-profile tx-sender) ERR-USER-NOT-FOUND))
    (partner-profile (unwrap! (get-user-profile partner) ERR-USER-NOT-FOUND))
    (current-block burn-block-height)
  )
    ;; Check if pair already exists
    (asserts! (is-none (map-get? language-pairs { user1: tx-sender, user2: partner })) ERR-ALREADY-PAIRED)
    (asserts! (is-none (map-get? language-pairs { user1: partner, user2: tx-sender })) ERR-ALREADY-PAIRED)

    ;; Create the pair
    (map-set language-pairs
      { user1: tx-sender, user2: partner }
      {
        user1-native: (get native-language my-profile),
        user1-learning: my-learning-language,
        user2-native: (get native-language partner-profile),
        user2-learning: partner-learning-language,
        compatibility-score: (calculate-compatibility my-profile partner-profile),
        paired-at: current-block,
        is-active: true
      }
    )
    (ok true)
  )
)

;; Simple compatibility calculation
(define-private (calculate-compatibility (profile1 {native-language: (string-ascii 10), learning-languages: (list 5 (string-ascii 10)), proficiency-levels: (list 5 uint), bio: (string-utf8 500), country: (string-ascii 50), timezone: (string-ascii 10), total-sessions: uint, average-rating: uint, total-ratings: uint, is-active: bool, joined-at: uint}) (profile2 {native-language: (string-ascii 10), learning-languages: (list 5 (string-ascii 10)), proficiency-levels: (list 5 uint), bio: (string-utf8 500), country: (string-ascii 50), timezone: (string-ascii 10), total-sessions: uint, average-rating: uint, total-ratings: uint, is-active: bool, joined-at: uint}))
  (let (
    (same-country (if (is-eq (get country profile1) (get country profile2)) u20 u0))
    (similar-timezone (if (is-eq (get timezone profile1) (get timezone profile2)) u15 u0))
    (base-score u65)
  )
    (+ base-score same-country similar-timezone)
  )
)

;; Get language pair
(define-read-only (get-language-pair (user1 principal) (user2 principal))
  (match (map-get? language-pairs { user1: user1, user2: user2 })
    pair (some pair)
    (map-get? language-pairs { user1: user2, user2: user1 })
  )
)

;; ===== SESSION MANAGEMENT =====

;; Schedule a learning session
(define-public (schedule-session
  (partner principal)
  (language-focus (string-ascii 10))
  (scheduled-time uint)
  (duration uint)
)
  (let (
    (session-id (+ (var-get session-counter) u1))
    (current-block burn-block-height)
  )
    ;; Verify language pair exists
    (asserts! (is-some (get-language-pair tx-sender partner)) ERR-USER-NOT-FOUND)

    ;; Charge session fee
    (try! (stx-transfer? SESSION-FEE tx-sender (as-contract tx-sender)))

    ;; Create session
    (map-set learning-sessions
      { session-id: session-id }
      {
        user1: tx-sender,
        user2: partner,
        language-focus: language-focus,
        scheduled-time: scheduled-time,
        duration: duration,
        status: "scheduled",
        user1-rating: none,
        user2-rating: none,
        notes: u"",
        created-at: current-block
      }
    )

    (var-set session-counter session-id)
    (ok session-id)
  )
)

;; Complete session and add rating
(define-public (complete-session
  (session-id uint)
  (rating uint)
  (notes (string-utf8 1000))
)
  (let ((session (unwrap! (map-get? learning-sessions { session-id: session-id }) ERR-SESSION-NOT-FOUND)))
    ;; Validate rating (1-5)
    (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-RATING)

    ;; Check if user is part of the session
    (asserts! (or (is-eq tx-sender (get user1 session)) (is-eq tx-sender (get user2 session))) ERR-NOT-AUTHORIZED)

    ;; Update session with rating
    (if (is-eq tx-sender (get user1 session))
      (map-set learning-sessions
        { session-id: session-id }
        (merge session {
          user1-rating: (some rating),
          status: "completed",
          notes: notes
        })
      )
      (map-set learning-sessions
        { session-id: session-id }
        (merge session {
          user2-rating: (some rating),
          status: "completed",
          notes: notes
        })
      )
    )

    ;; Update user statistics
    (try! (update-user-stats (if (is-eq tx-sender (get user1 session)) (get user2 session) (get user1 session)) rating))

    (ok true)
  )
)

;; Update user statistics after session
(define-private (update-user-stats (rated-user principal) (rating uint))
  (match (map-get? user-profiles { user: rated-user })
    profile
    (let (
      (new-total-sessions (+ (get total-sessions profile) u1))
      (new-total-ratings (+ (get total-ratings profile) u1))
      (new-average-rating (/ (+ (* (get average-rating profile) (get total-ratings profile)) (* rating u20)) new-total-ratings))
    )
      (map-set user-profiles
        { user: rated-user }
        (merge profile {
          total-sessions: new-total-sessions,
          total-ratings: new-total-ratings,
          average-rating: new-average-rating
        })
      )
      (ok true)
    )
    ERR-USER-NOT-FOUND
  )
)

;; Get session details
(define-read-only (get-session (session-id uint))
  (map-get? learning-sessions { session-id: session-id })
)

;; ===== CULTURAL EXCHANGE =====

;; Create cultural post
(define-public (create-cultural-post
  (title (string-utf8 200))
  (content (string-utf8 2000))
  (language (string-ascii 10))
  (category (string-ascii 50))
)
  (let (
    (post-id (+ (var-get post-counter) u1))
    (current-block burn-block-height)
  )
    ;; Verify user has a profile
    (asserts! (is-some (get-user-profile tx-sender)) ERR-USER-NOT-FOUND)

    (map-set cultural-posts
      { post-id: post-id }
      {
        author: tx-sender,
        title: title,
        content: content,
        language: language,
        category: category,
        likes: u0,
        created-at: current-block
      }
    )

    (var-set post-counter post-id)
    (ok post-id)
  )
)

;; Like a cultural post
(define-public (like-cultural-post (post-id uint))
  (match (map-get? cultural-posts { post-id: post-id })
    post
    (begin
      (map-set cultural-posts
        { post-id: post-id }
        (merge post { likes: (+ (get likes post) u1) })
      )
      (ok true)
    )
    ERR-SESSION-NOT-FOUND
  )
)

;; Get cultural post
(define-read-only (get-cultural-post (post-id uint))
  (map-get? cultural-posts { post-id: post-id })
)

;; ===== LESSON PLANS =====

;; Create lesson plan
(define-public (create-lesson-plan
  (title (string-utf8 100))
  (description (string-utf8 500))
  (language (string-ascii 10))
  (difficulty-level uint)
  (duration uint)
  (topics (list 10 (string-utf8 50)))
  (materials (string-utf8 1000))
)
  (let (
    (plan-id (+ (var-get plan-counter) u1))
    (current-block burn-block-height)
  )
    ;; Validate difficulty level
    (asserts! (and (>= difficulty-level u1) (<= difficulty-level u5)) ERR-INVALID-PROFICIENCY)

    ;; Verify user has a profile
    (asserts! (is-some (get-user-profile tx-sender)) ERR-USER-NOT-FOUND)

    (map-set lesson-plans
      { plan-id: plan-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        language: language,
        difficulty-level: difficulty-level,
        duration: duration,
        topics: topics,
        materials: materials,
        usage-count: u0,
        average-rating: u0,
        created-at: current-block
      }
    )

    (var-set plan-counter plan-id)
    (ok plan-id)
  )
)

;; Use lesson plan (increment usage count)
(define-public (use-lesson-plan (plan-id uint))
  (match (map-get? lesson-plans { plan-id: plan-id })
    plan
    (begin
      (map-set lesson-plans
        { plan-id: plan-id }
        (merge plan { usage-count: (+ (get usage-count plan) u1) })
      )
      (ok true)
    )
    ERR-SESSION-NOT-FOUND
  )
)

;; Get lesson plan
(define-read-only (get-lesson-plan (plan-id uint))
  (map-get? lesson-plans { plan-id: plan-id })
)

;; ===== READ-ONLY FUNCTIONS =====

;; Get total registered users count
(define-read-only (get-platform-stats)
  {
    total-sessions: (var-get session-counter),
    total-cultural-posts: (var-get post-counter),
    total-lesson-plans: (var-get plan-counter),
    contract-balance: (stx-get-balance (as-contract tx-sender))
  }
)
