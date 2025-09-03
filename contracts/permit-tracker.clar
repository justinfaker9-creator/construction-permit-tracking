
;; title: permit-tracker
;; version: 1.0.0
;; summary: Construction permit tracking and management system
;; description: Smart contract for managing building permits, inspections, and compliance verification

;; constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PERMIT-NOT-FOUND (err u101))
(define-constant ERR-INVALID-STATUS (err u102))
(define-constant ERR-INSPECTION-NOT-FOUND (err u103))
(define-constant ERR-ALREADY-EXISTS (err u104))

;; data vars
(define-data-var permit-id-nonce uint u0)
(define-data-var inspection-id-nonce uint u0)

;; data maps
(define-map permits uint {
  applicant: principal,
  contractor: principal,
  property-address: (string-ascii 256),
  permit-type: (string-ascii 64),
  description: (string-ascii 512),
  status: (string-ascii 32),
  application-date: uint,
  approval-date: (optional uint),
  expiry-date: (optional uint),
  fees-paid: uint,
  compliance-verified: bool
})

(define-map inspections uint {
  permit-id: uint,
  inspector: principal,
  inspection-type: (string-ascii 64),
  scheduled-date: uint,
  completed-date: (optional uint),
  status: (string-ascii 32),
  notes: (string-ascii 512),
  passed: (optional bool)
})

(define-map contractors principal {
  name: (string-ascii 128),
  license-number: (string-ascii 64),
  verified: bool,
  registration-date: uint
})

;; public functions

;; Submit permit application
(define-public (submit-permit-application 
  (contractor principal)
  (property-address (string-ascii 256))
  (permit-type (string-ascii 64))
  (description (string-ascii 512))
  (fees uint))
  (let
    ((permit-id (+ (var-get permit-id-nonce) u1)))
    (asserts! (is-verified-contractor contractor) ERR-NOT-AUTHORIZED)
    (map-set permits permit-id {
      applicant: tx-sender,
      contractor: contractor,
      property-address: property-address,
      permit-type: permit-type,
      description: description,
      status: "pending",
      application-date: stacks-block-height,
      approval-date: none,
      expiry-date: none,
      fees-paid: fees,
      compliance-verified: false
    })
    (var-set permit-id-nonce permit-id)
    (ok permit-id)
  )
)

;; Register contractor
(define-public (register-contractor
  (contractor principal)
  (name (string-ascii 128))
  (license-number (string-ascii 64)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? contractors contractor)) ERR-ALREADY-EXISTS)
    (map-set contractors contractor {
      name: name,
      license-number: license-number,
      verified: true,
      registration-date: stacks-block-height
    })
    (ok true)
  )
)

;; Approve permit (admin only)
(define-public (approve-permit (permit-id uint) (expiry-days uint))
  (let
    ((permit (unwrap! (map-get? permits permit-id) ERR-PERMIT-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status permit) "pending") ERR-INVALID-STATUS)
    (map-set permits permit-id (merge permit {
      status: "approved",
      approval-date: (some stacks-block-height),
      expiry-date: (some (+ stacks-block-height expiry-days))
    }))
    (ok true)
  )
)

;; Schedule inspection
(define-public (schedule-inspection
  (permit-id uint)
  (inspector principal)
  (inspection-type (string-ascii 64))
  (scheduled-date uint))
  (let
    ((permit (unwrap! (map-get? permits permit-id) ERR-PERMIT-NOT-FOUND))
     (inspection-id (+ (var-get inspection-id-nonce) u1)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status permit) "approved") ERR-INVALID-STATUS)
    (map-set inspections inspection-id {
      permit-id: permit-id,
      inspector: inspector,
      inspection-type: inspection-type,
      scheduled-date: scheduled-date,
      completed-date: none,
      status: "scheduled",
      notes: "",
      passed: none
    })
    (var-set inspection-id-nonce inspection-id)
    (ok inspection-id)
  )
)

;; Complete inspection
(define-public (complete-inspection
  (inspection-id uint)
  (passed bool)
  (notes (string-ascii 512)))
  (let
    ((inspection (unwrap! (map-get? inspections inspection-id) ERR-INSPECTION-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get inspector inspection)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status inspection) "scheduled") ERR-INVALID-STATUS)
    (map-set inspections inspection-id (merge inspection {
      completed-date: (some stacks-block-height),
      status: "completed",
      notes: notes,
      passed: (some passed)
    }))
    (ok true)
  )
)

;; Update permit compliance status
(define-public (update-compliance-status (permit-id uint) (verified bool))
  (let
    ((permit (unwrap! (map-get? permits permit-id) ERR-PERMIT-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set permits permit-id (merge permit {
      compliance-verified: verified
    }))
    (ok true)
  )
)

;; Revoke permit
(define-public (revoke-permit (permit-id uint))
  (let
    ((permit (unwrap! (map-get? permits permit-id) ERR-PERMIT-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set permits permit-id (merge permit {
      status: "revoked"
    }))
    (ok true)
  )
)

;; read only functions

;; Get permit details
(define-read-only (get-permit (permit-id uint))
  (map-get? permits permit-id)
)

;; Get inspection details
(define-read-only (get-inspection (inspection-id uint))
  (map-get? inspections inspection-id)
)

;; Get contractor info
(define-read-only (get-contractor (contractor principal))
  (map-get? contractors contractor)
)

;; Check if permit is expired
(define-read-only (is-permit-expired (permit-id uint))
  (match (map-get? permits permit-id)
    permit (match (get expiry-date permit)
      expiry (> stacks-block-height expiry)
      false)
    false)
)

;; Get permit count
(define-read-only (get-permit-count)
  (var-get permit-id-nonce)
)

;; Get inspection count
(define-read-only (get-inspection-count)
  (var-get inspection-id-nonce)
)

;; private functions

;; Check if contractor is verified
(define-private (is-verified-contractor (contractor principal))
  (match (map-get? contractors contractor)
    contractor-data (get verified contractor-data)
    false)
)
