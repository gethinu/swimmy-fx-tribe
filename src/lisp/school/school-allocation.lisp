;;; school-allocation.lisp - Warrior Allocation & Position Reconciliation
;;; Part of the Swimmy School System
;;; Implements PAO (Positive Acknowledgement Only) Protocol to prevent Ghost Positions.
;;; V19.6

(in-package :swimmy.school)

;;; ==========================================
;;; STATE
;;; ==========================================

;; Active Warriors (CONFIRMED by MT5)
(defparameter *warrior-allocation* (make-hash-table :test 'equal)) 
;; Key: "Category-Slot" (e.g., "trend-0")
;; Value: Property List (:strategy "Name" :symbol "USDJPY" :magic 12345 ...)

;; Pending Orders (SENT to Guardian, Awaiting MT5)
(defparameter *pending-orders* (make-hash-table :test 'eql)) 
;; Key: Magic Number (Integer)
;; Value: Property List (:timestamp 1234567890 :strategy "Name" :symbol "USDJPY" ...)

(defparameter *pending-ttl* 60) ; Seconds to wait for MT5 confirmation before giving up

;;; ==========================================
;;; MAGIC NUMBER LOGIC (Reversible)
;;; ==========================================

(defun get-category-id (category)
  (case category
    (:trend 1)
    (:reversion 2)
    (:breakout 3)
    (:scalp 4)
    (t 9)))

(defun get-id-category (id)
  (case id
    (1 :trend)
    (2 :reversion)
    (3 :breakout)
    (4 :scalp)
    (t :unknown)))

(defun get-warrior-magic (category slot-index)
  "Generate deterministic and reversible magic number.
   Format: 1[Category_ID][Slot_Index] (e.g., Trend Slot 0 -> 110000)"
  ;; Base: 100,000
  ;; Cat:  10,000 * ID
  ;; Slot: 1 * Slot
  (+ 100000 
     (* (get-category-id category) 10000) 
     slot-index))

(defun decode-warrior-magic (magic)
  "Decode magic number to extract Category and Slot"
  (if (and (>= magic 100000) (< magic 200000))
      (let* ((val (- magic 100000))
             (cat-id (floor val 10000))
             (slot (mod val 10000)))
        (values (get-id-category cat-id) slot))
      (values :unknown 0)))

(defun try-reserve-warrior-slot (category strategy-name symbol direction)
  "V44.2: Atomic Slot Reservation (Expert Panel Approved)
   Attempts to find AND reserve a slot atomically.
   Returns (values slot magic) if successful, or (values nil nil) if full.
   This prevents the 'Musical Chairs' race condition."
  (dotimes (i 4)
    (let* ((key (format nil "~a-~d" category i))
           (magic (get-warrior-magic category i))
           (active (gethash key *warrior-allocation*))
           (pending (gethash magic *pending-orders*)))
      (unless (or active pending)
        ;; ATOMIC RESERVATION: Immediately register pending order
        (register-pending-order magic strategy-name symbol category direction)
        (format t "[ALLOC] 🔒 Slot ~d Reserved for ~a (Magic ~d)~%" i strategy-name magic)
        (return-from try-reserve-warrior-slot (values i magic)))))
  (values nil nil))

;;; ==========================================
;;; PENDING ORDER MANAGEMENT
;;; ==========================================

(defun register-pending-order (magic strategy-name symbol category direction)
  "Register a trade as PENDING. Do not allocate warrior slot yet."
  (setf (gethash magic *pending-orders*)
        (list :timestamp (get-universal-time)
              :strategy strategy-name
              :symbol symbol
              :category category
              :direction direction
              :magic magic))
  (format t "[ALLOC] ⏳ Pending Order Registered: Magic ~d (~a ~a)~%" magic strategy-name direction))

(defun check-pending-timeouts ()
  "Remove pending orders that timed out (never confirmed by MT5)"
  (let ((now (get-universal-time))
        (dead-magic nil))
    (maphash (lambda (magic info)
               (when (> (- now (getf info :timestamp)) *pending-ttl*)
                 (push magic dead-magic)))
             *pending-orders*)
    (dolist (m dead-magic)
      (remhash m *pending-orders*)
      (format t "[ALLOC] ❌ Pending Order TIMEOUT: Magic ~d~%" m))))

;;; ==========================================
;;; RECONCILIATION (The Truth Protocol)
;;; ==========================================

(defun request-mt5-positions ()
  "Send GET_POSITIONS command to MT5"
  (when (boundp 'swimmy.core::*cmd-publisher*)
    (let ((msg (jsown:to-json (jsown:new-js ("action" "GET_POSITIONS")))))
      (pzmq:send swimmy.core::*cmd-publisher* msg))))

(defun reconcile-with-mt5-positions (symbol mt5-positions)
  "Sync Brain state with MT5 Reality.
   1. Promote Pending -> Active (Confirmation)
   2. Adopt Unknowns
   3. Bust Ghosts"
  
  (check-pending-timeouts)

  (let ((active-magics (make-hash-table :test 'eql))
        (ghost-warriors nil)
        (promoted-count 0)
        (busted-count 0))
    
    ;; --- 1. Process Actual MT5 Positions ---
    (dolist (pos mt5-positions)
      (let* ((magic (if (jsown:keyp pos "magic") (jsown:val pos "magic") 0))
             (ticket (jsown:val pos "ticket"))
             (profit (jsown:val pos "profit")))
        
        (setf (gethash magic active-magics) t)
        
        ;; Check if this magic is in PENDING -> Promote
        (let ((pending (gethash magic *pending-orders*)))
          (when pending
            (let* ((cat (getf pending :category))
                   (slot (second (multiple-value-list (decode-warrior-magic magic)))) ;; Get slot from Magic
                   (key (format nil "~a-~d" cat slot)))
              
              ;; PROMOTE TO WARRIOR
              (let ((strat-name (getf pending :strategy)))
                (setf (gethash key *warrior-allocation*)
                      (append pending 
                              (list :ticket ticket 
                                    :start-time (get-universal-time))))
                (remhash magic *pending-orders*)
                (incf promoted-count)
                (format t "[ALLOC] ✅ TRADE CONFIRMED: ~a (Magic ~d) Promoted to Warrior! Strategy: ~a~%" key magic strat-name)))))
        
        ;; Check if this Magic is NOT allocated (Adoption)
        ;; Logic: Decode Magic, check if slot is empty. If so, adopt.
        (multiple-value-bind (cat slot) (decode-warrior-magic magic)
          (unless (eq cat :unknown)
            (let ((key (format nil "~a-~d" cat slot)))
              (unless (gethash key *warrior-allocation*)
                ;; Adopt Orphan - Try to find active strategy for category as best guess
                (let ((best-guess-strat (if (boundp '*active-team*) 
                                          (let ((leader (first (gethash cat *active-team*))))
                                            (if leader (strategy-name leader) "Restored-Warrior"))
                                          "Restored-Warrior")))
                  (setf (gethash key *warrior-allocation*)
                        (list :strategy best-guess-strat
                              :symbol symbol
                              :category cat
                              :magic magic
                              :ticket ticket
                              :start-time (get-universal-time)))
                  (format t "[ALLOC] 🍼 ORPHAN ADOPTED: ~a (Magic ~d) restored from MT5. Strategy: ~a~%" key magic best-guess-strat))))))))

    ;; --- 2. Find Ghosts (Brain says yes, MT5 says no) ---
    (maphash (lambda (key warrior)
               (let ((w-symbol (getf warrior :symbol))
                     (w-magic (getf warrior :magic)))
                 (when (and (equal w-symbol symbol)
                            w-magic
                            (not (gethash w-magic active-magics)))
                   ;; Double check grace period (e.g. just promoted)
                   (let ((start-time (getf warrior :start-time)))
                     (when (and start-time (> (- (get-universal-time) start-time) 10)) ;; 10s buffer
                       (push (list key w-magic) ghost-warriors))))))
             *warrior-allocation*)
    
    ;; --- 3. Bust Ghosts ---
    (dolist (ghost ghost-warriors)
      (let ((key (first ghost))
            (magic (second ghost)))
        (remhash key *warrior-allocation*)
        (incf busted-count)
        (format t "[ALLOC] 👻 GHOST BUSTED: ~a (Magic ~d) removed.~%" key magic)))
    
    (when (or (> promoted-count 0) (> busted-count 0))
      (format t "[ALLOC] Reconcile Summary: +~d Confirmed, -~d Ghosts.~%" promoted-count busted-count))))

;;; ==========================================
;;; MAINTENANCE
;;; ==========================================

(defun cleanup-stale-allocations ()
  "Periodic maintenance for allocations.
   Called by periodic scheduler.
   1. Check pending timeouts.
   2. Request active sync with MT5 periodically."
  (check-pending-timeouts)
  
  ;; V19.6: Active Ghost Prevention
  ;; Force sync every 15 seconds
  (when (= (mod (get-universal-time) 15) 0)
    (request-mt5-positions)))

(defun report-active-positions ()
  "Send a detailed report of all active positions (Strategy mappings)"
  (handler-case
      (let ((count 0)
            (msg "🏰 **Active Strategy Status**\n━━━━━━━━━━━━━━━━━━━━━━━━━━\n"))
        (maphash (lambda (k v)
                   (declare (ignore k))
                   (when v
                     (let* ((cat (getf v :category))
                            (sym (getf v :symbol))
                            (dir (getf v :direction))
                            (magic (getf v :magic))
                            (entry (getf v :entry))
                            ;; Strat name might not be directly in warrior, fallback to active team leader if needed
                            ;; But warrior allocation doesn't store strategy name explicitly...
                            ;; Wait, register-pending-order DOES store it or we need to find it.
                            ;; school-execution.lisp:428 (register-pending-order magic lead-name ...)
                            ;; Let's check register-pending-order implementation.
                            ;; If missing, we infer from *active-team*
                            (strat-name (or (getf v :strategy) 
                                          (let ((leader (first (gethash cat *active-team*))))
                                            (if leader (strategy-name leader) "Unknown")))))
                       
                       (setf msg (format nil "~a\n**~a** (~a)\nStrat: `~a`\nMagic: `~d` | Entry: ~,3f\n" 
                                         msg cat sym strat-name magic entry))
                       (incf count))))
                 *warrior-allocation*)
        
        (if (> count 0)
            (swimmy.shell:notify-discord msg :color 3447003)
            (swimmy.shell:notify-discord "🏰 **Status Report**: No active positions." :color 15158332))
        (format t "[L] 🏰 Status Report sent (~d positions)~%" count))
    (error (e) (format t "[L] Report Error: ~a~%" e))))
