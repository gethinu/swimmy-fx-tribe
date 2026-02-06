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
;;; KELLY CRITERION LOGIC (Phase 12)
;;; ==========================================

(defparameter *base-lot* 0.01 "Default base lot if Kelly fails")
(defparameter *max-kelly-fraction* 0.10 "Safety cap: max 10% risk of equity per trade")
(defparameter *max-simultaneous* 3)

(defun calculate-kelly-lot (win-rate avg-win avg-loss equity &optional (volatility 0.01))
  "Calculate optimal position size using Kelly Criterion.
   f* = p - q/b where p=win-rate, q=loss-rate, b=avg-win/avg-loss.
   Result is clamped to safety limits."
  
  (let* ((p win-rate)
         (q (- 1.0 p))
         ;; Avoid division by zero
         (loss (max 0.0001 (abs avg-loss)))
         (win (max 0.0001 avg-win))
         (b (/ win loss))
         ;; Full Kelly
         (f-star (if (> b 0) (- p (/ q b)) 0.0))
         ;; Half Kelly for safety
         (safe-kelly (* f-star 0.5)))
    
    (if (<= safe-kelly 0)
        *base-lot* ; No edge, return min lot
        (let* ((risk-amt (* equity (min safe-kelly *max-kelly-fraction*)))
               ;; Estimate stop distance based on volatility (1.5 * ATR approx)
               ;; e.g. for USDJPY 0.01 vol -> ~1.50 movement -> ~150 pips
               (stop-distance (* volatility 100.0)) 
               (lot (/ risk-amt (* stop-distance 1000.0)))) ; Approx 1000 units per lot pip
          
          (clamp-lot lot)))))

(defun clamp-lot (lot)
  "Clamp lot size to broker limits (0.01 - 5.0)"
  (cond ((< lot 0.01) 0.01)
        ((> lot 2.0) 2.0)  ; Soft cap
        (t (float (/ (round (* lot 100)) 100)))))

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

(defun get-warrior-magic (category slot-index &optional (strategy-name "Unknown"))
  "Generate deterministic magic number combining Slot and Strategy Hash.
   Format: 1[CatID][Slot][Hash(5)] (9 digits total)
   e.g. Trend(1) Slot(0) Hash(12345) -> 110012345"
  (let ((cat-id (get-category-id category))
        (hash-part (mod (sxhash (string strategy-name)) 100000)))
    (+ 100000000                  ; Base 9 digits
       (* cat-id 10000000)        ; Category
       (* slot-index 1000000)     ; Slot
       hash-part)))               ; Strategy Uniqueness

(defun decode-warrior-magic (magic)
  "Decode magic number to extract Category and Slot.
   Ignores the unique hash suffix."
  (if (and (>= magic 100000000) (< magic 200000000))
      (let* ((val (- magic 100000000))
             (cat-id (floor val 10000000))
             (rem (mod val 10000000))
             (slot (floor rem 1000000)))
        (values (get-id-category cat-id) slot))
      (values :unknown 0)))

(defun try-reserve-warrior-slot (category strategy-name symbol direction &key pair-id lot)
  "V44.2: Atomic Slot Reservation (Expert Panel Approved)
   Attempts to find AND reserve a slot atomically.
   Returns (values slot magic) if successful, or (values nil nil) if full.
   This prevents the 'Musical Chairs' race condition."
  (dotimes (i 4)
    (let* ((key (format nil "~a-~d" category i))
           ;; V44.8: Unique Magic Hash
           (magic (get-warrior-magic category i strategy-name))
           (active (gethash key *warrior-allocation*))
           ;; Check if ANY magic exists for this slot/category pattern?
           ;; But simpler: if slot is free (active nil), we can take it.
           ;; Pending orders are keyed by Magic. Since Magic is now unique per strategy,
           ;; we risk multiple strategies claiming same slot if we don't check slot usage carefully.
           ;; However, active checked by KEY (cat-slot), so that is safe.
           ;; We just need to ensure we don't overwrite a pending order (unlikely with unique magic).
           (pending (gethash magic *pending-orders*)))
      (unless (or active pending)
        ;; ATOMIC RESERVATION: Immediately register pending order
        (register-pending-order magic strategy-name symbol category direction :pair-id pair-id :lot lot)
        (format t "[ALLOC] 🔒 Slot ~d Reserved for ~a (Magic ~d)~%" i strategy-name magic)
        (return-from try-reserve-warrior-slot (values i magic)))))
  (values nil nil))

;;; ==========================================
;;; PENDING ORDER MANAGEMENT
;;; ==========================================

(defun register-pending-order (magic strategy-name symbol category direction &key pair-id lot)
  "Register a trade as PENDING. Do not allocate warrior slot yet."
  (setf (gethash magic *pending-orders*)
        (list :timestamp (get-universal-time)
              :strategy strategy-name
              :symbol symbol
              :category category
              :direction direction
              :magic magic
              :pair-id pair-id
              :lot lot))
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
    (let ((msg (swimmy.core:encode-sexp '((type . "GET_POSITIONS")))))
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
             (ticket (jsown:val pos "ticket")))
        
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

;;; ==========================================
;;; QUERY HELPER
;;; ==========================================

(defun lookup-strategy-by-magic (magic)
  "Find strategy name from magic number (checking Pending and Active Warriors)"
  (when magic
    ;; 1. Check Pending
    (let ((pending (gethash magic *pending-orders*)))
      (when pending (return-from lookup-strategy-by-magic (getf pending :strategy))))
    
    ;; 2. Check Active Warriors
    (maphash (lambda (key warrior)
               (declare (ignore key))
               (when (eql (getf warrior :magic) magic)
                 (return-from lookup-strategy-by-magic (getf warrior :strategy))))
             *warrior-allocation*))
  nil)

(defun lookup-pair-id-by-magic (magic)
  "Find pair-id from magic number (checking Pending and Active Warriors)"
  (when magic
    (let ((pending (gethash magic *pending-orders*)))
      (when pending (return-from lookup-pair-id-by-magic (getf pending :pair-id))))
    (maphash (lambda (key warrior)
               (declare (ignore key))
               (when (eql (getf warrior :magic) magic)
                 (return-from lookup-pair-id-by-magic (getf warrior :pair-id))))
             *warrior-allocation*))
  nil)

(defun force-close-strategy-positions (strategy-name)
  "Force close all positions belonging to a specific strategy (e.g., during extinction)"
  (let ((closed-count 0))
    (format t "[ALLOC] 💀 Force closing positions for extinct strategy: ~a~%" strategy-name)
    (handler-case
      (maphash 
       (lambda (key warrior)
         (let ((warrior-strat (getf warrior :strategy))
               (symbol (getf warrior :symbol))
               (magic (getf warrior :magic)))
           (when (and warrior-strat (string= warrior-strat strategy-name))
             (format t "[ALLOC] ✂️ Closing ~a Position (Key: ~a, Magic: ~a)~%" symbol key magic)
             (let ((msg (swimmy.core:encode-sexp `((type . "CLOSE")
                                                   (symbol . ,symbol)
                                                   (magic . ,magic)))))
               (pzmq:send *cmd-publisher* msg))
             (incf closed-count))))
       *warrior-allocation*)
      (error (e) (format t "[ALLOC] ⚠️ Error closing positions: ~a~%" e)))
    closed-count))
