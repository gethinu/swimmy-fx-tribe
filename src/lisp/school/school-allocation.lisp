;;; school-allocation.lisp - Warrior Allocation & Position Reconciliation
;;; Part of the Swimmy School System
;;; Extracted from school-execution.lisp to comply with SRP (Expert Panel 2026-01-14)

(in-package :swimmy.school)

;;; ==========================================
;;; ALLOCATION PARAMETERS
;;; ==========================================

(defparameter *stale-allocation-age* 14400) ; 4 hours = 14400 seconds
(defparameter *last-allocation-cleanup* 0)

;;; ==========================================
;;; ALLOCATION LIFECYCLE
;;; ==========================================

;;; V19: Stale Allocation Detection and Cleanup
(defun cleanup-stale-allocations ()
  "Remove warrior allocations older than *stale-allocation-age* seconds.
   Sends Discord alert when stale allocations are detected."
  (let ((now (get-universal-time))
        (stale-keys nil)
        (removed-count 0))
    ;; Only run every 5 minutes
    (when (< (- now *last-allocation-cleanup*) 300)
      (return-from cleanup-stale-allocations 0))
    (setf *last-allocation-cleanup* now)
    ;; Find stale entries
    (maphash (lambda (key warrior)
               (let ((start-time (getf warrior :start-time)))
                 (when (and start-time (> (- now start-time) *stale-allocation-age*))
                   (push (list key warrior) stale-keys))))
             *warrior-allocation*)
    ;; Remove stale entries and alert
    (when stale-keys
      (dolist (entry stale-keys)
        (let* ((key (first entry))
               (warrior (second entry))
               (symbol (getf warrior :symbol))
               (category (getf warrior :category))
               (direction (getf warrior :direction))
               (age-hours (/ (- now (getf warrior :start-time)) 3600.0)))
          (remhash key *warrior-allocation*)
          (incf removed-count)
          (format t "[ALLOC] 🧹 STALE CLEANUP: ~a ~a ~a (age: ~,1fh)~%"
                  category direction symbol age-hours)))
      ;; Discord alert
      (when (fboundp 'notify-discord-alert)
        (notify-discord-alert
         (format nil "🧹 **ALLOCATION CLEANUP**~%~%~d stale warrior(s) removed (>4h old)~%~%Slots freed: ~{~a~^, ~}"
                 removed-count (mapcar #'first stale-keys))
         :color 16776960)))  ; Yellow
    
    ;; V19: Trigger MT5 sync check (Full Reconcile)
    (request-mt5-positions)
    
    removed-count))

;;; ==========================================
;;; MT5 POSITION RECONCILIATION (V19)
;;; ==========================================

(defun request-mt5-positions ()
  "Send GET_POSITIONS command to MT5"
  (when (boundp 'swimmy.core::*cmd-publisher*)
    (let ((msg (jsown:to-json (jsown:new-js ("action" "GET_POSITIONS")))))
      (pzmq:send swimmy.core::*cmd-publisher* msg))))

(defun reconcile-with-mt5-positions (symbol mt5-positions)
  "Compare warrior-allocation with actual MT5 positions for a symbol.
   If a warrior is allocated in Brain but has no corresponding position in MT5, free the slot."
  (let ((active-magics (make-hash-table :test 'eql))
        (ghost-warriors nil)
        (removed-count 0))
    
    ;; 1. Build set of active MT5 magics
    (dolist (pos mt5-positions)
      (let ((magic (if (jsown:keyp pos "magic") (jsown:val pos "magic") 0)))
        (setf (gethash magic active-magics) t)))
    
    ;; 2. Find ghost warriors (Brain has allocation, MT5 has no position)
    (maphash (lambda (key warrior)
               (let ((w-symbol (getf warrior :symbol))
                     (w-magic (getf warrior :magic)))
                 (when (and (equal w-symbol symbol)
                            w-magic
                            (not (gethash w-magic active-magics)))
                   ;; Double check: Ensure it's not a brand new trade (give 60s grace period)
                   (let ((start-time (getf warrior :start-time)))
                     (when (and start-time (> (- (get-universal-time) start-time) 60))
                       (push (list key w-magic (getf warrior :category)) ghost-warriors))))))
             *warrior-allocation*)
    
    ;; 3. Exorcise ghosts
    (when ghost-warriors
      (dolist (ghost ghost-warriors)
        (let ((key (first ghost))
              (magic (second ghost))
              (cat (third ghost)))
          (remhash key *warrior-allocation*)
          (incf removed-count)
          (format t "[ALLOC] 👻 GHOST BUSTED: ~a (Magic ~d) removed. Not in MT5.~%" key magic)))
      
      ;; Alert User
      (when (fboundp 'notify-discord-alert)
        (notify-discord-alert
         (format nil "👻 **GHOST POSITIONS REMOVED**~%Symbol: ~a~%Count: ~d~%Magics: ~{~a~^, ~}"
                 symbol removed-count (mapcar #'second ghost-warriors))
         :color 16776960))) ; Yellow
    
    (when (> removed-count 0)
      (format t "[ALLOC] Reconfigured ~d slots for ~a~%" removed-count symbol))))
