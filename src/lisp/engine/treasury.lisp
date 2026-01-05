;;; ============================================================================
;;; engine/treasury.lisp - Profit Lock-In System (Graham's Warning)
;;; ============================================================================
;;; "The essence of investment management is the management of RISKS, 
;;;  not the management of returns." - Benjamin Graham
;;;
;;; This module implements a "cold storage" mechanism:
;;; When profits exceed a threshold, a portion is permanently locked away
;;; and never risked again.
;;; ============================================================================

(in-package :swimmy.engine)

;;; ==========================================
;;; TREASURY PARAMETERS
;;; ==========================================

(defparameter *locked-treasury* 0
  "Permanently locked profits that are never risked.")

(defparameter *treasury-threshold* 5000
  "PnL threshold to trigger treasury lock-in (in Yen).")

(defparameter *treasury-lock-ratio* 0.5
  "Ratio of excess profits to lock (0.5 = 50%).")

(defparameter *treasury-file-path*
  (merge-pathnames ".swimmy/treasury.sexp" (user-homedir-pathname))
  "Path to the treasury ledger file.")

(defparameter *base-treasury-threshold* 5000
  "Base threshold before volatility adjustment.")

;;; ==========================================
;;; DYNAMIC THRESHOLD (Graham's Improvement)
;;; ==========================================

(defun get-dynamic-treasury-threshold ()
  "Calculate treasury threshold based on current volatility.
   High volatility = lower threshold (lock profits earlier).
   Low volatility = higher threshold (let profits compound)."
  (let ((volatility-factor 
         (cond
           ;; High volatility: lock early (threshold * 0.5)
           ((and (boundp '*current-volatility-state*)
                 (eq *current-volatility-state* :extreme))
            0.5)
           ;; Medium volatility: normal threshold
           ((and (boundp '*current-volatility-state*)
                 (eq *current-volatility-state* :high))
            0.75)
           ;; Low volatility: let it compound (threshold * 1.5)
           ((and (boundp '*current-volatility-state*)
                 (eq *current-volatility-state* :low))
            1.5)
           ;; Default: use base threshold
           (t 1.0))))
    (floor (* *base-treasury-threshold* volatility-factor))))

;;; ==========================================
;;; TREASURY LOGIC
;;; ==========================================

(defun check-treasury-threshold ()
  "Check if profits exceed threshold and lock a portion if so.
   Returns T if a lock-in occurred, NIL otherwise.
   Uses DYNAMIC threshold based on volatility (Graham's Improvement)."
  (let ((threshold (get-dynamic-treasury-threshold)))
    (when (and (boundp '*accumulated-pnl*)
               (> *accumulated-pnl* threshold))
      (let* ((excess (- *accumulated-pnl* threshold))
             (to-lock (floor (* excess *treasury-lock-ratio*))))
        (when (> to-lock 0)
          ;; Lock the profits
          (incf *locked-treasury* to-lock)
          ;; Reduce available PnL (it's now "cold storage")
          (decf *accumulated-pnl* to-lock)
          ;; Save to disk
          (save-treasury)
          ;; Notify
          (format t "[TREASURY] 🏦 PROFIT LOCKED: ¥~:d (threshold: ¥~:d)~%" to-lock threshold)
          (format t "[TREASURY] 💰 Total locked: ¥~:d | Remaining: ¥~:d~%" 
                  *locked-treasury* *accumulated-pnl*)
          ;; Notify Discord if available
          (when (fboundp 'notify-discord-alert)
            (notify-discord-alert 
             (format nil "🏦 TREASURY: ¥~:d locked. Total cold storage: ¥~:d" 
                     to-lock *locked-treasury*)
             :color 65280)) ; Green
          t)))))

(defun get-total-equity ()
  "Return total equity: locked treasury + available PnL."
  (+ *locked-treasury* 
     (if (boundp '*accumulated-pnl*) *accumulated-pnl* 0)))

;;; ==========================================
;;; TREASURY PERSISTENCE
;;; ==========================================

(defun save-treasury ()
  "Save treasury state to disk."
  (handler-case
      (progn
        (ensure-directories-exist *treasury-file-path*)
        (with-open-file (out *treasury-file-path*
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
          (format out "~s~%" (list :locked-treasury *locked-treasury*
                                    :timestamp (get-universal-time)))))
    (error (e)
      (format t "[TREASURY] ❌ Save failed: ~a~%" e))))

(defun load-treasury ()
  "Load treasury state from disk if it exists."
  (when (probe-file *treasury-file-path*)
    (handler-case
        (with-open-file (in *treasury-file-path* :direction :input)
          (let ((data (read in nil nil)))
            (when data
              (setf *locked-treasury* (or (getf data :locked-treasury) 0))
              (format t "[TREASURY] 📂 Loaded: ¥~:d in cold storage~%" *locked-treasury*))))
      (error (e)
        (format t "[TREASURY] ⚠️ Load failed: ~a~%" e)))))

(format t "[ENGINE] treasury.lisp loaded~%")
