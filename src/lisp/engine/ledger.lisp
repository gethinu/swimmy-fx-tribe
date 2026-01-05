;;; ============================================================================
;;; engine/ledger.lisp - State Persistence (Hickey's Warning)
;;; ============================================================================
;;; Saves and loads system state to survive restarts.
;;; "A system that forgets its positions is Stupid, not Simple." - Rich Hickey
;;; ============================================================================

(in-package :swimmy.engine)

(defparameter *state-file-path* 
  (merge-pathnames ".swimmy/state.sexp" (user-homedir-pathname))
  "Path to the persistent state file.")

;;; ==========================================
;;; SAVE STATE
;;; ==========================================

(defun ensure-state-directory ()
  "Ensure the .swimmy directory exists."
  (let ((dir (make-pathname :directory (pathname-directory *state-file-path*))))
    (ensure-directories-exist dir)))

(defun arm-to-plist (arm idx)
  "Convert an arm and its state to a property list for serialization."
  (let ((state (gethash idx *arm-states*)))
    (list :index idx
          :params (car arm)
          :wins (cadr arm)
          :losses (cddr arm)
          :state state)))

(defun save-state ()
  "Save current system state to disk atomically."
  (ensure-state-directory)
  (let ((tmp-path (merge-pathnames ".state.tmp" *state-file-path*)))
    (handler-case
        (progn
          (with-open-file (out tmp-path 
                               :direction :output 
                               :if-exists :supersede
                               :if-does-not-exist :create)
            (let* ((arms-data (loop for arm in *arms*
                                    for idx from 0
                                    collect (arm-to-plist arm idx)))
                   (state-obj (list :version "1.0"
                                    :timestamp (get-universal-time)
                                    :arms arms-data
                                    :portfolio-indices *portfolio-indices*
                                    :daily-pnl (if (boundp '*daily-pnl*) *daily-pnl* 0)
                                    :accumulated-pnl (if (boundp '*accumulated-pnl*) *accumulated-pnl* 0)
                                    ;; V7.1 Persistence (Andrew Ng)
                                    :failure-log (if (boundp '*failure-log*) *failure-log* nil)
                                    :success-log (if (boundp '*success-log*) *success-log* nil)
                                    :evolved-strategies (if (boundp '*evolved-strategies*) *evolved-strategies* nil))))
              ;; Simple JSON-like output (Lisp S-expression for now)
              (format out "~s~%" state-obj)))
          ;; Atomic rename to prevent corruption
          (rename-file tmp-path *state-file-path*)
          (format t "[LEDGER] 💾 State saved atomically to ~a~%" *state-file-path*))
      (error (e)
        (format t "[LEDGER] ❌ Save failed: ~a~%" e)))))

;;; ==========================================
;;; LOAD STATE
;;; ==========================================

(defun plist-to-arm (plist)
  "Convert a property list back to an arm structure."
  (let ((params (getf plist :params))
        (wins (getf plist :wins))
        (losses (getf plist :losses)))
    (cons params (cons wins losses))))

(defun load-state ()
  "Load system state from disk if it exists."
  (when (probe-file *state-file-path*)
    (handler-case
        (with-open-file (in *state-file-path* :direction :input)
          (let ((state-obj (read in nil nil)))
            (when state-obj
              (let ((arms-data (getf state-obj :arms))
                    (portfolio (getf state-obj :portfolio-indices))
                    (daily (getf state-obj :daily-pnl))
                    (acc (getf state-obj :accumulated-pnl))
                    (fails (getf state-obj :failure-log))
                    (succs (getf state-obj :success-log))
                    (strats (getf state-obj :evolved-strategies)))
                ;; Restore arms
                (setf *arms* (mapcar #'plist-to-arm arms-data))
                ;; Restore arm-states hash table
                (clrhash *arm-states*)
                (dolist (plist arms-data)
                  (let ((idx (getf plist :index))
                        (state (getf plist :state)))
                    (when state
                      (setf (gethash idx *arm-states*) state))))
                ;; Restore portfolio
                (when portfolio
                  (setf *portfolio-indices* portfolio))
                ;; Restore PnL
                (when (boundp '*daily-pnl*)
                  (setf *daily-pnl* (or daily 0)))
                (when (boundp '*accumulated-pnl*)
                  (setf *accumulated-pnl* (or acc 0)))
                
                ;; V7.1 Restore Learning Data
                (when (boundp '*failure-log*)
                  (setf *failure-log* fails))
                (when (boundp '*success-log*)
                  (setf *success-log* succs))
                (when (and strats (boundp '*evolved-strategies*))
                  (setf *evolved-strategies* strats))
                
                (format t "[LEDGER] 📂 State restored: ~d arms, portfolio ~a, ~d strategies~%" 
                        (length *arms*) *portfolio-indices* (length (or strats nil)))))))
      (error (e)
        (format t "[LEDGER] ⚠️ Load failed: ~a~%" e)))))

(format t "[ENGINE] ledger.lisp loaded~%")
