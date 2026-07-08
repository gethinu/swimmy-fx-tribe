(in-package :cl-user)
(require :asdf)

;; Quiet startup and fail fast
(sb-ext:disable-debugger)
(setf *debugger-hook*
      (lambda (c h)
        (declare (ignore h))
        (format *error-output* "[LEGENDS-61] 💥 Fatal error: ~a~%" c)
        (sb-ext:exit :code 1)))

;;; ---------------------------------------------------------------------------
;;; P2c: IDEMPOTENT STAMP GATE (Thread A / rank sanitization)
;;; ---------------------------------------------------------------------------
;;; This script runs as the school daemon's boot Pre-step (install_tasks.ps1).
;;; Previously it fully re-registered the 61 legends AND re-flagged all of them
;;; for revalidation on EVERY boot — an expensive full `asdf:load-system :swimmy`
;;; plus churn that immortalized legends regardless of quality.
;;;
;;; Now restoration is gated on a stamp keyed to the legend SOURCE signature
;;; (strategies_v3.lisp version+size + a manual STAMP-VERSION). On a normal boot
;;; where the stamp already matches, we skip the whole system load and exit 0.
;;; Restoration re-runs only when:
;;;   - the stamp is missing (first boot / DB dir wiped — stamp is co-located
;;;     with the DB under data/memory/), or
;;;   - the legend source changed (signature mismatch), or
;;;   - SWIMMY_FORCE_LEGEND_RESTORE is set (manual override).
;;; The stamp is written only AFTER a successful restore, so a failed restore
;;; (debugger-hook exit 1) is retried on the next boot.

(defparameter *legend-restore-stamp-version* 1
  "Bump when the restore logic or *legend-61-names* set changes so the gate re-runs.")

(defun %legend-src-path ()
  (merge-pathnames "strategies_v3.lisp" (uiop:getcwd)))

(defun %legend-stamp-path ()
  ;; Co-located with the DB (data/memory/) so wiping the DB dir also drops the stamp.
  (merge-pathnames "data/memory/.legend_61_restore.stamp" (uiop:getcwd)))

(defun %legend-restore-signature ()
  "Signature of the legend source (nil if source unreadable -> force restore).
   Uses write-date + byte length rather than a full hash to stay cheap and
   dependency-free at pre-load time."
  (let ((src (%legend-src-path)))
    (when (probe-file src)
      (let ((wd (ignore-errors (file-write-date src)))
            (len (ignore-errors
                   (with-open-file (s src :element-type '(unsigned-byte 8))
                     (file-length s)))))
        (when (and wd len)
          (format nil "v~a|wd~a|len~a" *legend-restore-stamp-version* wd len))))))

(defun %legend-force-restore-p ()
  (let ((v (ignore-errors (uiop:getenv "SWIMMY_FORCE_LEGEND_RESTORE"))))
    (and (stringp v) (> (length (string-trim '(#\Space #\Tab #\Newline #\Return) v)) 0))))

(defun %legend-stamp-matches-p (sig)
  "T when the stamp file exists and equals SIG."
  (handler-case
      (let ((path (%legend-stamp-path)))
        (and sig
             (probe-file path)
             (string= (string-trim '(#\Space #\Tab #\Newline #\Return)
                                   (uiop:read-file-string path))
                      sig)))
    (error () nil)))

(defun %write-legend-stamp (sig)
  (handler-case
      (when sig
        (ensure-directories-exist (%legend-stamp-path))
        (with-open-file (s (%legend-stamp-path)
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
          (write-string sig s)
          (terpri s))
        t)
    (error (e)
      (format *error-output* "[LEGENDS-61] ⚠️ Failed to write stamp: ~a~%" e)
      nil)))

;; Gate: skip the expensive restore when the stamp already matches (unless forced).
(let ((sig (%legend-restore-signature)))
  (when (and (not (%legend-force-restore-p))
             (%legend-stamp-matches-p sig))
    (format t "[LEGENDS-61] ✅ Stamp current (~a) — skipping restore (idempotent).~%" sig)
    (sb-ext:exit :code 0))

  ;; --- Full restore path ---
  ;; Ensure project root is on ASDF path
  (push (uiop:getcwd) asdf:*central-registry*)
  (let ((ql-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file ql-init) (load ql-init)))

  (format t "[LEGENDS-61] Loading Swimmy system for restoration...~%")
  (handler-bind ((style-warning #'muffle-warning))
    (asdf:load-system :swimmy))

  (let ((*package* (find-package :swimmy.school)))
    (format t "[LEGENDS-61] Restoring 61 legends...~%")
    (when (fboundp 'restore-legend-61)
      (restore-legend-61))
    ;; Flag-only queue to avoid premature BACKTEST dispatch before daemon wiring
    (when (fboundp 'queue-legend-revalidation)
      (queue-legend-revalidation :send-requests nil)))

  ;; Persist the stamp only after a successful restore.
  (%write-legend-stamp sig)
  (format t "[LEGENDS-61] Done (stamp=~a).~%" sig))

(sb-ext:exit :code 0)
