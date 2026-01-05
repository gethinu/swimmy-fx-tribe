;;; ============================================================================
;;; engine/metrics.lisp - Performance Tracking
;;; ============================================================================
;;; Performance records and statistics - pure data
;;; Part of "The Efficient Gardener" refactoring

(in-package :swimmy.engine)
;;; ============================================================================

;;; ==========================================
;;; PERFORMANCE RECORDS
;;; ==========================================

(defparameter *performance-log* nil)
(defparameter *performance-log-max* 1000)

(defstruct performance-record
  timestamp
  daily-pnl
  accumulated-pnl
  goal-progress
  trades-today
  win-rate
  sharpe-estimate)

(defun record-daily-performance ()
  "Record end-of-day performance for verification.
   Returns the created performance record."
  (let ((record (make-performance-record
                 :timestamp (get-universal-time)
                 :daily-pnl *daily-pnl*
                 :accumulated-pnl *accumulated-pnl*
                 :goal-progress (getf (get-goal-progress) :progress-pct)
                 :trades-today *total-trades*
                 :win-rate (float (/ *consecutive-wins*
                                     (max 1 (+ *consecutive-wins* *consecutive-losses*))))
                 :sharpe-estimate 0.0)))
    (push record *performance-log*)
    (when (> (length *performance-log*) *performance-log-max*)
      (setf *performance-log* (subseq *performance-log* 0 *performance-log-max*)))
    record))

(defun get-performance-stats ()
  "Get raw performance statistics.
   Returns property list with :avg-pnl, :best-day, :worst-day, :total-records."
  (if (null *performance-log*)
      (list :avg-pnl 0 :best-day 0 :worst-day 0 :total-records 0)
      (let* ((recent (subseq *performance-log* 0 (min 30 (length *performance-log*))))
             (pnls (mapcar #'performance-record-daily-pnl recent)))
        (list :avg-pnl (/ (reduce #'+ pnls) (max 1 (length pnls)))
              :best-day (apply #'max pnls)
              :worst-day (apply #'min pnls)
              :total-records (length *performance-log*)))))

(defun calculate-sharpe-ratio (&optional (risk-free-rate 0.0))
  "Calculate Sharpe ratio from performance log."
  (when (> (length *performance-log*) 5)
    (let* ((pnls (mapcar #'performance-record-daily-pnl
                         (subseq *performance-log* 0 (min 30 (length *performance-log*)))))
           (mean (/ (reduce #'+ pnls) (length pnls)))
           (sq-diffs (mapcar (lambda (p) (expt (- p mean) 2)) pnls))
           (std-dev (sqrt (/ (reduce #'+ sq-diffs) (max 1 (1- (length pnls)))))))
      (if (> std-dev 0)
          (/ (- mean risk-free-rate) std-dev)
          0))))

(format t "[ENGINE] metrics.lisp loaded~%")
