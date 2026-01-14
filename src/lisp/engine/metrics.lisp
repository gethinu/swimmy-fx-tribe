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

(defun recalculate-portfolio-stats ()
  "Recalculate all portfolio metrics from restored trade logs.
   Updates: *accumulated-pnl*, *all-time-win-rate*, *max-drawdown*, *portfolio-sharpe*"
  (when (and (boundp 'swimmy.school::*success-log*)
             (boundp 'swimmy.school::*failure-log*))
    (let* ((successes swimmy.school::*success-log*)
           (failures swimmy.school::*failure-log*)
           (all-trades (append successes failures))
           (total-pnl 0.0)
           (wins 0)
           (losses 0)
           (peak-pnl (if (boundp 'swimmy.globals::*min-safe-capital*) 
                         swimmy.globals::*min-safe-capital* 
                         100000.0))
           (max-dd 0.0)
           (pnls nil))
      
      ;; 1. Sort by timestamp (asc) to simulate equity curve
      (setf all-trades (sort all-trades #'< :key #'swimmy.school::trade-record-timestamp))
      
      ;; 2. Replay history
      (dolist (trade all-trades)
          (incf total-pnl pnl)
          (push pnl pnls)
          (if (> pnl 0) (incf wins) (incf losses))
          
          ;; Track Drawdown (Proper reconstruction using System Base Capital)
          ;; Uses *min-safe-capital* (100k) as the baseline for history reconstruction
          (let* ((base-cap (if (boundp 'swimmy.globals::*min-safe-capital*) 
                               swimmy.globals::*min-safe-capital* 
                               100000.0))
                 (sim-equity (+ base-cap total-pnl)))
            (when (> sim-equity peak-pnl) (setf peak-pnl sim-equity))
            (let ((dd (if (> peak-pnl 0) 
                          (* 100 (/ (- peak-pnl sim-equity) peak-pnl))
                          0.0)))
              (when (> dd max-dd) (setf max-dd dd)))))
      
      ;; 3. Update Globals
      (when (> (length all-trades) 0)
        (setf *accumulated-pnl* total-pnl)
        (setf *all-time-win-rate* (float (* 100 (/ wins (max 1 (+ wins losses))))))
        (setf *max-drawdown* max-dd)
        
        ;; 4. Calculate Sharpe (using daily PnL approximation if detailed timestamps not strictly enforced)
        (let* ((n (length pnls))
               (mean (/ total-pnl (max 1 n)))
               (sq-diffs (reduce #'+ (mapcar (lambda (x) (expt (- x mean) 2)) pnls)))
               (std-dev (sqrt (/ sq-diffs (max 1 n)))))
          (if (> std-dev 0)
              (setf *portfolio-sharpe* (/ mean std-dev))
              (setf *portfolio-sharpe* 0.0)))
          
        (format t "[METRICS] 🔄 Recalculated Stats: PnL=~,0f Win=~,1f% DD=~,2f% Sharpe=~,2f~%"
                *accumulated-pnl* *all-time-win-rate* *max-drawdown* *portfolio-sharpe*)))))
