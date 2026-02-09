;;; school-dalio.lisp - Uncorrelated Return Streams & Risk Parity
;;; Phase 27: The Holy Grail (Ray Dalio)
;;;
;;; "The Holy Grail of investing is having 15 or more good, uncorrelated return streams."
;;; - Ray Dalio
;;;
;;; This module complements `school-macro.lisp` and `school-kb.lisp`.
;;; While school-kb checks LOGIC similarity (Simons), this module checks OUTCOME correlation.
;;; It ensures that the portfolio is not exposed to a single risk factor.

(defpackage :swimmy.school.dalio
  (:use :cl :swimmy.school :swimmy.globals)
  (:export #:calculate-holy-grail-score
           #:is-uncorrelated-p
           #:get-correlation-matrix
           #:optimize-risk-parity))

(in-package :swimmy.school.dalio)

;;; ============================================================================
;;; STATE
;;; ============================================================================

(defparameter *correlation-threshold* 0.2 "Dalio's threshold for 'Uncorrelated'.")
(defparameter *correlation-cache* (make-hash-table :test 'equal))

;;; ============================================================================
;;; OUTCOME CORRELATION (PnL)
;;; ============================================================================

(defun get-returns-vector (strategy &optional (period 30))
  "Extract last N daily PnL values for the strategy.
   Returns a vector of floats."
  (declare (ignore strategy))
  ;; Placeholder: Requires trade history access from `school-analytics`
  ;; Aggregate trades by day... 
  ;; For now, returning a mock random vector for architectural verification
  (make-array period :initial-element (random 1.0)))

(defun pearson-correlation (v1 v2)
  "Calculate Pearson correlation coefficient between two vectors."
  (let* ((n (length v1))
         (mean1 (/ (reduce #'+ v1) n))
         (mean2 (/ (reduce #'+ v2) n))
         (num 0.0)
         (den1 0.0)
         (den2 0.0))
    (dotimes (i n)
      (let ((d1 (- (aref v1 i) mean1))
            (d2 (- (aref v2 i) mean2)))
        (incf num (* d1 d2))
        (incf den1 (* d1 d1))
        (incf den2 (* d2 d2))))
    (if (or (zerop den1) (zerop den2))
        0.0
        (/ num (sqrt (* den1 den2))))))

(defun calculate-holy-grail-score (portfolio)
  "Calculate the 'Holy Grail Score' of a portfolio.
   Score = Ratio of uncorrelated pairs (< 0.2) to total pairs.
   Target: > 0.8"
  (let ((strategies (if (listp portfolio) portfolio (list portfolio)))
        (count 0)
        (uncorrelated 0))
    (let ((n (length strategies)))
      (when (< n 2) (return-from calculate-holy-grail-score 1.0))
      
      (dotimes (i n)
        (dotimes (j i)
          (let* ((s1 (nth i strategies))
                 (s2 (nth j strategies))
                 (corr (calculate-strategy-correlation s1 s2)))
            (incf count)
            (when (< (abs corr) *correlation-threshold*)
              (incf uncorrelated))))))
    
    (if (zerop count) 1.0
        (/ (float uncorrelated) count))))

(defun calculate-strategy-correlation (s1 s2)
  "Calculate PnL correlation between two strategies."
  (let ((v1 (get-returns-vector s1))
        (v2 (get-returns-vector s2)))
    (pearson-correlation v1 v2)))

(defun is-uncorrelated-p (strategy portfolio &optional (threshold 0.2))
  "Check if a new strategy is uncorrelated with the existing portfolio."
  (dolist (s portfolio)
    (let ((corr (calculate-strategy-correlation strategy s)))
      (when (> (abs corr) threshold)
        ;; If highly correlated, check if it improves Sharpe (Highlander Rule applied to Risk)
        (return-from is-uncorrelated-p (values nil s corr)))))
  (values t nil 0.0))

(defun optimize-risk-parity (portfolio)
  "Adjust weights to equalize risk contribution (Dalio All Weather).
   Inverse volatility weighting."
  ;; To be implemented
  portfolio)

;;; ============================================================================
;;; DAILY PNL CORRELATION (Promotion Notifications)
;;; ============================================================================

(in-package :swimmy.school)

(defun %strategy-name (s)
  (cond
    ((stringp s) s)
    ((and (fboundp 'strategy-name) (ignore-errors (strategy-name s))) (strategy-name s))
    (t (format nil "~a" s))))

(defun %daily-pnl-map (strategy &key (days 30))
  (let* ((name (%strategy-name strategy))
         (rows (fetch-strategy-daily-pnl name :limit days))
         (table (make-hash-table :test 'equal)))
    (dolist (row rows)
      (setf (gethash (first row) table) (second row)))
    (values table (length rows))))

(defun aligned-daily-pnl-vectors (s1 s2 &key (days 30))
  (multiple-value-bind (map1 count1) (%daily-pnl-map s1 :days days)
    (multiple-value-bind (map2 count2) (%daily-pnl-map s2 :days days)
      (let ((v1 nil)
            (v2 nil))
        (maphash (lambda (k v)
                   (let ((v2val (gethash k map2 :missing)))
                     (unless (eq v2val :missing)
                       (push v v1)
                       (push v2val v2))))
                 map1)
        (let ((overlap (length v1)))
          (if (and (>= count1 days) (>= count2 days) (>= overlap days))
              (values (coerce (nreverse v1) 'vector)
                      (coerce (nreverse v2) 'vector)
                      overlap)
              (values nil nil overlap)))))))

(defun calculate-daily-pnl-correlation (s1 s2 &key (days 30))
  "Pearson correlation on aligned daily pnl vectors. Returns NIL if insufficient data."
  (multiple-value-bind (v1 v2 overlap) (aligned-daily-pnl-vectors s1 s2 :days days)
    (if (and v1 v2)
        (values (swimmy.school.dalio::pearson-correlation v1 v2) overlap)
        (values nil overlap))))

(defun calculate-noncorrelation-score (strategies &key (days 30)
                                                  (threshold swimmy.school.dalio::*correlation-threshold*))
  "Return (values score reason coverage-days). Score = uncorrelated_pairs / total_pairs."
  (let ((list (if (listp strategies) strategies (list strategies)))
        (min-overlap nil))
    (when (< (length list) 2)
      (return-from calculate-noncorrelation-score (values nil :insufficient-strategies 0)))
    (let ((count 0)
          (uncorrelated 0))
      (dotimes (i (length list))
        (dotimes (j i)
          (let* ((s1 (nth i list))
                 (s2 (nth j list)))
            (multiple-value-bind (corr overlap)
                (calculate-daily-pnl-correlation s1 s2 :days days)
              (when (or (null min-overlap) (< overlap min-overlap))
                (setf min-overlap overlap))
              (unless corr
                (return-from calculate-noncorrelation-score
                  (values nil :insufficient-data min-overlap)))
              (incf count)
              (when (< (abs corr) threshold)
                (incf uncorrelated))))))
      (if (zerop count)
          (values nil :insufficient-strategies (or min-overlap 0))
          (values (/ (float uncorrelated) count) nil min-overlap)))))

(defun %promotion-p (old-rank new-rank)
  (or (and (eq new-rank :A)
           (not (member old-rank '(:A :S :LEGEND) :test #'eq)))
      (and (eq new-rank :S) (eq old-rank :A))))

(defun %current-portfolio-strategies ()
  (remove-if-not
   (lambda (s)
     (member (strategy-rank s) '(:A :S :LEGEND) :test #'eq))
   *strategy-knowledge-base*))

(defun notify-noncorrelated-promotion (strategy new-rank &key (days 30) promotion-reason)
  "Compute noncorrelation score and send Discord notification."
  (let* ((portfolio (%current-portfolio-strategies))
         (score nil)
         (nc-reason nil)
         (coverage-days nil)
         (threshold swimmy.school.dalio::*correlation-threshold*))
    (multiple-value-setq (score nc-reason coverage-days)
      (calculate-noncorrelation-score portfolio :days days :threshold threshold))
    (let* ((title "⚖️ 非相関 昇格通知")
           (portfolio-size (length portfolio))
           (ts (swimmy.core:get-jst-timestamp))
           (score-text (if score (format nil "~,2f" score) "N/A"))
           (reason-text (cond
                          ((eq nc-reason :insufficient-data)
                           (if (numberp coverage-days)
                               (format nil "(データ不足: ~d/~d日)" coverage-days days)
                               "(データ不足)"))
                          ((eq nc-reason :insufficient-strategies) "(戦略数不足)")
                          (t "")))
           (promotion-reason-text (if promotion-reason (format nil "【理由】**~a**~%" promotion-reason) ""))
           (msg (format nil "~a戦略: **~a**~%昇格: **~a**~%非相関スコア: **~a** ~a~%閾値: |corr| < ~,2f~%ポートフォリオ: ~d 戦略~%時刻: ~a"
                        promotion-reason-text
                        (strategy-name strategy)
                        new-rank
                        score-text
                        reason-text
                        threshold
                        portfolio-size
                        ts))
           (webhook (or swimmy.core:*discord-daily-webhook*
                        swimmy.globals:*discord-webhook-url*))
           (telemetry-data (list :strategy (strategy-name strategy)
                                 :new-rank new-rank
                                 :score (if score score "N/A")
                                 :reason nc-reason
                                 :promotion-reason promotion-reason
                                 :coverage-days coverage-days
                                 :required-days days
                                 :portfolio-size portfolio-size)))
      (swimmy.core::emit-telemetry-event "noncorrelation.score"
        :service "school"
        :severity "info"
        :correlation-id (strategy-name strategy)
        :data telemetry-data)
      (swimmy.core:log-info msg)
      (when webhook
        (swimmy.core:queue-discord-notification
         webhook msg :color swimmy.core:+color-info+ :title title)))))

(in-package :swimmy.school.dalio)

(format t "[DALIO] 🌐 school-dalio.lisp loaded - Seeking the Holy Grail (Uncorrelated Alpha)~%")
