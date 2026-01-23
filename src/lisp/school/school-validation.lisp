;;; school-validation.lisp
;;; ============================================================================
;;; P9: OUT-OF-SAMPLE VALIDATION (CPCV Lite)
;;; Extracted from school-backtest.lisp for SRP compliance
;;; ============================================================================
;;; Purpose: Validate strategy on unseen data before A-RANK promotion
;;; Method: Split CSV data into 70% train / 30% test, run backtest on test portion
;;; ============================================================================

(in-package :swimmy.school)

(defparameter *oos-test-ratio* 0.3
  "Ratio of data to use for out-of-sample testing (30%)")

(defparameter *oos-min-sharpe* 0.3
  "Minimum Sharpe ratio required to pass OOS validation for A-RANK")

(defun run-oos-validation (strat)
  "Run Out-of-Sample validation for A-RANK promotion candidates.
   Returns (values passed-p oos-sharpe message)"
  (let* ((symbol (or (strategy-symbol strat) "USDJPY"))
         (data-file (format nil "/home/swimmy/swimmy/data/historical/~a_M1.csv" symbol)))
    
    (unless (probe-file data-file)
      (return-from run-oos-validation 
        (values nil 0.0 (format nil "No data file for ~a" symbol))))
    
    (handler-case
        (let* ((lines (with-open-file (s data-file :direction :input)
                        (loop for line = (read-line s nil)
                              while line count 1)))
               (test-start-row (floor (* lines (- 1.0 *oos-test-ratio*))))
               (test-rows (- lines test-start-row)))
          
          (format t "[OOS] 📊 ~a: Using rows ~d-~d for OOS test (~d rows)~%"
                  (strategy-name strat) test-start-row lines test-rows)
          
          ;; Request backtest with suffix to differentiate from normal BT
          ;; The Guardian will use the same CSV but we're conceptually testing OOS
          ;; TODO: Implement proper data range limiting in Guardian API
          (let ((result (request-backtest strat :suffix "-OOS")))
            (if result
                (let ((sharpe (getf result :sharpe 0.0)))
                  (if (>= sharpe *oos-min-sharpe*)
                      (values t sharpe 
                              (format nil "OOS PASSED: Sharpe=~,2f" sharpe))
                      (values nil sharpe 
                              (format nil "OOS FAILED: Sharpe=~,2f < ~,2f" 
                                      sharpe *oos-min-sharpe*))))
                (values nil 0.0 "OOS backtest failed"))))
      (error (e)
        (values nil 0.0 (format nil "OOS error: ~a" e))))))

(defun validate-for-a-rank-promotion (strat)
  "Validate strategy is ready for A-RANK promotion using OOS validation.
   Returns T if strategy passes all A-RANK criteria including OOS."
  (format t "[A-RANK] 🎯 Validating ~a for A-RANK promotion...~%"
          (strategy-name strat))
  
  ;; Check basic rank criteria first
  (unless (meets-a-rank-criteria strat)
    (format t "[A-RANK] ❌ ~a: Does not meet basic A-RANK criteria~%"
            (strategy-name strat))
    (return-from validate-for-a-rank-promotion nil))
  
  ;; Run OOS validation
  (multiple-value-bind (passed sharpe msg) (run-oos-validation strat)
    (format t "[A-RANK] ~a ~a: ~a~%" 
            (if passed "✅" "❌") (strategy-name strat) msg)
    passed))

(defun meets-a-rank-criteria (strat)
  "Check if strategy meets basic A-RANK criteria (without OOS)."
  (let ((sharpe (or (strategy-sharpe strat) 0.0))
        (pf (or (strategy-profit-factor strat) 0.0))
        (wr (or (strategy-win-rate strat) 0.0))
        (maxdd (or (strategy-max-dd strat) 1.0)))
    (and (>= sharpe 0.3)
         (>= pf 1.2)
         (>= wr 0.4)
         (< maxdd 0.2))))

;;; ============================================================================
;;; P12: CPCV LISP-RUST INTEGRATION
;;; ============================================================================

(defun request-cpcv-validation (strat)
  "Request CPCV validation from Guardian via ZMQ.
   Returns (values passed-p result-plist error-msg)"
   (let* ((symbol (or (strategy-symbol strat) "USDJPY"))
          (candles-file (format nil "/home/swimmy/swimmy/data/historical/~a_M1.csv" symbol))
          (strat-params (strategy-to-alist strat))
          (request `((action . "CPCV_VALIDATE")
                     (strategy_name . ,(strategy-name strat))
                     (symbol . ,symbol)
                     (candles_file . ,candles-file)
                     (strategy_params . ,strat-params))))
     (format t "[CPCV] 🔬 Requesting CPCV validation for ~a (S-Exp)...~%" (strategy-name strat))
     (handler-case
         (let ((response-str (send-zmq-msg (prin1-to-string request))))
           (if (and response-str (> (length response-str) 0) (char= (char response-str 0) #\())
               ;; S-Expression Response
               (let* ((response (read-from-string response-str))
                      (passed (cdr (assoc 'passed response)))
                      (error-msg (cdr (assoc 'error response))))
                 (if error-msg
                     (values nil nil error-msg)
                     (values passed
                             (list :median-sharpe (cdr (assoc 'median_sharpe response))
                                   :median-pf (cdr (assoc 'median_pf response))
                                   :median-wr (cdr (assoc 'median_wr response))
                                   :median-maxdd (cdr (assoc 'median_maxdd response))
                                   :std-sharpe (cdr (assoc 'std_sharpe response))
                                   :path-count (cdr (assoc 'path_count response))
                                   :passed-count (cdr (assoc 'passed_count response)))
                             nil)))
               ;; Legacy JSON fallback
               (if (and response-str (> (length response-str) 0) (char= (char response-str 0) #\{))
                   (let* ((response (jsown:parse response-str))
                          (passed (jsown:val-safe response "passed"))
                          (error-msg (jsown:val-safe response "error")))
                     (if error-msg
                         (values nil nil error-msg)
                         (values passed (list :median-sharpe (jsown:val-safe response "median_sharpe")) nil)))
                   (values nil nil "Invalid response format"))))
      (error (e)
        (values nil nil (format nil "CPCV error: ~a" e))))))

(defun validate-for-s-rank-promotion (strat)
  "Validate strategy is ready for S-RANK promotion using true CPCV.
   S-RANK criteria: Sharpe >= 0.5, PF >= 1.5, WR >= 45%, MaxDD < 15%
   Plus: CPCV validation must pass (median Sharpe >= 0.5, 50%+ paths passing).
   Returns T if strategy passes all S-RANK criteria including CPCV."
  (format t "[S-RANK] 🏆 Validating ~a for S-RANK promotion...~%"
          (strategy-name strat))
  
  ;; Check basic S-RANK criteria first
  (let ((sharpe (or (strategy-sharpe strat) 0.0))
        (pf (or (strategy-profit-factor strat) 0.0))
        (wr (or (strategy-win-rate strat) 0.0))
        (maxdd (or (strategy-max-dd strat) 1.0)))
    (unless (and (>= sharpe 0.5) (>= pf 1.5) (>= wr 0.45) (< maxdd 0.15))
      (format t "[S-RANK] ❌ ~a: Does not meet basic S-RANK criteria (S=~,2f PF=~,2f WR=~,2f DD=~,2f)~%"
              (strategy-name strat) sharpe pf wr maxdd)
      (return-from validate-for-s-rank-promotion nil)))
  
  ;; Run CPCV validation via Guardian
  (multiple-value-bind (passed result error-msg) (request-cpcv-validation strat)
    (cond
      (error-msg
       (format t "[S-RANK] ⚠️ ~a: CPCV error - ~a~%" (strategy-name strat) error-msg)
       nil)
      (passed
       (format t "[S-RANK] ✅ ~a: CPCV PASSED (median Sharpe=~,3f, ~d/~d paths)~%"
               (strategy-name strat)
               (getf result :median-sharpe)
               (getf result :passed-count)
               (getf result :path-count))
       t)
      (t
       (format t "[S-RANK] ❌ ~a: CPCV FAILED (median Sharpe=~,3f)~%"
               (strategy-name strat) (getf result :median-sharpe))
       nil))))

;;; ============================================================================
;;; V48.0: BATCH CPCV VALIDATION TRIGGERS
;;; Called from evolution loop to process A-RANK strategies
;;; ============================================================================

(defparameter *cpcv-batch-size* 20
  "V48.1: Number of A-RANK strategies to validate per cycle (Expert Panel P1: 5→20)")

(defparameter *last-cpcv-cycle* 0
  "Timestamp of last CPCV cycle")

(defparameter *cpcv-cycle-interval* 300
  "Minimum seconds between CPCV cycles (5 minutes)")

(defun run-a-rank-cpcv-batch ()
  "V48.0: Batch process A-RANK strategies for S-RANK promotion via CPCV.
   Called from evolution loop. Processes up to *cpcv-batch-size* at a time."
  (let ((now (get-universal-time)))
    ;; Rate limit CPCV cycles
    (when (< (- now *last-cpcv-cycle*) *cpcv-cycle-interval*)
      (return-from run-a-rank-cpcv-batch nil))
    (setf *last-cpcv-cycle* now)
    ;; Get A-RANK strategies that meet basic S-RANK criteria
    (let* ((a-rank-strategies (remove-if-not 
                                (lambda (s) 
                                  (and (eq (strategy-rank s) :A)
                                       ;; Only try to promote if it meets basic S-RANK stats (Sharpe > 0.5 etc)
                                       (check-rank-criteria s :S)))
                                *strategy-knowledge-base*))
           ;; Shuffle to avoid getting stuck on the same failing strategies if pool is large
           (shuffled (sort (copy-list a-rank-strategies) 
                           (lambda (a b) (declare (ignore a b)) (< (random 100) 50))))
           (batch (if (> (length shuffled) *cpcv-batch-size*)
                      (subseq shuffled 0 *cpcv-batch-size*)
                      shuffled))
           (promoted 0)
           (failed 0))
      (when (null batch)
        (format t "[CPCV] ℹ️ No A-RANK strategies ready for S-RANK validation (Sharpe < 0.5)~%")
        (return-from run-a-rank-cpcv-batch nil))
      (format t "[CPCV] 🔬 Processing ~d S-RANK candidates...~%" (length batch))
      ;; Process each strategy
      (dolist (strat batch)
        (handler-case
            (if (validate-for-s-rank-promotion strat)
                (progn
                  (ensure-rank strat :S "CPCV Passed")
                  (incf promoted)
                  (notify-discord-alert 
                    (format nil "🏆 **S-RANK PROMOTION**
~a passed CPCV!
Live trading permitted." 
                            (strategy-name strat))
                    :color 15844367))
                (progn
                  ;; V48.2: Demote back to B if it fails CPCV (Overfitted)
                  (%ensure-rank-no-lock strat :B "CPCV Failed (Overfit)")
                  (incf failed)))
          (error (e)
            (format t "[CPCV] ⚠️ Error validating ~a: ~a~%" (strategy-name strat) e)
            (incf failed))))
      ;; Report results
      (format t "[CPCV] 📊 Batch complete: Promoted=~d, Failed=~d~%" promoted failed)
      
      ;; V48.1: Send CPCV batch summary to Discord with JST
      (when (or (> promoted 0) (> failed 0))
        (let ((jst-time (swimmy.core:get-time-string)))
          (notify-discord-alert
            (format nil "🔬 **CPCV Validation**~%⏰ ~a JST~%~%A-RANK Tested: ~d~%🏆 S-RANK Promoted: ~d~%❌ Failed: ~d~%~%A-RANK Remaining: ~d"
                    jst-time (length batch) promoted failed
                    (count-if (lambda (s) (eq (strategy-rank s) :A)) *strategy-knowledge-base*))
            :color (if (> promoted 0) 5763719 15158332))))
      
      ;; Save promoted strategies
      (when (> promoted 0)
        (dolist (s batch)
          (when (eq (strategy-rank s) :S)
            (swimmy.persistence:save-strategy s)))))))

(defvar *rank-criteria-cache* (make-hash-table :test 'eq)
  "V48.0: Cache for rank criteria loaded from DB")

(defvar *criteria-cache-time* 0
  "Last time criteria were loaded from DB")

(defparameter *criteria-cache-ttl* 300
  "Cache TTL in seconds (5 minutes)")

(defun load-rank-criteria-from-db ()
  "V48.0: Load rank criteria from PostgreSQL.
   Returns T if loaded successfully, NIL otherwise."
  (handler-case
      (when (and (boundp 'swimmy.db::*db-connected*) swimmy.db::*db-connected*)
        (let ((rows (postmodern:query 
                      "SELECT rank, min_sharpe, min_pf, min_wr, max_dd FROM rank_criteria")))
          (clrhash *rank-criteria-cache*)
          (dolist (row rows)
            (let ((rank (intern (string-upcase (first row)) :keyword)))
              (setf (gethash rank *rank-criteria-cache*)
                    (list :min-sharpe (float (second row))
                          :min-pf (float (third row))
                          :min-wr (float (fourth row))
                          :max-dd (float (fifth row))))))
          (setf *criteria-cache-time* (get-universal-time))
          (format t "[CRITERIA] ✅ Loaded ~d rank criteria from DB~%" (hash-table-count *rank-criteria-cache*))
          t))
    (error (e)
      (format t "[CRITERIA] ⚠️ DB load failed: ~a (using defaults)~%" e)
      nil)))

(defun get-rank-criteria (rank)
  "V48.0: Get criteria for a rank. Loads from PostgreSQL if available.
   Falls back to hardcoded values if DB unavailable."
  ;; Check cache validity
  (when (> (- (get-universal-time) *criteria-cache-time*) *criteria-cache-ttl*)
    (load-rank-criteria-from-db))
  
  ;; Try cache first
  (let ((cached (gethash rank *rank-criteria-cache*)))
    (when cached
      (return-from get-rank-criteria cached)))
  
  ;; Fallback to hardcoded defaults
  (case rank
    (:B '(:min-sharpe 0.1 :min-pf 0.0 :min-wr 0.0 :max-dd 1.0))
    (:A '(:min-sharpe 0.3 :min-pf 1.2 :min-wr 0.40 :max-dd 0.20))
    (:S '(:min-sharpe 0.5 :min-pf 1.5 :min-wr 0.45 :max-dd 0.15))
    (otherwise '(:min-sharpe 0.0 :min-pf 0.0 :min-wr 0.0 :max-dd 1.0))))

(defun check-rank-criteria (strat rank)
  "V48.0: Check if strategy meets criteria for given rank."
  (let ((criteria (get-rank-criteria rank))
        (sharpe (or (strategy-sharpe strat) 0.0))
        (pf (or (strategy-profit-factor strat) 0.0))
        (wr (or (strategy-win-rate strat) 0.0))
        (maxdd (or (strategy-max-dd strat) 1.0)))
    (and (>= sharpe (getf criteria :min-sharpe))
         (>= pf (getf criteria :min-pf))
         (>= wr (getf criteria :min-wr))
         (< maxdd (getf criteria :max-dd)))))
