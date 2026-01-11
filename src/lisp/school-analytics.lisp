;;; school-analytics.lisp - Structured Performance Analysis
;;; Extracted from dreamer2.lisp

(in-package :swimmy.school)

;;; ==========================================
;;; SELF-ANALYSIS v2.0 (Structure Performance Metrics)
;;; ==========================================
;;; Features:
;;; - Structured performance metrics by category/session/regime
;;; - Strategy performance ranking
;;; - Actionable insights generation
;;; - JSON-formatted feedback for Gemini

(defun analyze-by-category ()
  "Analyze performance by trading category"
  (let ((stats (make-hash-table :test 'eq)))
    ;; Initialize categories
    (dolist (cat '(:trend :reversion :breakout :scalp))
      (setf (gethash cat stats) (list 0 0 0.0)))  ; wins losses pnl
    ;; Aggregate from logs
    (when (boundp '*success-log*)
      (dolist (record *success-log*)
        (let* ((cat (trade-record-category record))
               (s (gethash cat stats (list 0 0 0.0))))
          (incf (first s))
          (incf (third s) (or (trade-record-pnl record) 0))
          (setf (gethash cat stats) s))))
    (when (boundp '*failure-log*)
      (dolist (record *failure-log*)
        (let* ((cat (trade-record-category record))
               (s (gethash cat stats (list 0 0 0.0))))
          (incf (second s))
          (incf (third s) (or (trade-record-pnl record) 0))
          (setf (gethash cat stats) s))))
    stats))

(defun analyze-by-session ()
  "Analyze performance by trading session"
  (let ((stats (make-hash-table :test 'eq)))
    (dolist (session '(:tokyo :london :newyork :overlap :off))
      (setf (gethash session stats) (list 0 0 0.0)))
    (when (boundp '*success-log*)
      (dolist (record *success-log*)
        (let* ((sess (trade-record-session record))
               (s (gethash sess stats (list 0 0 0.0))))
          (when s
            (incf (first s))
            (incf (third s) (or (trade-record-pnl record) 0))
            (setf (gethash sess stats) s)))))
    (when (boundp '*failure-log*)
      (dolist (record *failure-log*)
        (let* ((sess (trade-record-session record))
               (s (gethash sess stats (list 0 0 0.0))))
          (when s
            (incf (second s))
            (incf (third s) (or (trade-record-pnl record) 0))
            (setf (gethash sess stats) s)))))
    stats))

(defun analyze-by-regime ()
  "Analyze performance by market regime"
  (let ((stats (make-hash-table :test 'eq)))
    (dolist (regime '(:trending :ranging :unknown))
      (setf (gethash regime stats) (list 0 0 0.0)))
    (when (boundp '*success-log*)
      (dolist (record *success-log*)
        (let* ((reg (or (trade-record-regime record) :unknown))
               (s (gethash reg stats (list 0 0 0.0))))
          (incf (first s))
          (incf (third s) (or (trade-record-pnl record) 0))
          (setf (gethash reg stats) s))))
    (when (boundp '*failure-log*)
      (dolist (record *failure-log*)
        (let* ((reg (or (trade-record-regime record) :unknown))
               (s (gethash reg stats (list 0 0 0.0))))
          (incf (second s))
          (incf (third s) (or (trade-record-pnl record) 0))
          (setf (gethash reg stats) s))))
    stats))

(defun format-stats-summary (stats-hash)
  "Format hash table stats into readable string"
  (let ((result nil))
    (maphash (lambda (key val)
               (let* ((wins (first val))
                      (losses (second val))
                      (pnl (third val))
                      (total (+ wins losses))
                      (win-rate (if (> total 0) (* 100 (/ wins total)) 0)))
                 (when (> total 0)
                   (push (format nil "~a: ~,0f% win (~d trades, ~,2f PnL)" 
                                 key win-rate total pnl) result))))
             stats-hash)
    (format nil "~{~a~^; ~}" (reverse result))))

(defun get-best-and-worst-conditions ()
  "Identify best and worst trading conditions"
  (let ((conditions nil))
    ;; Analyze by session
    (let ((session-stats (analyze-by-session)))
      (maphash (lambda (sess val)
                 (let* ((wins (first val)) (losses (second val))
                        (total (+ wins losses)))
                   (when (> total 3)
                     (push (cons sess (/ wins (max 1 total))) conditions))))
               session-stats))
    ;; Sort by win rate
    (let ((sorted (sort conditions #'> :key #'cdr)))
      (list :best (car (first sorted))
            :best-rate (cdr (first sorted))
            :worst (car (car (last sorted)))
            :worst-rate (cdr (car (last sorted)))))))

(defun generate-actionable-insights ()
  "Generate specific actionable insights from analysis"
  (let ((insights nil)
        (conditions (get-best-and-worst-conditions)))
    ;; Session insights
    (when (and (getf conditions :best) (> (getf conditions :best-rate) 0.6))
      (push (format nil "FOCUS on ~a session (~,0f% win rate)" 
                    (getf conditions :best) (* 100 (getf conditions :best-rate))) insights))
    (when (and (getf conditions :worst) (< (getf conditions :worst-rate) 0.4))
      (push (format nil "AVOID ~a session (~,0f% win rate)" 
                    (getf conditions :worst) (* 100 (getf conditions :worst-rate))) insights))
    ;; Regime insights from regime-performance if available
    (when (boundp '*regime-performance*)
      (maphash (lambda (key val)
                 (let* ((wins (first val)) (losses (second val))
                        (total (+ wins losses)))
                   (when (and (> total 5) (< (/ wins (max 1 total)) 0.35))
                     (push (format nil "REDUCE trading in ~a conditions" key) insights))))
               *regime-performance*))
    ;; Return as formatted string
    (if insights
        (format nil "~{- ~a~^~%~}" insights)
        "No strong patterns detected yet. Continue gathering data.")))

(defun get-top-strategies (n)
  "Get top N performing strategies by Sharpe ratio"
  (when *evolved-strategies*
    (let ((sorted (sort (copy-list *evolved-strategies*) #'> 
                        :key (lambda (s) (or (strategy-sharpe s) -999)))))
      (subseq sorted 0 (min n (length sorted))))))

(defun get-structured-self-analysis ()
  "Generate comprehensive structured analysis for Gemini"
  (let* ((category-stats (analyze-by-category))
         (session-stats (analyze-by-session))
         (regime-stats (analyze-by-regime))
         (top-strats (get-top-strategies 3))
         (insights (generate-actionable-insights)))
    (format nil "
=== SELF-ANALYSIS REPORT ===

PERFORMANCE BY CATEGORY:
~a

PERFORMANCE BY SESSION:
~a

PERFORMANCE BY REGIME:
~a

TOP STRATEGIES:
~{~a (Sharpe: ~,2f)~^, ~}

ACTIONABLE INSIGHTS:
~a

CURRENT MARKET: Regime=~a, Volatility=~a
"
            (format-stats-summary category-stats)
            (format-stats-summary session-stats)
            (format-stats-summary regime-stats)
            (if top-strats
                (loop for s in top-strats 
                      collect (strategy-name s)
                      collect (or (strategy-sharpe s) 0))
                (list "None" 0))
            insights
            *current-regime*
            *volatility-regime*)))
