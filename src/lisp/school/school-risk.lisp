;;; school-risk.lisp - Risk Management & Correlation Analysis
;;; Part of the Swimmy School System
;;; Extracted from school.lisp to comply with SRP (Expert Panel 2026-01-13)

(in-package :swimmy.school)

;;; ==========================================
;;; PORTFOLIO CORRELATION RISK MANAGEMENT
;;; ==========================================

(defun get-correlation (sym1 sym2)
  "Get correlation between two symbols"
  (let ((pairs (cdr (assoc sym1 *pair-correlations* :test #'string=))))
    (if pairs
        (or (cdr (assoc sym2 pairs :test #'string=)) 0.0)
        0.0)))

(defun calculate-correlated-exposure (symbol)
  "Calculate total exposure considering correlations"
  (let ((total 0.0))
    (maphash (lambda (sym exp)
               (let ((corr (abs (get-correlation symbol sym))))
                 (incf total (* exp corr))))
             *symbol-exposure*)
    total))

(defun correlation-adjusted-lot (symbol base-lot)
  "Reduce lot size if highly correlated positions exist"
  (let* ((corr-exposure (calculate-correlated-exposure symbol))
         (symbol-exp (gethash symbol *symbol-exposure* 0.0))
         (headroom (- *max-symbol-exposure* symbol-exp))
         (corr-factor (max 0.3 (- 1.0 (* corr-exposure 0.5)))))  ; Min 30% of base lot
    (min headroom (* base-lot corr-factor))))

(defun get-symbol-exposure (symbol)
  "Return current exposure for a symbol."
  (gethash symbol *symbol-exposure* 0.0))

(defun update-symbol-exposure (symbol lot action)
  "Update exposure tracking on trade open/close"
  (if (eq action :open)
      (incf (gethash symbol *symbol-exposure* 0.0) lot)
      (decf (gethash symbol *symbol-exposure* 0.0) lot)))

(defun total-exposure-allowed-p ()
  "Check if total exposure is within limits"
  (let ((total 0.0))
    (maphash (lambda (sym exp) (declare (ignore sym)) (incf total exp)) *symbol-exposure*)
    (< total *max-total-exposure*)))

;;; ==========================================
;;; STRATEGY CORRELATION ANALYSIS (V4.0)
;;; ==========================================

(defparameter *strategy-signal-history* (make-hash-table :test 'equal))
(defparameter *strategy-correlation-cache* nil)

(defun record-strategy-signal (strategy-name direction timestamp &key (confidence 0.5) (reason "Signal"))
  "Record strategy signal for correlation analysis (V4.1: Added Confidence/Reason)"
  (let* ((key strategy-name)
         (history (gethash key *strategy-signal-history* nil))
         (signal (list timestamp direction confidence reason)))
    (push signal history)
    ;; Keep only last 100 signals
    (when (> (length history) 100)
      (setf history (subseq history 0 100)))
    (setf (gethash key *strategy-signal-history*) history)))

(defun calculate-signal-correlation (name1 name2)
  "Calculate correlation between two strategies' signals"
  (let ((h1 (gethash name1 *strategy-signal-history*))
        (h2 (gethash name2 *strategy-signal-history*)))
    (if (and h1 h2 (> (length h1) 10) (> (length h2) 10))
        (let ((matches 0)
              (comparisons 0))
          ;; Compare signals within same time windows
          (dolist (s1 h1)
            (let ((t1 (first s1))
                  (d1 (second s1)))
              (dolist (s2 h2)
                (let ((t2 (first s2))
                      (d2 (second s2)))
                  ;; Same minute window
                  (when (< (abs (- t1 t2)) 60)
                    (incf comparisons)
                    (when (eq d1 d2)
                      (incf matches)))))))
          (if (> comparisons 0)
              (float (/ matches comparisons))
              0.0))
        0.0)))

(defun analyze-strategy-correlation ()
  "Analyze all strategies for correlation and identify redundant ones"
  (let ((strategies (mapcar (lambda (s) (strategy-name s)) *strategy-knowledge-base*))
        (high-corr nil))
    (format t "~%[L] 🔬 V4.0: STRATEGY CORRELATION ANALYSIS~%")
    (format t "[L] Analyzing ~d strategies...~%" (length strategies))
    
    ;; Compare all pairs
    (loop for i from 0 below (length strategies)
          for s1 = (nth i strategies) do
          (loop for j from (1+ i) below (length strategies)
                for s2 = (nth j strategies) do
                (let ((corr (calculate-signal-correlation s1 s2)))
                  (when (> corr 0.85)
                    (push (list s1 s2 corr) high-corr)
                    (format t "[L] ⚠️ High correlation (~,0f%): ~a ↔ ~a~%"
                            (* 100 corr) s1 s2)))))
    
    (setf *strategy-correlation-cache* high-corr)
    
    (if high-corr
        (format t "[L] 📊 Found ~d highly correlated pairs~%" (length high-corr))
        (format t "[L] ✅ No highly correlated strategies found~%"))
    
    high-corr))

(defun get-redundant-strategies ()
  "Get list of strategies that might be redundant"
  (unless *strategy-correlation-cache*
    (analyze-strategy-correlation))
  (let ((redundant nil))
    (dolist (pair *strategy-correlation-cache*)
      (let ((s1 (first pair))
            (s2 (second pair)))
        ;; Keep the one with better name (shorter, more descriptive)
        (if (< (length s1) (length s2))
            (pushnew s2 redundant :test 'equal)
            (pushnew s1 redundant :test 'equal))))
    redundant))

(defun prune-redundant-strategies ()
  "Remove strategies with >85% correlation (Graham's simplicity)"
  (let ((redundant (get-redundant-strategies))
        (removed 0))
    (when redundant
      (format t "[L] 🧹 GRAHAM: Pruning ~d redundant strategies...~%" (length redundant))
      (dolist (name redundant)
        (let ((strat (find name *strategy-knowledge-base* 
                          :key #'strategy-name :test #'string=)))
          (when strat
            (setf *strategy-knowledge-base* 
                  (remove strat *strategy-knowledge-base*))
            (incf removed)
            (format t "[L]   ✂️ Removed: ~a~%" name))))
      (format t "[L] 🧹 GRAHAM: Removed ~d strategies. Remaining: ~d~%" 
              removed (length *strategy-knowledge-base*)))
    removed))

(defun count-strategies-by-type ()
  "Count strategies by indicator type for analysis"
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (strat *strategy-knowledge-base*)
      (let* ((indicators (strategy-indicators strat))
             (types (mapcar (lambda (ind) 
                             (if (listp ind) (car ind) ind)) 
                           indicators)))
        (dolist (type types)
          (incf (gethash (symbol-name type) counts 0)))))
    (format t "[L] 📊 Strategy Type Distribution:~%")
    (maphash (lambda (k v) (format t "[L]   ~a: ~d~%" k v)) counts)
    counts))
