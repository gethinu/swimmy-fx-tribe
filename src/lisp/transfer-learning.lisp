;;; ============================================================================
;;; transfer-learning.lisp - Naval Advisor Homework #3: Cross-Asset Transfer Learning
;;; ============================================================================
;;;
;;; PURPOSE:
;;;   Transfer learned patterns from one currency pair to another.
;;;   USDJPY insights can accelerate learning on EURUSD, GBPUSD, etc.
;;;
;;; KEY CONCEPTS:
;;;   - Pattern: Learned market behavior (trend-continuation, mean-reversion, etc.)
;;;   - Similarity: Statistical + Structural (FFT) + Event-Response
;;;   - Threshold: Naval requires >= 0.9 for strict quality transfer
;;;
;;; MAIN FUNCTIONS:
;;;   (extract-learned-patterns symbol)
;;;     → Extract patterns from trading history
;;;   (apply-pattern-to-symbol pattern target-symbol)
;;;     → Transfer pattern with adjusted parameters
;;;   (enhanced-transfer-similarity symbol1 symbol2)
;;;     → Combined similarity using 3 methods
;;;
;;; NAVAL IMPROVEMENTS:
;;;   - FFT-based structural similarity
;;;   - Event response correlation
;;;   - Strict 0.9 threshold for transfers
;;;
;;; VERSION: 2.0 (2026-01-02)
;;; ============================================================================

;;; ==========================================
;;; TRANSFER LEARNING STATE
;;; ==========================================

(defparameter *learned-patterns* (make-hash-table :test 'equal)
  "Hash: symbol -> list of learned patterns")
(defparameter *transfer-success-rates* (make-hash-table :test 'equal)
  "Hash: (source . target) -> (successful-transfers total-transfers)")
(defparameter *pattern-similarity-threshold* 0.9
  "Naval: Minimum similarity to consider assets related - STRICT (was 0.7)")
(defparameter *structural-similarity-threshold* 0.9
  "Naval: FFT-based structural similarity threshold - STRICT")

;;; ==========================================
;;; PATTERN STRUCTURES
;;; ==========================================

(defstruct learned-pattern
  source-symbol        ;; Where pattern was learned (e.g., "USDJPY")
  pattern-type         ;; :trend-continuation, :reversal, :breakout, etc.
  conditions           ;; Plist of market conditions when pattern works
  success-rate         ;; How often this pattern succeeds
  sample-count         ;; Number of observations
  parameters           ;; Optimal parameters for this pattern
  created-time)

;;; ==========================================
;;; PATTERN EXTRACTION
;;; ==========================================

(defun extract-learned-patterns (symbol)
  "Extract learned patterns from a symbol's trading history.
   Returns list of learned-pattern structs."
  (let ((patterns nil)
        (history (gethash symbol *candle-histories*)))
    (when (and history (> (length history) 100))
      ;; Pattern 1: Trend continuation after pullback
      (let* ((wins 0) (total 0) (params nil))
        (maphash
         (lambda (key stats)
           (when (and (eq (car key) :trending)
                      (string= symbol (cdr key)))
             (incf wins (first stats))
             (incf total (+ (first stats) (second stats)))))
         *regime-strategy-matrix*)
        (when (> total 5)
          (push (make-learned-pattern
                 :source-symbol symbol
                 :pattern-type :trend-continuation
                 :conditions (list :regime :trending :volatility :normal)
                 :success-rate (if (> total 0) (/ wins (float total)) 0.5)
                 :sample-count total
                 :parameters (list :sl-multiplier 1.5 :tp-multiplier 2.0)
                 :created-time (get-universal-time))
                patterns)))
      
      ;; Pattern 2: Mean reversion in ranging markets
      (let* ((wins 0) (total 0))
        (maphash
         (lambda (key stats)
           (when (and (eq (car key) :ranging)
                      (string= symbol (cdr key)))
             (incf wins (first stats))
             (incf total (+ (first stats) (second stats)))))
         *regime-strategy-matrix*)
        (when (> total 5)
          (push (make-learned-pattern
                 :source-symbol symbol
                 :pattern-type :mean-reversion
                 :conditions (list :regime :ranging :volatility :low)
                 :success-rate (if (> total 0) (/ wins (float total)) 0.5)
                 :sample-count total
                 :parameters (list :sl-multiplier 1.0 :tp-multiplier 1.5)
                 :created-time (get-universal-time))
                patterns)))
      
      ;; Store patterns
      (setf (gethash symbol *learned-patterns*) patterns)
      (format t "[T] 📚 Extracted ~d patterns from ~a~%" (length patterns) symbol))
    
    patterns))

;;; ==========================================
;;; ASSET SIMILARITY
;;; ==========================================

(defun calculate-asset-similarity (symbol1 symbol2)
  "Calculate similarity between two currency pairs.
   Based on: currency overlap, correlation, volatility profile."
  (let ((score 0.0))
    ;; Currency overlap score
    (let* ((currencies1 (list (subseq symbol1 0 3) (subseq symbol1 3 6)))
           (currencies2 (list (subseq symbol2 0 3) (subseq symbol2 3 6)))
           (overlap (intersection currencies1 currencies2 :test #'string=)))
      (incf score (* 0.4 (/ (length overlap) 2.0))))
    
    ;; Dynamic correlation if available
    (when (fboundp 'get-dynamic-correlation)
      (let ((corr (abs (get-dynamic-correlation symbol1 symbol2))))
        (incf score (* 0.4 corr))))
    
    ;; Volatility similarity (if both have ATR data)
    (let ((history1 (gethash symbol1 *candle-histories*))
          (history2 (gethash symbol2 *candle-histories*)))
      (when (and history1 history2 
                 (> (length history1) 20) 
                 (> (length history2) 20))
        (let ((atr1 (calculate-atr history1 14))
              (atr2 (calculate-atr history2 14)))
          (when (and atr1 atr2 (> (max atr1 atr2) 0))
            (let ((ratio (/ (min atr1 atr2) (max atr1 atr2))))
              (incf score (* 0.2 ratio)))))))
    
    score))

(defun find-similar-assets (symbol)
  "Find assets similar to given symbol for transfer learning"
  (let ((similar nil)
        (candidates (if (boundp '*supported-symbols*) 
                        *supported-symbols* 
                        '("USDJPY" "EURUSD" "GBPUSD"))))
    (dolist (candidate candidates)
      (unless (string= candidate symbol)
        (let ((similarity (calculate-asset-similarity symbol candidate)))
          (when (>= similarity *pattern-similarity-threshold*)
            (push (list :symbol candidate :similarity similarity) similar)))))
    (sort similar #'> :key (lambda (s) (getf s :similarity)))))

;;; ==========================================
;;; PATTERN TRANSFER
;;; ==========================================

(defun can-transfer-pattern-p (pattern target-symbol)
  "Check if a pattern can be transferred to target symbol.
   Naval: Now includes STRICT FFT structural similarity check (0.9+)"
  (let* ((source (learned-pattern-source-symbol pattern))
         (stat-similarity (calculate-asset-similarity source target-symbol))
         (struct-similarity (or (get-structural-similarity source target-symbol) 0.0)))
    (and
     ;; Statistical similarity check
     (>= stat-similarity *pattern-similarity-threshold*)
     ;; Naval: FFT structural similarity MUST be >= 0.9
     (>= struct-similarity *structural-similarity-threshold*)
     ;; Quality checks
     (>= (learned-pattern-success-rate pattern) 0.5)
     (>= (learned-pattern-sample-count pattern) 10)
     ;; Log strict check result
     (progn
       (format t "[T] 🔒 Transfer check: stat=~,2f struct=~,2f → ~a~%"
               stat-similarity struct-similarity
               (if (and (>= stat-similarity *pattern-similarity-threshold*)
                        (>= struct-similarity *structural-similarity-threshold*))
                   "PASS" "FAIL"))
       t))))

(defun apply-pattern-to-symbol (pattern target-symbol)
  "Apply a learned pattern to a new symbol.
   Returns adjusted parameters for the target."
  (let ((source (learned-pattern-source-symbol pattern)))
    (if (can-transfer-pattern-p pattern target-symbol)
        (let* ((similarity (calculate-asset-similarity source target-symbol))
               (base-params (learned-pattern-parameters pattern))
               ;; Adjust parameters based on similarity
               ;; Lower similarity = more conservative params
               (confidence-factor (+ 0.5 (* 0.5 similarity)))
               (adjusted-params 
                (list :sl-multiplier (* (getf base-params :sl-multiplier) 
                                        (/ 1.0 confidence-factor))
                      :tp-multiplier (* (getf base-params :tp-multiplier)
                                        confidence-factor)
                      :lot-multiplier (* 0.5 confidence-factor)))
               (transfer-result
                (list :pattern-type (learned-pattern-pattern-type pattern)
                      :source source
                      :target target-symbol
                      :conditions (learned-pattern-conditions pattern)
                      :adjusted-params adjusted-params
                      :confidence similarity)))
          
          (format t "[T] ✨ Transferred ~a pattern: ~a → ~a (conf: ~,0f%)~%"
                  (learned-pattern-pattern-type pattern)
                  source target-symbol
                  (* 100 similarity))
          transfer-result)
        (progn
          (format t "[T] ❌ Cannot transfer pattern to ~a (similarity too low)~%"
                  target-symbol)
          nil))))

(defun transfer-all-patterns (source-symbol target-symbol)
  "Transfer all applicable patterns from source to target"
  (let ((patterns (or (gethash source-symbol *learned-patterns*)
                      (extract-learned-patterns source-symbol)))
        (transferred nil))
    (dolist (pattern patterns)
      (let ((result (apply-pattern-to-symbol pattern target-symbol)))
        (when result
          (push result transferred))))
    
    (format t "[T] 📤 Transferred ~d/~d patterns: ~a → ~a~%"
            (length transferred) (length patterns)
            source-symbol target-symbol)
    transferred))

;;; ==========================================
;;; TRANSFER SUCCESS TRACKING
;;; ==========================================

(defun record-transfer-result (source-symbol target-symbol success-p)
  "Record whether a transferred pattern succeeded"
  (let* ((key (cons source-symbol target-symbol))
         (stats (gethash key *transfer-success-rates* (list 0 0))))
    (when success-p (incf (first stats)))
    (incf (second stats))
    (setf (gethash key *transfer-success-rates*) stats)
    
    (let ((rate (if (> (second stats) 0)
                    (* 100 (/ (first stats) (float (second stats))))
                    0)))
      (format t "[T] 📊 Transfer ~a→~a: ~,0f% success (~d/~d)~%"
              source-symbol target-symbol rate
              (first stats) (second stats)))))

(defun calculate-transfer-success-rate (source target)
  "Get transfer success rate between two assets"
  (let ((stats (gethash (cons source target) *transfer-success-rates*)))
    (if (and stats (> (second stats) 0))
        (/ (first stats) (float (second stats)))
        nil)))

;;; ==========================================
;;; TRANSFER LEARNING REPORT
;;; ==========================================

(defun transfer-learning-report ()
  "Generate transfer learning status report"
  (format t "~%")
  (format t "════════════════════════════════════════════════════~%")
  (format t "🔄 TRANSFER LEARNING REPORT (Naval)~%")
  (format t "════════════════════════════════════════════════════~%")
  
  ;; Learned patterns by symbol
  (format t "~%📚 LEARNED PATTERNS:~%")
  (maphash
   (lambda (symbol patterns)
     (format t "   ~a: ~d patterns~%" symbol (length patterns))
     (dolist (p patterns)
       (format t "      • ~a (~,0f% success, n=~d)~%"
               (learned-pattern-pattern-type p)
               (* 100 (learned-pattern-success-rate p))
               (learned-pattern-sample-count p))))
   *learned-patterns*)
  
  ;; Transfer success rates
  (format t "~%📊 TRANSFER SUCCESS RATES:~%")
  (maphash
   (lambda (key stats)
     (let ((rate (if (> (second stats) 0)
                     (* 100 (/ (first stats) (float (second stats))))
                     0)))
       (format t "   ~a → ~a: ~,0f% (~d/~d)~%"
               (car key) (cdr key) rate
               (first stats) (second stats))))
   *transfer-success-rates*)
  
  (format t "~%════════════════════════════════════════════════════~%"))

;;; ==========================================
;;; NAVAL IMPROVEMENT: STRUCTURAL TRANSFER LEARNING
;;; ==========================================
;;; Instead of just copying statistics, learn structural similarity
;;; using FFT-based pattern comparison and event response analysis

(defparameter *price-pattern-cache* (make-hash-table :test 'equal)
  "Cache of extracted price patterns for each symbol")
(defparameter *event-responses* (make-hash-table :test 'equal)
  "How each symbol responds to major events: symbol -> event-response-list")

(defun extract-price-pattern (history window-size)
  "Extract normalized price pattern from candle history.
   Returns list of normalized returns (pseudo-FFT approach)."
  (when (and history (> (length history) window-size))
    (let ((returns nil))
      (loop for i from 0 below (1- window-size)
            for c1 = (nth i history)
            for c2 = (nth (1+ i) history)
            when (and c1 c2 (candle-close c1) (candle-close c2) 
                      (> (candle-close c2) 0))
            do (push (/ (- (candle-close c1) (candle-close c2))
                        (candle-close c2))
                     returns))
      (nreverse returns))))

(defun calculate-pattern-similarity-fft (pattern1 pattern2)
  "Calculate structural similarity between two price patterns.
   Uses correlation-based pseudo-FFT approach."
  (when (and pattern1 pattern2 
             (> (length pattern1) 10)
             (= (length pattern1) (length pattern2)))
    (let* ((n (length pattern1))
           (mean1 (/ (reduce #'+ pattern1) n))
           (mean2 (/ (reduce #'+ pattern2) n))
           (cov (/ (reduce #'+ (mapcar (lambda (a b) 
                                         (* (- a mean1) (- b mean2)))
                                       pattern1 pattern2)) n))
           (var1 (/ (reduce #'+ (mapcar (lambda (a) (expt (- a mean1) 2)) 
                                        pattern1)) n))
           (var2 (/ (reduce #'+ (mapcar (lambda (a) (expt (- a mean2) 2)) 
                                        pattern2)) n))
           (std1 (sqrt var1))
           (std2 (sqrt var2)))
      (if (and (> std1 0) (> std2 0))
          (/ cov (* std1 std2))
          0.0))))

(defun get-structural-similarity (symbol1 symbol2 &optional (window 50))
  "Get structural (not just statistical) similarity between symbols.
   Naval improvement: Compares actual price movement patterns."
  (let* ((history1 (gethash symbol1 *candle-histories*))
         (history2 (gethash symbol2 *candle-histories*))
         (pattern1 (or (gethash symbol1 *price-pattern-cache*)
                       (setf (gethash symbol1 *price-pattern-cache*)
                             (extract-price-pattern history1 window))))
         (pattern2 (or (gethash symbol2 *price-pattern-cache*)
                       (setf (gethash symbol2 *price-pattern-cache*)
                             (extract-price-pattern history2 window)))))
    (if (and pattern1 pattern2)
        (let ((similarity (calculate-pattern-similarity-fft pattern1 pattern2)))
          (format t "[T] 📐 Structural similarity ~a↔~a: ~,2f~%"
                  symbol1 symbol2 similarity)
          similarity)
        0.0)))

(defun record-event-response (symbol event-type price-change timestamp)
  "Record how a symbol responded to a major event.
   Naval improvement: Learn causality, not just correlation."
  (let ((responses (gethash symbol *event-responses* nil)))
    (push (list :event event-type
                :change price-change
                :timestamp timestamp)
          responses)
    ;; Keep last 50 events
    (when (> (length responses) 50)
      (setf responses (subseq responses 0 50)))
    (setf (gethash symbol *event-responses*) responses)
    (format t "[T] 📝 Recorded ~a response to ~a: ~,2f%~%"
            symbol event-type (* 100 price-change))))

(defun calculate-event-response-similarity (symbol1 symbol2)
  "Calculate how similarly two symbols respond to the same events.
   Naval improvement: Causal transfer learning."
  (let ((responses1 (gethash symbol1 *event-responses*))
        (responses2 (gethash symbol2 *event-responses*)))
    (when (and responses1 responses2)
      (let ((common-events nil)
            (total-similarity 0.0))
        ;; Find common event types
        (dolist (r1 responses1)
          (let ((matching (find (getf r1 :event) responses2
                                :key (lambda (r) (getf r :event)))))
            (when matching
              (let ((change1 (getf r1 :change))
                    (change2 (getf matching :change)))
                ;; Similarity: same direction = positive, opposite = negative
                (push (list :event (getf r1 :event)
                            :similarity (if (or (and (> change1 0) (> change2 0))
                                                (and (< change1 0) (< change2 0)))
                                            (min 1.0 (/ (min (abs change1) (abs change2))
                                                        (max (abs change1) (abs change2))))
                                            -1.0))
                      common-events)))))
        ;; Average similarity
        (when common-events
          (setf total-similarity 
                (/ (reduce #'+ common-events :key (lambda (e) (getf e :similarity)))
                   (length common-events)))
          (format t "[T] 🎭 Event response similarity ~a↔~a: ~,2f (~d events)~%"
                  symbol1 symbol2 total-similarity (length common-events))
          total-similarity)))))

(defun enhanced-transfer-similarity (symbol1 symbol2)
  "Naval improvement: Combined similarity using all methods.
   Weights: 40% statistical, 30% structural, 30% event-response."
  (let ((stat-sim (calculate-asset-similarity symbol1 symbol2))
        (struct-sim (or (get-structural-similarity symbol1 symbol2) 0.5))
        (event-sim (or (calculate-event-response-similarity symbol1 symbol2) 0.5)))
    (let ((combined (+ (* 0.4 stat-sim)
                       (* 0.3 struct-sim)
                       (* 0.3 event-sim))))
      (format t "[T] 📊 Enhanced similarity ~a↔~a: ~,2f (stat:~,2f struct:~,2f event:~,2f)~%"
              symbol1 symbol2 combined stat-sim struct-sim event-sim)
      combined)))



;;; ==========================================
;;; CONVENIENCE FUNCTIONS
;;; ==========================================

(defun setup-transfer-learning ()
  "Initialize transfer learning from primary asset"
  (let ((primary "USDJPY")
        (targets '("EURUSD" "GBPUSD")))
    (format t "[T] 🚀 Setting up transfer learning from ~a~%" primary)
    
    ;; Extract patterns from primary
    (extract-learned-patterns primary)
    
    ;; Transfer to each target
    (dolist (target targets)
      (transfer-all-patterns primary target))
    
    (format t "[T] ✅ Transfer learning setup complete~%")))

;;; ==========================================
;;; INITIALIZATION
;;; ==========================================

(format t "[L] 🔄 transfer-learning.lisp loaded - Naval transfer learning active~%")
(format t "[L] 📤 Use (setup-transfer-learning) to initialize~%")
