;;; school-evaluation.lisp - Strategy Signal Evaluation (SRP)
(in-package :swimmy.school)

;;; ==========================================
;;; INDICATORS & SIGNALS
;;; ==========================================

(defun get-indicator-values (strat history)
  "Calculate current indicator values for display"
  (let ((values nil))
    (dolist (ind (strategy-indicators strat))
      (let ((type (car ind)) (p (cdr ind)))
        (handler-case
            (case type
              (sma (push (list (format nil "SMA-~d" (car p)) (float (ind-sma (car p) history))) values))
              (ema (push (list (format nil "EMA-~d" (car p)) (float (ind-ema (car p) history))) values))
              (rsi (push (list (format nil "RSI-~d" (car p)) (float (ind-rsi (car p) history))) values))
              (macd (multiple-value-bind (m s) (ind-macd (first p) (second p) (third p) history)
                      (push (list "MACD" (float m)) values)))
              (bb (multiple-value-bind (m u l) (ind-bb (first p) (second p) history)
                    (push (list "BB-Mid" (float m)) values))))
          (error (e) nil))))
    (nreverse values)))

(defun transform-cross-calls-helper (expr pkg)
  (flet ((add-prev-suffix (sym)
           (if (symbolp sym)
               (intern (format nil "~a-PREV" (symbol-name sym)) pkg)
               sym)))
    (cond
      ((atom expr) expr)
      ((and (listp expr) 
            (member (car expr) '(cross-above cross-below))
            (= (length expr) 3))
       (let ((fn (first expr))
             (a (second expr))
             (b (third expr)))
         (list fn 
               (if (symbolp a) a (eval a)) 
               (if (symbolp b) b (eval b)) 
               (if (symbolp a) (add-prev-suffix a) (eval a)) 
               (if (symbolp b) (add-prev-suffix b) (eval b)))))
      (t (mapcar (lambda (e) (transform-cross-calls-helper e pkg)) expr)))))

(defun evaluate-strategy-signal (strat history)
  "Evaluate strategy signal. V49.5: Support for Swarm Ensemble voting."
  (when (and (fboundp 'swarm-strategy-p) (swarm-strategy-p strat))
    (multiple-value-bind (sig strength) (convene-swarm-voting strat history)
      (declare (ignore strength))
      (return-from evaluate-strategy-signal 
        (case sig (1 :buy) (-1 :sell) (t :hold)))))
  
  (unless (is-safe-trading-time-p (strategy-name strat))
    (return-from evaluate-strategy-signal :hold))
  (when (and history (> (length history) 100))
    (let* ((indicators (strategy-indicators strat))
           (entry-logic (strategy-entry strat))
           (rest-hist (rest history))
           (bindings 
            (loop for ind in indicators
                  append (let ((type (car ind)) (p (cdr ind))
                                (pkg (find-package :swimmy.school)))
                            (case type
                              (sma `((,(intern (format nil "SMA-~d" (car p)) pkg) ,(ind-sma (car p) history))
                                     (,(intern (format nil "SMA-~d-PREV" (car p)) pkg) ,(ind-sma (car p) rest-hist))))
                              (ema `((,(intern (format nil "EMA-~d" (car p)) pkg) ,(ind-ema (car p) history))
                                     (,(intern (format nil "EMA-~d-PREV" (car p)) pkg) ,(ind-ema (car p) rest-hist))))
                              (rsi `((,(intern (format nil "RSI-~d" (car p)) pkg) ,(ind-rsi (car p) history))
                                     (,(intern (format nil "RSI-~d-PREV" (car p)) pkg) ,(ind-rsi (car p) rest-hist))))
                              (cci `((,(intern (format nil "CCI-~d" (car p)) pkg) ,(ind-cci (car p) history))
                                     (,(intern (format nil "CCI-~d-PREV" (car p)) pkg) ,(ind-cci (car p) rest-hist))))
                              (atr `((,(intern (format nil "ATR-~d" (car p)) pkg) ,(ind-atr (car p) history))))
                              (macd (multiple-value-bind (m s) (ind-macd (first p) (second p) (third p) history)
                                      (multiple-value-bind (pm ps) (ind-macd (first p) (second p) (third p) rest-hist)
                                        `((,(intern "MACD-LINE" pkg) ,m) (,(intern "SIGNAL-LINE" pkg) ,s) 
                                          (,(intern "MACD-LINE-PREV" pkg) ,pm) (,(intern "SIGNAL-LINE-PREV" pkg) ,ps)))))
                              (bb (multiple-value-bind (m u l) (ind-bb (first p) (second p) history)
                                    (multiple-value-bind (pm pu pl) (ind-bb (first p) (second p) rest-hist)
                                      (let ((dev (second p)))
                                        `((,(intern (format nil "BB-MIDDLE-~d" dev) pkg) ,m)
                                          (,(intern (format nil "BB-UPPER-~d" dev) pkg) ,u)
                                          (,(intern (format nil "BB-LOWER-~d" dev) pkg) ,l)
                                          (,(intern (format nil "BB-MIDDLE-~d-PREV" dev) pkg) ,pm)
                                          (,(intern (format nil "BB-UPPER-~d-PREV" dev) pkg) ,pu)
                                          (,(intern (format nil "BB-LOWER-~d-PREV" dev) pkg) ,pl)
                                          (,(intern "BB-MIDDLE" pkg) ,m) (,(intern "BB-UPPER" pkg) ,u) (,(intern "BB-LOWER" pkg) ,l)
                                          (,(intern "BB-WIDTH" pkg) (- u l))
                                          (,(intern "BB-MIDDLE-PREV" pkg) ,pm) (,(intern "BB-UPPER-PREV" pkg) ,pu) (,(intern "BB-LOWER-PREV" pkg) ,pl))))))
                              (stoch (let ((k (ind-stoch (first p) (second p) history))
                                           (pk (ind-stoch (first p) (second p) rest-hist)))
                                      `((,(intern "STOCH-K" pkg) ,k) (,(intern "STOCH-K-PREV" pkg) ,pk) 
                                        (,(intern "STOCH-D" pkg) 50) (,(intern "STOCH-D-PREV" pkg) 50))))
                              (session-high `((,(intern (format nil "SESSION-HIGH-~d-~d" (first p) (second p)) pkg) 
                                               ,(ind-session-high (first p) (second p) history))))
                              (session-low `((,(intern (format nil "SESSION-LOW-~d-~d" (first p) (second p)) pkg) 
                                              ,(ind-session-low (first p) (second p) history))))
                              ;; Placeholder bindings for unimplemented indicators (prevent compile errors)
                              (ichimoku `((,(intern "SENKOU-A" pkg) ,(candle-close (first history)))
                                          (,(intern "SENKOU-B" pkg) ,(candle-close (first history)))
                                          (,(intern "TENKAN" pkg) ,(candle-close (first history)))
                                          (,(intern "KIJUN" pkg) ,(candle-close (first history)))))
                              (donchian (let* ((period (first p))
                                               (highs (mapcar #'candle-high (subseq history 0 (min period (length history)))))
                                               (lows (mapcar #'candle-low (subseq history 0 (min period (length history)))))
                                               (upper (if highs (apply #'max highs) 0))
                                               (lower (if lows (apply #'min lows) 0))
                                               (mid (/ (+ upper lower) 2)))
                                          `((,(intern "DONCHIAN-UPPER" pkg) ,upper)
                                            (,(intern "DONCHIAN-LOWER" pkg) ,lower)
                                            (,(intern "DONCHIAN-MID" pkg) ,mid))))
                              (t nil))))))
      (let ((pkg (find-package :swimmy.school))
            (current-close (candle-close (first history)))
            (prev-close (candle-close (second history)))
            (vol (candle-volume (first history))))
        (push `(,(intern "CLOSE" pkg) ,current-close) bindings)
        (push `(,(intern "CLOSE-PREV" pkg) ,prev-close) bindings)
        (push `(,(intern "HIGH" pkg) ,(candle-high (first history))) bindings)
        (push `(,(intern "HIGH-PREV" pkg) ,(candle-high (second history))) bindings)
        (push `(,(intern "LOW" pkg) ,(candle-low (first history))) bindings)
        (push `(,(intern "LOW-PREV" pkg) ,(candle-low (second history))) bindings)
        (push `(,(intern "OPEN" pkg) ,(candle-open (first history))) bindings)
        
        ;; V15.2: Fix undefined variables (RSI, EMA, VOLUME, PNL, HISTORY, TP, SL)
        ;; Bind defaults for generic variable names used in generated strategies
        (push `(,(intern "VOLUME" pkg) ,vol) bindings)
        (push `(,(intern "HISTORY" pkg) ',history) bindings)
        (push `(,(intern "PNL" pkg) 0.0) bindings) ; Placeholder as PnL is strategy-specific
        (push `(,(intern "TP" pkg) ,(strategy-tp strat)) bindings)
        (push `(,(intern "SL" pkg) ,(strategy-sl strat)) bindings)
        
        ;; Default indicator bindings (alias to standard params if not specified)
        ;; Note: These are rough defaults to preventing crashing. 
        ;; Ideal fix is to re-generate strategies with correct params.
        (when (fboundp 'ind-rsi)
          (push `(,(intern "RSI" pkg) ,(ind-rsi 14 history)) bindings))
        (when (fboundp 'ind-ema)
          (push `(,(intern "EMA" pkg) ,(ind-ema 20 history)) bindings))
        (when (fboundp 'ind-bb)
           (multiple-value-bind (m u l) (ind-bb 20 2.0 history)
             (push `(,(intern "BB-WIDTH" pkg) ,(- u l)) bindings)))
        
        (multiple-value-bind (sec min hour day month year dow) (decode-universal-time (get-universal-time))
          (declare (ignore sec day month year dow))
          (push `(,(intern "HOUR" pkg) ,hour) bindings)
          (push `(,(intern "MINUTE" pkg) ,min) bindings)
          (let ((is-gotobi (if (fboundp (intern "GOTOBI-DAY-P" pkg))
                               (if (funcall (intern "GOTOBI-DAY-P" pkg)) t nil)
                               nil)))
            (push `(,(intern "GOTOBI-P" pkg) ,is-gotobi) bindings))))
      (setf bindings (remove-duplicates bindings :key #'car :from-end t))
      (let* ((pkg (find-package :swimmy.school))
             (transformed-logic (transform-cross-calls-helper entry-logic pkg)))
        (handler-case
            (locally (declare (sb-ext:muffle-conditions style-warning))
              (let ((entry-result (eval `(let ,bindings 
                                           (declare (ignorable ,@(mapcar #'car bindings)))
                                           ,transformed-logic))))
                (cond
                  (entry-result :buy)
                  ((and (strategy-exit strat)
                        (eval `(let ,bindings 
                                 (declare (ignorable ,@(mapcar #'car bindings)))
                                 ,(transform-cross-calls-helper (strategy-exit strat) pkg)))) :sell)
                  (t :hold))))
          (error (e) 
            (format t "[L] Eval Err ~a: ~a~%" (strategy-name strat) e)
            :hold))))))


(defun select-strategies-for-regime (regime strategies)
  "Select strategies appropriate for the current market regime (Layered Selection)"
  (let ((target-categories 
         (case regime
           ((:trend-early :trend-mature) '(:trend :breakout))
           ((:trend-exhausted) '(:mean-reversion :counter-trend))
           ((:range-expansion :ranging) '(:range :mean-reversion))
           ((:range-compression) '(:breakout :trend))
           ;; V45: Don't fully halt - allow LEGEND + scalp with warning (Opus 2026-01-20)
           ((:volatile-spike :illiquid) 
            (format t "[L] ⚠️ MARKET CAUTION (~a). Limiting to LEGEND/scalp strategies.~%" regime)
            '(:scalp :legend))  ;; Allow defensive trading
           (t '(:trend :range))))) ;; Default generic
    
    (remove-if-not 
     (lambda (s) 
       (let ((cat (strategy-category s)))
         (or (member cat target-categories)
             (search "LEGEND" (strategy-name s))))) ;; Always check legends
     strategies)))

(defun collect-strategy-signals (symbol history)
  "Evaluate Context-Aware subset of strategies and return triggered signals"
  (let* ((regime (or *current-regime* (detect-market-regime)))
         (candidates (select-strategies-for-regime regime *strategy-knowledge-base*))
         (signals nil))
    
    ;; Log selection count for debugging
    (format t "[L] Regime: ~a | Scanning ~d/~d strategies~%" 
            regime (length candidates) (length *strategy-knowledge-base*))

    (dolist (strat candidates)
      (handler-case
          (let* ((name (strategy-name strat))
                 (benched (and (fboundp 'strategy-benched-p) (strategy-benched-p name)))
                 (mismatch (check-symbol-mismatch name symbol))) 
            (unless (or benched mismatch) 
              (let ((sig (evaluate-strategy-signal strat history)))
                (when (member sig '(:buy :sell))
                  ;; Phase 5: Confidence & Scoring Engine
                  ;; Score = Adaptation * Confidence
                  (let* ((score (if (fboundp 'calculate-strategy-score)
                                    (calculate-strategy-score strat regime)
                                    0.6)) ;; Fallback
                         (adapt-mult (if (fboundp 'get-adaptation-multiplier)
                                         (get-adaptation-multiplier strat regime)
                                         1.0))
                         (confidence (if (fboundp 'estimate-confidence)
                                         (estimate-confidence strat)
                                         0.5)))
                    
                    (record-strategy-signal name sig (get-universal-time) 
                                          :confidence score
                                          :reason (format nil "Score: ~,2f (Adapt: ~,2f, Conf: ~,2f)" 
                                                          score adapt-mult confidence))
                    (push (list :strategy-name name
                                :category (infer-strategy-category strat)
                                :direction sig
                                :confidence score  ;; Use computed score as signal confidence
                                :sl (strategy-sl strat)
                                :tp (strategy-tp strat)
                                :indicator-values (get-indicator-values strat history))
                          signals))))))
        (error (e) 
           (format t "[L] Err evaluating ~a: ~a~%" (strategy-name strat) e)
           nil)))
    signals))
