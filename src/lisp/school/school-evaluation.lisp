;;; school-evaluation.lisp - Strategy Signal Evaluation (SRP)
(in-package :swimmy.school)

;;; ==========================================
;;; INDICATORS & SIGNALS
;;; ==========================================

(defun normalize-positive-integer (value default)
  "Normalize indicator period values to a positive integer."
  (if (and (integerp value) (> value 0))
      value
      default))

(defun normalize-bb-params (params)
  "Return (period dev dev-label) for Bollinger params.
When deviation is omitted, use dev=2.0."
  (let* ((period (normalize-positive-integer (first params) 20))
         (dev-raw (second params))
         (dev (if (numberp dev-raw) (float dev-raw) 2.0))
         (dev-label (if (numberp dev-raw)
                        dev-raw
                        (if (= dev (round dev)) (round dev) dev))))
    (values period dev dev-label)))

(defun normalize-williams-period (params)
  "Return Williams period with sane default."
  (normalize-positive-integer (first params) 14))

(defun normalize-indicator-type (type)
  "Normalize indicator type to keyword (:SMA/:EMA/...)."
  (flet ((normalize-name (name)
           (case (intern name :keyword)
             ((:BOLLINGER :BOLLINGER-BANDS) :BB)
             ((:VWAP-VOLUME-RATIO :VWAP_VOLUME_RATIO) :VWAPVR)
             (t (intern name :keyword)))))
    (cond
      ((keywordp type) (normalize-name (string-upcase (symbol-name type))))
      ((symbolp type) (normalize-name (string-upcase (symbol-name type))))
      ((stringp type) (normalize-name (string-upcase type)))
      (t nil))))

(defun string-ends-with (string suffix)
  "Return T when STRING ends with SUFFIX."
  (let ((slen (length string))
        (xlen (length suffix)))
    (and (>= slen xlen)
         (string= suffix string :start2 (- slen xlen)))))

(defun parse-positive-integer-string (text)
  "Parse TEXT as a positive integer; return NIL on failure."
  (handler-case
      (multiple-value-bind (value end) (parse-integer text :junk-allowed t)
        (when (and value (= end (length text)) (> value 0))
          value))
    (error () nil)))

(defun parse-period-indicator-symbol (symbol)
  "Parse SYMBOL like SMA-20 or SMA-20-PREV into (type period prev-p)."
  (when (symbolp symbol)
    (let* ((raw (string-upcase (symbol-name symbol)))
           (prev-p (string-ends-with raw "-PREV"))
           (base (if prev-p (subseq raw 0 (- (length raw) 5)) raw)))
      (dolist (spec '(("SMA-" . :sma)
                      ("EMA-" . :ema)
                      ("RSI-" . :rsi)
                      ("VWAPVR-" . :vwapvr)
                      ("CCI-" . :cci)
                      ("ATR-" . :atr)
                      ("WILLIAMS-" . :williams)))
        (let ((prefix (car spec))
              (type (cdr spec)))
          (when (and (>= (length base) (length prefix))
                     (string= prefix base :end2 (length prefix)))
            (let ((period (parse-positive-integer-string
                           (subseq base (length prefix)))))
              (when period
                (return (values type period prev-p))))))))))

(defun collect-referenced-period-indicators (expr)
  "Collect unique indicator refs as list of (type period) from EXPR."
  (let ((refs nil))
    (labels ((walk (node)
               (cond
                 ((symbolp node)
                  (multiple-value-bind (type period prev-p)
                      (parse-period-indicator-symbol node)
                    (declare (ignore prev-p))
                    (when (and type period)
                      (pushnew (list type period) refs :test #'equal))))
                 ((consp node)
                  (walk (car node))
                  (walk (cdr node)))
                 (t nil))))
      (walk expr))
    refs))

(defun collect-referenced-symbols (expr)
  "Collect unique symbols referenced from EXPR."
  (let ((refs nil))
    (labels ((walk (node)
               (cond
                 ((symbolp node)
                  (unless (or (keywordp node)
                              (member node '(t nil) :test #'eq))
                    (pushnew node refs :test #'eq)))
                 ((consp node)
                  (walk (car node))
                  (walk (cdr node)))
                 (t nil))))
      (walk expr))
    refs))

(defun indicator-period-prefix (type)
  "Return indicator symbol prefix (SMA/EMA/...) for TYPE."
  (case type
    (:sma "SMA")
    (:ema "EMA")
    (:rsi "RSI")
    (:vwapvr "VWAPVR")
    (:cci "CCI")
    (:atr "ATR")
    (:williams "WILLIAMS")
    (t nil)))

(defun indicator-default-value (type history)
  "Fallback numeric value for indicator TYPE when calculation is unavailable."
  (let ((close (float (or (ignore-errors (candle-close (first history))) 0.0))))
    (case type
      ((:sma :ema) close)
      (:rsi 50.0)
      (:vwapvr 0.0)
      (:cci 0.0)
      (:atr 0.0)
      (:williams -50.0)
      (t 0.0))))

(defun period-indicator-value (type period history)
  "Compute period indicator value with safe numeric fallback."
  (let ((value (ignore-errors
                 (case type
                   (:sma (ind-sma period history))
                   (:ema (ind-ema period history))
                   (:rsi (ind-rsi period history))
                   (:vwapvr (ind-vwapvr period history))
                   (:cci (ind-cci period history))
                   (:atr (ind-atr period history))
                   (:williams (ind-williams period history))
                   (t nil)))))
    (if (numberp value)
        value
        (indicator-default-value type history))))

(defun ensure-logic-period-bindings (bindings history rest-hist entry-logic exit-logic pkg)
  "Add missing period indicator bindings referenced in entry/exit logic."
  (let ((refs (append (collect-referenced-period-indicators entry-logic)
                      (collect-referenced-period-indicators exit-logic))))
    (dolist (ref refs bindings)
      (destructuring-bind (type period) ref
        (let ((prefix (indicator-period-prefix type)))
          (when prefix
            (let* ((base-name (format nil "~A-~D" prefix period))
                   (curr-sym (intern base-name pkg))
                   (prev-sym (intern (format nil "~A-PREV" base-name) pkg)))
              (unless (assoc curr-sym bindings)
                (push `(,curr-sym ,(period-indicator-value type period history)) bindings))
              (unless (assoc prev-sym bindings)
                (push `(,prev-sym ,(period-indicator-value type period rest-hist)) bindings)))))))))

(defun ensure-logic-symbol-alias-bindings (bindings entry-logic exit-logic pkg)
  "Add alias bindings for symbols referenced from non-canonical packages."
  (labels ((find-binding-by-symbol-name (name)
             (find (string-upcase name)
                   bindings
                   :test #'string=
                   :key (lambda (binding)
                          (let ((sym (and (consp binding) (car binding))))
                            (if (symbolp sym)
                                (string-upcase (symbol-name sym))
                                ""))))))
    (let ((refs (append (collect-referenced-symbols entry-logic)
                        (collect-referenced-symbols exit-logic))))
      (dolist (sym refs bindings)
        (let* ((canonical (intern (string-upcase (symbol-name sym)) pkg))
               (canonical-binding (or (assoc canonical bindings)
                                      (find-binding-by-symbol-name (symbol-name sym)))))
          (when (and canonical-binding
                     (not (assoc sym bindings))
                     (not (eq sym canonical)))
            (push (list sym (second canonical-binding)) bindings)))))))

(defun find-indicator-period (indicators target-type default)
  "Find first period value for TARGET-TYPE from INDICATORS, or DEFAULT."
  (or (dolist (ind indicators nil)
        (let* ((type (normalize-indicator-type (car ind)))
               (params (cdr ind))
               (period (normalize-positive-integer (first params) nil)))
          (when (and (eq type target-type) period)
            (return period))))
      default))

(defun unwrap-singleton-logic (expr)
  "Unwrap singleton wrapper forms like ((...)) produced by legacy strategy imports."
  (if (and (consp expr)
           (null (cdr expr))
           (consp (car expr)))
      (unwrap-singleton-logic (car expr))
      expr))

(defun legacy-indicator-ref->symbol (type period pkg)
  "Convert legacy indicator ref (:SMA 50) into canonical symbol SMA-50."
  (let* ((norm-type (normalize-indicator-type type))
         (norm-period (normalize-positive-integer period nil))
         (prefix (and norm-type (indicator-period-prefix norm-type))))
    (if (and prefix norm-period)
        (intern (format nil "~A-~D" prefix norm-period) pkg)
        type)))

(defun normalize-legacy-logic-form (expr indicators pkg)
  "Normalize legacy keyword shorthand logic into canonical evaluable forms."
  (labels ((walk (node)
             (let ((node (unwrap-singleton-logic node)))
               (cond
                 ((atom node) node)
                 ((and (consp node) (keywordp (car node)))
                  (case (car node)
                    (:rsi-below
                     (let* ((threshold-raw (second node))
                            (threshold (if (numberp threshold-raw) threshold-raw 50))
                            (period (find-indicator-period indicators :rsi 14))
                            (rsi-sym (intern (format nil "RSI-~D" period) pkg)))
                       `(< ,rsi-sym ,threshold)))
                    (:rsi-above
                     (let* ((threshold-raw (second node))
                            (threshold (if (numberp threshold-raw) threshold-raw 50))
                            (period (find-indicator-period indicators :rsi 14))
                            (rsi-sym (intern (format nil "RSI-~D" period) pkg)))
                       `(> ,rsi-sym ,threshold)))
                    (:cross-over
                     (destructuring-bind (&optional a1 a2 b1 b2) (cdr node)
                       (let ((left (if (and b1 b2)
                                       (legacy-indicator-ref->symbol a1 a2 pkg)
                                       (walk a1)))
                             (right (if (and b1 b2)
                                        (legacy-indicator-ref->symbol b1 b2 pkg)
                                        (walk a2))))
                         `(cross-above ,left ,right))))
                    (:cross-under
                     (destructuring-bind (&optional a1 a2 b1 b2) (cdr node)
                       (let ((left (if (and b1 b2)
                                       (legacy-indicator-ref->symbol a1 a2 pkg)
                                       (walk a1)))
                             (right (if (and b1 b2)
                                        (legacy-indicator-ref->symbol b1 b2 pkg)
                                        (walk a2))))
                         `(cross-below ,left ,right))))
                    (otherwise (mapcar #'walk node))))
                 ((consp node) (mapcar #'walk node))
                 (t node)))))
    (walk expr)))

(defun get-indicator-values (strat history)
  "Calculate current indicator values for display"
  (let ((values nil))
    (dolist (ind (strategy-indicators strat))
      (let* ((type (normalize-indicator-type (car ind)))
             (p (cdr ind)))
        (handler-case
            (case type
              (:sma (push (list (format nil "SMA-~d" (car p)) (float (ind-sma (car p) history))) values))
              (:ema (push (list (format nil "EMA-~d" (car p)) (float (ind-ema (car p) history))) values))
              (:rsi (push (list (format nil "RSI-~d" (car p)) (float (ind-rsi (car p) history))) values))
              (:vwapvr (let* ((period (normalize-positive-integer (first p) 20))
                              (v (or (ind-vwapvr period history) 0.0)))
                         (push (list (format nil "VWAPVR-~d" period) (float v)) values)))
              (:macd (multiple-value-bind (m s) (ind-macd (first p) (second p) (third p) history)
                      (declare (ignore s))
                      (push (list "MACD" (float m)) values)))
              (:bb (multiple-value-bind (period dev dev-label) (normalize-bb-params p)
                    (declare (ignore dev-label))
                    (multiple-value-bind (m u l) (ind-bb period dev history)
                      (declare (ignore u l))
                      (push (list "BB-Mid" (float m)) values))))
              (:williams (let ((period (normalize-williams-period p)))
                          (push (list (format nil "WILLIAMS-~d" period)
                                      (float (ind-williams period history)))
                                values))))
          (error () nil))))
    (nreverse values)))

(defun transform-cross-calls-helper (expr pkg)
  "V49.8 Fix: Safe code transformation without dangerous EVAL.
   Recursively maps forms like (SMA 5) or symbols like CLOSE to their bound equivalents."
  (flet ((map-to-symbol (item &optional prev-p)
           (cond
             ((symbolp item)
              (if prev-p
                  (intern (format nil "~A-PREV" (symbol-name item)) pkg)
                  item))
             ((and (listp item) (car item))
              (let* ((type (normalize-indicator-type (car item)))
                     (params (cdr item))
                     (base (case type
                             (:sma (format nil "SMA-~D" (first params)))
                             (:ema (format nil "EMA-~D" (first params)))
                             (:rsi (format nil "RSI-~D" (first params)))
                             (:cci (format nil "CCI-~D" (first params)))
                             (:atr (format nil "ATR-~D" (first params)))
                             (:williams (format nil "WILLIAMS-~D" (first params)))
                             (t nil))))
                (if base
                    (intern (if prev-p 
                                (format nil "~A-PREV" base)
                                base)
                            pkg)
                    item)))
             ((numberp item) item)
             (t item))))
    (cond
      ((atom expr) expr)
      ((and (listp expr) 
            (member (car expr) '(cross-above cross-below) :test #'eq)
            (= (length expr) 3))
       (let ((fn (first expr))
             (a (second expr))
             (b (third expr)))
         (list fn 
               (map-to-symbol a nil) 
               (map-to-symbol b nil) 
               (map-to-symbol a t) 
               (map-to-symbol b t))))
      (t (mapcar (lambda (e) (transform-cross-calls-helper e pkg)) expr)))))

(defun evaluate-strategy-signal (strat history)
  "Evaluate strategy signal. V49.5: Support for Swarm Ensemble voting."
  ;; Swarm Logic Removed (V50.1 Cleanup)
  
  (unless (is-safe-trading-time-p (strategy-name strat))
    (return-from evaluate-strategy-signal :hold))
  (when (and history (> (length history) 100))
    (let* ((indicators (strategy-indicators strat))
           (entry-logic (strategy-entry strat))
           (exit-logic (strategy-exit strat))
           (rest-hist (rest history))
           (pkg (find-package :swimmy.school))
           (normalized-entry-logic (normalize-legacy-logic-form entry-logic indicators pkg))
           (normalized-exit-logic (and exit-logic
                                       (normalize-legacy-logic-form exit-logic indicators pkg)))
           (transformed-entry-logic (transform-cross-calls-helper normalized-entry-logic pkg))
           (transformed-exit-logic (and normalized-exit-logic
                                        (transform-cross-calls-helper normalized-exit-logic pkg)))
           (bindings 
            (loop for ind in indicators
                  append (let* ((type (normalize-indicator-type (car ind)))
                                (p (cdr ind)))
                            (case type
                              (:sma `((,(intern (format nil "SMA-~d" (car p)) pkg) ,(ind-sma (car p) history))
                                     (,(intern (format nil "SMA-~d-PREV" (car p)) pkg) ,(ind-sma (car p) rest-hist))))
                              (:ema `((,(intern (format nil "EMA-~d" (car p)) pkg) ,(ind-ema (car p) history))
                                     (,(intern (format nil "EMA-~d-PREV" (car p)) pkg) ,(ind-ema (car p) rest-hist))))
                              (:rsi `((,(intern (format nil "RSI-~d" (car p)) pkg) ,(ind-rsi (car p) history))
                                     (,(intern (format nil "RSI-~d-PREV" (car p)) pkg) ,(ind-rsi (car p) rest-hist))))
                              (:vwapvr (let* ((period (normalize-positive-integer (first p) 20))
                                              (threshold (float (normalize-positive-integer (second p) 150)))
                                              (v (or (ind-vwapvr period history) 0.0))
                                              (pv (or (ind-vwapvr period rest-hist) 0.0)))
                                         `((,(intern (format nil "VWAPVR-~d" period) pkg) ,v)
                                           (,(intern (format nil "VWAPVR-~d-PREV" period) pkg) ,pv)
                                           (,(intern "VWAPVR" pkg) ,v)
                                           (,(intern "VWAPVR-PREV" pkg) ,pv)
                                           (,(intern "VWAPVR-THRESHOLD" pkg) ,threshold)
                                           (,(intern "VWAPVR-THRESH" pkg) ,threshold)
                                           (,(intern (format nil "VWAPVR-~d-THRESH" period) pkg) ,threshold))))
                              (:cci `((,(intern (format nil "CCI-~d" (car p)) pkg) ,(ind-cci (car p) history))
                                     (,(intern (format nil "CCI-~d-PREV" (car p)) pkg) ,(ind-cci (car p) rest-hist))))
                              (:atr `((,(intern (format nil "ATR-~d" (car p)) pkg) ,(ind-atr (car p) history))))
                              (:macd (multiple-value-bind (m s) (ind-macd (first p) (second p) (third p) history)
                                      (multiple-value-bind (pm ps) (ind-macd (first p) (second p) (third p) rest-hist)
                                        `((,(intern "MACD-LINE" pkg) ,m) (,(intern "SIGNAL-LINE" pkg) ,s) 
                                          (,(intern "MACD-LINE-PREV" pkg) ,pm) (,(intern "SIGNAL-LINE-PREV" pkg) ,ps)))))
                              (:bb (multiple-value-bind (period dev dev-label) (normalize-bb-params p)
                                    (multiple-value-bind (m u l) (ind-bb period dev history)
                                      (multiple-value-bind (pm pu pl) (ind-bb period dev rest-hist)
                                        `((,(intern (format nil "BB-MIDDLE-~a" dev-label) pkg) ,m)
                                          (,(intern (format nil "BB-UPPER-~a" dev-label) pkg) ,u)
                                          (,(intern (format nil "BB-LOWER-~a" dev-label) pkg) ,l)
                                          (,(intern (format nil "BB-MIDDLE-~a-PREV" dev-label) pkg) ,pm)
                                          (,(intern (format nil "BB-UPPER-~a-PREV" dev-label) pkg) ,pu)
                                          (,(intern (format nil "BB-LOWER-~a-PREV" dev-label) pkg) ,pl)
                                          (,(intern "BB-MIDDLE" pkg) ,m) (,(intern "BB-UPPER" pkg) ,u) (,(intern "BB-LOWER" pkg) ,l)
                                          (,(intern "BB-WIDTH" pkg) (- u l))
                                          (,(intern "BB-MIDDLE-PREV" pkg) ,pm) (,(intern "BB-UPPER-PREV" pkg) ,pu) (,(intern "BB-LOWER-PREV" pkg) ,pl)
                                          ;; Alias bindings for legacy hunted strategies.
                                          (,(intern "UPPER-BAND" pkg) ,u) (,(intern "LOWER-BAND" pkg) ,l)
                                          (,(intern "UPPER-BAND-PREV" pkg) ,pu) (,(intern "LOWER-BAND-PREV" pkg) ,pl))))))
                              (:williams (let* ((period (normalize-williams-period p))
                                               (w (ind-williams period history))
                                               (pw (ind-williams period rest-hist)))
                                          `((,(intern (format nil "WILLIAMS-~d" period) pkg) ,w)
                                            (,(intern (format nil "WILLIAMS-~d-PREV" period) pkg) ,pw)
                                            (,(intern "WILLIAMS" pkg) ,w)
                                            (,(intern "WILLIAMS-PREV" pkg) ,pw))))
                              (:stoch (let ((k (ind-stoch (first p) (second p) history))
                                           (pk (ind-stoch (first p) (second p) rest-hist)))
                                      `((,(intern "STOCH-K" pkg) ,k) (,(intern "STOCH-K-PREV" pkg) ,pk) 
                                        (,(intern "STOCH-D" pkg) 50) (,(intern "STOCH-D-PREV" pkg) 50))))
                              (:session-high `((,(intern (format nil "SESSION-HIGH-~d-~d" (first p) (second p)) pkg) 
                                               ,(ind-session-high (first p) (second p) history))))
                              (:session-low `((,(intern (format nil "SESSION-LOW-~d-~d" (first p) (second p)) pkg) 
                                              ,(ind-session-low (first p) (second p) history))))
                              ;; Placeholder bindings for unimplemented indicators (prevent compile errors)
                              (:ichimoku `((,(intern "SENKOU-A" pkg) ,(candle-close (first history)))
                                          (,(intern "SENKOU-B" pkg) ,(candle-close (first history)))
                                          (,(intern "TENKAN" pkg) ,(candle-close (first history)))
                                          (,(intern "KIJUN" pkg) ,(candle-close (first history)))))
                              (:donchian (let* ((period (first p))
                                               (highs (mapcar #'candle-high (subseq history 0 (min period (length history)))))
                                               (lows (mapcar #'candle-low (subseq history 0 (min period (length history)))))
                                               (upper (if highs (apply #'max highs) 0))
                                               (lower (if lows (apply #'min lows) 0))
                                              (mid (/ (+ upper lower) 2)))
                                          `((,(intern "DONCHIAN-UPPER" pkg) ,upper)
                                            (,(intern "DONCHIAN-LOWER" pkg) ,lower)
                                            (,(intern "DONCHIAN-MID" pkg) ,mid)
                                            ;; Alias bindings for hunted stop-hunter style logic.
                                            (,(intern "UPPER-BAND" pkg) ,upper)
                                            (,(intern "LOWER-BAND" pkg) ,lower))))
                              (t nil))))))
      (let ((current-close (candle-close (first history)))
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
             (declare (ignore m))
             (push `(,(intern "BB-WIDTH" pkg) ,(- u l)) bindings)))
        
        (multiple-value-bind (sec min hour day month year dow) (decode-universal-time (get-universal-time))
          (declare (ignore sec day month year dow))
          (push `(,(intern "HOUR" pkg) ,hour) bindings)
          (push `(,(intern "MINUTE" pkg) ,min) bindings)
          (let ((is-gotobi (if (fboundp (intern "GOTOBI-DAY-P" pkg))
                               (if (funcall (intern "GOTOBI-DAY-P" pkg)) t nil)
                               nil)))
            (push `(,(intern "GOTOBI-P" pkg) ,is-gotobi) bindings))))
      (setf bindings
            (ensure-logic-period-bindings bindings history rest-hist
                                          transformed-entry-logic
                                          transformed-exit-logic
                                          pkg))
      (setf bindings
            (ensure-logic-symbol-alias-bindings bindings
                                                transformed-entry-logic
                                                transformed-exit-logic
                                                pkg))
      (setf bindings (remove-duplicates bindings :key #'car :from-end t))
      (handler-case
          (locally (declare (sb-ext:muffle-conditions style-warning))
            (let ((entry-result (eval `(let ,bindings
                                         (declare (ignorable ,@(mapcar #'car bindings)))
                                         ,transformed-entry-logic))))
              (cond
                (entry-result :buy)
                ((and transformed-exit-logic
                      (eval `(let ,bindings
                               (declare (ignorable ,@(mapcar #'car bindings)))
                               ,transformed-exit-logic))) :sell)
                (t :hold))))
        (error (e)
          (format t "[L] Eval Err ~a: ~a~%" (strategy-name strat) e)
          :hold)))))


(defun select-strategies-for-regime (regime strategies)
  "Select strategies appropriate for the current market regime (Layered Selection)"
  (labels ((legend-strategy-p (s)
             (let* ((rank (and s (fboundp 'strategy-rank) (strategy-rank s)))
                    (rank-token (cond
                                  ((null rank) "")
                                  ((keywordp rank) (string-upcase (symbol-name rank)))
                                  ((symbolp rank) (string-upcase (symbol-name rank)))
                                  ((stringp rank) (string-upcase (string-trim '(#\Space #\Tab #\Newline #\Return #\:) rank)))
                                  (t (string-upcase (format nil "~a" rank))))))
               (or (string= rank-token "LEGEND")
                   ;; Backward compatibility for legacy naming conventions.
                   (search "LEGEND" (or (strategy-name s) "") :test #'char-equal)))))
    (let ((target-categories
           (case regime
             ((:trend-early :trend-mature) '(:trend :breakout))
             ;; Category taxonomy is :trend/:reversion/:breakout/:scalp (see DSL strategy struct).
             ;; Keep regime mapping aligned with real categories to avoid scanning 0 candidates live.
             ((:trend-exhausted) '(:reversion :scalp))
             ((:range-expansion :ranging) '(:reversion :scalp))
             ((:range-compression) '(:breakout :trend))
             ;; V45: Don't fully halt - allow LEGEND + scalp with warning (Opus 2026-01-20)
             ((:volatile-spike :illiquid)
              (format t "[L] ⚠️ MARKET CAUTION (~a). Limiting to LEGEND/scalp strategies.~%" regime)
              '(:scalp :legend))  ;; Allow defensive trading
             (t '(:trend :reversion))))) ;; Default generic

      (remove-if-not
       (lambda (s)
         (let ((regime-class (if (fboundp 'strategy-regime-class)
                                 (strategy-regime-class s)
                                 (strategy-category s))))
           (or (member regime-class target-categories)
               (legend-strategy-p s)))) ;; Always check legends
       strategies))))

(defun collect-strategy-signals (symbol history)
  "Evaluate Context-Aware subset of strategies and return triggered signals"
  (let* ((regime (or *current-regime* (detect-market-regime)))
         (active-pool (remove-if-not
                       (lambda (s)
                         (if (fboundp 'active-strategy-p)
                             (active-strategy-p s)
                             t))
                       *strategy-knowledge-base*))
         (regime-candidates (select-strategies-for-regime regime active-pool))
         (candidates (if (and (null regime-candidates) active-pool)
                         active-pool
                         regime-candidates))
         (signals nil))
    
    ;; Log selection count for debugging
    (when (and (null regime-candidates) active-pool)
      (format t "[L] ⚠️ Regime pool empty for ~a; fallback to active pool (~d).~%"
              regime (length active-pool)))
    (format t "[L] Regime: ~a | Scanning ~d/~d strategies~%" 
            regime (length candidates) (length active-pool))

    (dolist (strat candidates)
      (handler-case
          (let* ((name (strategy-name strat))
                 (mismatch (check-symbol-mismatch name symbol))) 
            (unless mismatch 
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
                                :timeframe (strategy-timeframe strat)
                                :sl (strategy-sl strat)
                                :tp (strategy-tp strat)
                                :indicator-values (get-indicator-values strat history))
                          signals))))))
        (error (e) 
           (format t "[L] Err evaluating ~a: ~a~%" (strategy-name strat) e)
           nil)))
    signals))
