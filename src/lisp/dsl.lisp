;; dsl.lisp - Strategy DSL

(in-package :swimmy.school)

;; Forward declarations to suppress warnings (actual values set in brain.lisp)
(defvar *candle-history* nil)
(defvar *pending-strategy* nil)
(defvar *last-confidence* 0.0)
(defvar *last-prediction* "HOLD")
(defvar *nn-threshold* 0.6)

(defun ind-sma (n history)
  (when (>= (length history) n)
    (float (/ (reduce #'+ (mapcar #'candle-close (subseq history 0 n))) n))))

(defun ind-ema (n history)
  (when (>= (length history) n)
    (let* ((k (float (/ 2 (+ n 1))))
           (seed (or (ind-sma n (reverse (subseq history 0 n))) (candle-close (first history))))
           (ema (float seed)))
      (dolist (c (reverse (subseq history 0 n)))
        (setf ema (float (+ (* (candle-close c) k) (* ema (- 1 k))))))
      ema)))

(defun ind-smma (n history)
  (when (>= (length history) n)
    (let* ((seed (or (ind-sma n (reverse (subseq history 0 n))) (candle-close (first history))))
           (smma (float seed)))
      (dolist (c (reverse (subseq history 0 n)))
        (let ((close (candle-close c)))
          (setf smma (float (/ (+ (* smma (- n 1)) close) n)))))
      smma)))

(defun ind-rsi (n history)
  (when (>= (length history) (1+ n))
    (let ((gains 0.0) (losses 0.0))
      (dotimes (i n)
        (let* ((curr (candle-close (nth i history)))
               (prev (candle-close (nth (1+ i) history)))
               (diff (- curr prev)))
          (if (> diff 0) (incf gains diff) (incf losses (abs diff)))))
      (float (if (zerop losses) 100 (- 100 (/ 100 (1+ (/ gains (max 0.00001 losses))))))))))

(defun ind-macd (fast slow signal history)
  (when (>= (length history) (+ slow signal))
    (let* ((fast-ema (ind-ema fast history))
           (slow-ema (ind-ema slow history))
           (macd (- fast-ema slow-ema))
           (signal-val 0.0))
      (values macd signal-val))))

(defun ind-bb (n dev history)
  (when (>= (length history) n)
    (let* ((sma (ind-sma n history))
           (closes (mapcar #'candle-close (subseq history 0 n)))
           (variance (float (/ (reduce #'+ (mapcar (lambda (x) (expt (- x sma) 2)) closes)) n)))
           (stddev (sqrt variance)))
      (values sma (+ sma (* dev stddev)) (- sma (* dev stddev))))))

(defun ind-stoch (k-n d-n history)
  (declare (ignore d-n))
  (when (>= (length history) k-n)
    (let* ((closes (subseq history 0 k-n))
           (low (reduce #'min (mapcar #'candle-low closes)))
           (high (reduce #'max (mapcar #'candle-high closes)))
           (curr (candle-close (first closes)))
           (k (float (if (= high low) 50 (* 100 (/ (- curr low) (max 0.00001 (- high low))))))))
      k)))

(defun ind-cci (n history)
  (when (>= (length history) n)
    (let* ((prices (mapcar (lambda (c) (float (/ (+ (candle-high c) (candle-low c) (candle-close c)) 3))) (subseq history 0 n)))
           (tp (first prices))
           (sma (float (/ (reduce #'+ prices) n)))
           (md (float (/ (reduce #'+ (mapcar (lambda (x) (abs (- x sma))) prices)) n))))
      (float (if (zerop md) 0 (/ (- tp sma) (* 0.015 md)))))))

(defun ind-atr (n history)
  (when (>= (length history) (1+ n))
    (let ((trs nil))
      (dotimes (i n)
        (let* ((c (nth i history)) (p (nth (1+ i) history))
               (tr (max (- (candle-high c) (candle-low c))
                        (abs (- (candle-high c) (candle-close p)))
                        (abs (- (candle-low c) (candle-close p))))))
          (push tr trs)))
      (float (/ (reduce #'+ trs) n)))))



;; 引数の数に依存しないクロス判定
(defun cross-above (a b &optional ap bp)
  (when (and (numberp a) (numberp b))
    (if (and (numberp ap) (numberp bp))
        (and (<= ap bp) (> a b))
        (> a b)))) ; 簡易判定

(defun cross-below (a b &optional ap bp)
  (when (and (numberp a) (numberp b))
    (if (and (numberp ap) (numberp bp))
        (and (>= ap bp) (< a b))
        (< a b))))


(defun get-hour-jst (timestamp)
  (nth 2 (multiple-value-list (decode-universal-time timestamp -9))))

(defun ind-session-high (start-h end-h history)
  "Get Highest High of the most recent session window (start-h to end-h JST)"
  (let ((max-h 0.0)
        (found nil))
    (dolist (c history)
      (let ((h (get-hour-jst (candle-timestamp c))))
        (if (and (>= h start-h) (< h end-h))
            (progn (setf found t) (setf max-h (max max-h (candle-high c)))))))
    (if found max-h (candle-close (first history)))))

(defun ind-session-low (start-h end-h history)
  "Get Lowest Low of the most recent session window (start-h to end-h JST)"
  (let ((min-l 1000000.0)
        (found nil))
    (dolist (c history)
      (let ((h (get-hour-jst (candle-timestamp c))))
        (if (and (>= h start-h) (< h end-h))
            (progn (setf found t) (setf min-l (min min-l (candle-low c)))))))
    (if found min-l (candle-close (first history)))))

(defparameter *allowed* '(+ - * / < > <= >= = and or not if 
                          ind-sma ind-ema ind-rsi ind-macd ind-bb ind-stoch ind-atr ind-cci
                          ind-ichimoku ind-donchian
                          ind-session-high ind-session-low
                          ind-kalman ind-kalman-velocity ind-kalman-trend
                          ind-smma is-staircase-pattern is-volume-increasing confirm-breakout
                          cross-above cross-below defstrategy sma ema rsi macd bb stoch atr cci kalman close high low open
                          session-high session-low ichimoku donchian))

(defun indicator-ref-p (sym)
  (let ((name (symbol-name sym)))
    (or (search "SMA-" name)
        (search "EMA-" name)
        (search "RSI-" name)
        (search "ATR-" name)
        (search "MACD-" name)
        (search "BB-" name)
        (search "STOCH-" name)
        (search "CCI-" name)
        (member sym '(close high low open)))))

;; V6.10: Added :category slot for explicit classification (Graham requirement)
;; V7.9++: Added :indicator-type slot for correct backtesting (Sharpe=-3.75 bug fix)
;; V8.0: Multi-Timeframe Revolution (Musk Order)
;; V8.9: Strategy Lineage Tracking (Pedigree)
;; V46.0: The Proving Grounds (Tier System) - Expert Panel 2026-01-18
;; V47.0: B/A/S Rank System - Owner's Vision 2026-01-21
;; V47.2: TF × Direction × Symbol Categorization - Owner's Vision 2026-01-21
(defstruct strategy name indicators entry exit (sl 0.0) (tp 0.0) (volume 0.01) 
            (sharpe 0.0) (profit-factor 0.0) (win-rate 0.0) (trades 0) (max-dd 0.0)
            (category :trend) (indicator-type "sma") (pnl-history nil) 
            (timeframe 1) (generation 0)
            (filter-enabled nil) (filter-tf "") (filter-period 0) (filter-logic "")
            (adapt-vector nil) (confidence-estimator "edge_ratio_v1")
            ;; Lifecycle Management (Phase 6)
            (consecutive-losses 0) (status :active) (status-reason "") (cooldown-until 0) (last-update 0)
            ;; The Proving Grounds Tiers: :graveyard, :incubator, :training, :battlefield
            (tier :incubator)
            ;; V47.0: B/A/S Rank System (:B, :A, :S, :legend, :graveyard)
            (rank nil)
            ;; V50.6: Legend再検証キューに入っている間の隔離フラグ
            (revalidation-pending nil)
            ;; V47.0: Breeding usage counter (max 3 before discard, Legend exempt)
            (breeding-count 0)
            ;; V49.2: Metadata tracking (Expert Panel)
            (regime-intent :unknown)
            ;; V17d: Multi-Currency Identity
            (symbol "USDJPY")
            ;; V47.2: Trade Direction (:BUY, :SELL, :BOTH)
            (direction :BOTH)
            ;; P10: Inactivity Pruning
            (last-signal-time 0)
            ;; P13: New Recruits Tracking
            (creation-time (get-universal-time))
            ;; V49.8: Stable Logic Hash for Graveyard Matching
            (hash nil)
            ;; V50.3: Validation Gates Metrics
            (oos-sharpe 0.0)
            (cpcv-median-sharpe 0.0)
            (cpcv-median-pf 0.0)
            (cpcv-median-wr 0.0)
            (cpcv-median-maxdd 0.0)
            (cpcv-pass-rate 0.0)
            ;; Phase 21: Breeding & Competition DNA (Survival of the Fittest)
            (age 0) (immortal nil) (parents nil))

(defmacro defstrategy (name &key indicators entry exit sl tp volume (category :trend) (indicator-type "sma") (timeframe 1) (generation 0) (filter-enabled nil) (regime-filter nil) (filter-tf "") (filter-period 0) (filter-logic "") (tier :incubator) (rank :incubator) (symbol "USDJPY") (direction :BOTH)
                         (sharpe 0.0) (profit-factor 0.0) (win-rate 0.0) (trades 0) (max-dd 0.0)
                         (oos-sharpe 0.0) (cpcv-median-sharpe 0.0) (cpcv-median-pf 0.0)
                         (cpcv-median-wr 0.0) (cpcv-median-maxdd 0.0) (cpcv-pass-rate 0.0)
                         (age 0) (immortal nil) (parents nil)
                         (revalidation-pending nil))
  `(make-strategy :name (string ',name) :indicators ',indicators :entry ',entry :exit ',exit :sl ,sl :tp ,tp :volume ,volume :category ,category :indicator-type ,indicator-type :timeframe ,timeframe :generation ,generation
                  :filter-enabled ,(or filter-enabled regime-filter) :filter-tf ,filter-tf :filter-period ,filter-period :filter-logic ,filter-logic :tier ,tier :rank ,rank :symbol ,symbol :direction ,direction
                  :sharpe ,sharpe :profit-factor ,profit-factor :win-rate ,win-rate :trades ,trades :max-dd ,max-dd
                  :oos-sharpe ,oos-sharpe :cpcv-median-sharpe ,cpcv-median-sharpe
                  :cpcv-median-pf ,cpcv-median-pf :cpcv-median-wr ,cpcv-median-wr
                  :cpcv-median-maxdd ,cpcv-median-maxdd :cpcv-pass-rate ,cpcv-pass-rate
                  :age ,age :immortal ,immortal :parents ,parents
                  :revalidation-pending ,revalidation-pending))

(defmacro with-trend-filter ((tf logic period) strategy-form)
  "Wraps a strategy definition with MTF trend filter parameters.
   Usage: (with-trend-filter (\"H1\" \"PRICE_ABOVE_SMA\" 50) (make-strategy ...))"
  (let ((strat-sym (gensym)))
    `(let ((,strat-sym ,strategy-form))
       (setf (strategy-filter-enabled ,strat-sym) t)
       (setf (strategy-filter-tf ,strat-sym) ,tf)
       (setf (strategy-filter-logic ,strat-sym) ,logic)
       (setf (strategy-filter-period ,strat-sym) ,period)
       ;; Append suffix to name to distinguish filtered version
       (setf (strategy-name ,strat-sym) 
             (format nil "~a-~a" (strategy-name ,strat-sym) ,tf))
       ,strat-sym)))

(defun validate (e) 
  (cond 
    ((atom e) (or (numberp e) (stringp e) (and (symbolp e) (or (member e *allowed*) (indicator-ref-p e) (keywordp e)))))
    ((listp e) (every #'validate e))))

(defun safe-read-dsl-form (code)
  "Safely read and validate DSL form. Returns form or NIL."
  (let ((form (swimmy.core:safe-read-sexp code :package :swimmy.school)))
    (when (and form (validate form)) form)))

(defun safe-eval-strategy (code-string)
  "Safely evaluate generated strategy code. Returns (strategy, valid-p)."
  (let ((opens (count #\( code-string))
        (closes (count #\) code-string)))
    (unless (= opens closes)
      (return-from safe-eval-strategy (values nil nil))))
  (unless (and (search ":indicators" code-string)
               (search ":entry" code-string)
               (search ":exit" code-string)
               (search ":sl" code-string)
               (search ":tp" code-string)
               (search ":volume" code-string))
    (return-from safe-eval-strategy (values nil nil)))
  (handler-case
      (let ((form (safe-read-dsl-form code-string)))
        (if (and (listp form)
                 (eq (car form) 'defstrategy))
            (values (eval form) t)
            (values nil nil)))
    (error (e)
      (format t "[DSL] Eval error: ~a~%" e)
      (values nil nil))))

(format t "[DSL] Loaded with CCI support~%")


;;; ══════════════════════════════════════════════════════════════════
;;;  ADVANCED LISP MACROS (Paul Graham Style)
;;; ══════════════════════════════════════════════════════════════════
;;; "Lisp is worth learning for the profound enlightenment experience
;;;  you will have when you finally get it." - Eric S. Raymond

;; Forward declarations
(defvar *constitution* nil)
(defvar *hall-of-fame* nil)
(defvar *current-regime* :unknown)
(defvar *current-volatility-state* :normal)
(defvar *daily-pnl* 0)
(defvar *danger-level* 0)

(defun get-current-context ()
  "Build context for constitution/elder checks"
  (list :regime *current-regime*
        :volatility-state *current-volatility-state*
        :daily-pnl *daily-pnl*
        :danger-level *danger-level*))

;;; ─────────────────────────────────────────
;;; MACRO: with-constitution-check
;;; ─────────────────────────────────────────
;;; Execute body only if the Constitution allows this action
(defmacro with-constitution-check (action &body body)
  "Execute body only if Constitution permits the action"
  `(let ((ctx (get-current-context)))
     (if (or (null *constitution*)
             (constitution-allows-p ,action ctx))
         (progn ,@body)
         (format t "[L] 📜 CONSTITUTION BLOCKED: ~a~%" ,action))))

;;; ─────────────────────────────────────────
;;; MACRO: with-elder-blessing
;;; ─────────────────────────────────────────
;;; Execute only if the Elders approve
(defmacro with-elder-blessing (proposal &body body)
  "Execute body only if Elders approve the proposal"
  `(let* ((ctx (get-current-context))
          (vote (if *hall-of-fame* (elder-vote ,proposal ctx) :approve)))
     (case vote
       (:approve (progn ,@body))
       (:caution 
        (format t "[L] 👴 Elders urge CAUTION for: ~a~%" ,proposal)
        (progn ,@body))
       (:reject
        (format t "[L] 👴 Elders REJECT: ~a~%" ,proposal)
        nil))))

;;; ─────────────────────────────────────────
;;; MACRO: defpattern
;;; ─────────────────────────────────────────
;;; Define a tribal market pattern with custom name
(defmacro defpattern (name description &body detection-body)
  "Define a tribal pattern. Body should return T if pattern is detected."
  `(define-pattern ,name ,description
     (lambda (history)
       (declare (ignorable history))
       ,@detection-body)))

;;; ----------------------------------------------------------------------------
;;; ICHIMOKU KINKO HYO (Equilibrium Chart)
;;; ----------------------------------------------------------------------------

(defun ind-ichimoku (tenkan-n kijun-n senkou-b-n history)
  "Calculates Ichimoku Kinko Hyo components. Returns (values tenkan kijun senkou-a senkou-b)."
  (if (> (length history) senkou-b-n)
      (let* ((get-hl (lambda (n)
                       (let ((slice (subseq history 0 n)))
                         (values (loop for c in slice maximize (candle-high c))
                                 (loop for c in slice minimize (candle-low c))))))
             (tenkan (multiple-value-bind (h l) (funcall get-hl tenkan-n) (/ (+ h l) 2.0)))
             (kijun (multiple-value-bind (h l) (funcall get-hl kijun-n) (/ (+ h l) 2.0)))
             (senkou-a (/ (+ tenkan kijun) 2.0))
             (senkou-b (multiple-value-bind (h l) (funcall get-hl senkou-b-n) (/ (+ h l) 2.0))))
        (values tenkan kijun senkou-a senkou-b))
      (values 0 0 0 0)))

;;; ----------------------------------------------------------------------------
;;; DONCHIAN CHANNELS (Price Action Structure)
;;; ----------------------------------------------------------------------------

(defun ind-donchian (n history)
  "Calculates Donchian Channel High/Low/Mid."
  (if (> (length history) n)
      (let* ((slice (subseq history 0 n))
             (upper (loop for c in slice maximize (candle-high c)))
             (lower (loop for c in slice minimize (candle-low c)))
             (middle (/ (+ upper lower) 2.0)))
        (values upper lower middle))
      (values 0 0 0)))

;;; ─────────────────────────────────────────
;;; MACRO: with-philosophy-log
;;; ─────────────────────────────────────────
;;; Automatically log the "Why" around an action
(defmacro with-philosophy-log (action-type what &body body)
  "Execute body and log philosophical reasoning"
  `(let ((result (progn ,@body)))
     (when (fboundp 'log-philosophy)
       (funcall 'log-philosophy ,action-type ,what (get-current-context)))
     result))

;;; ─────────────────────────────────────────
;;; MACRO: aif (Anaphoric If - Paul Graham classic)
;;; ─────────────────────────────────────────
(defmacro aif (test then &optional else)
  "Anaphoric if - binds result of test to IT"
  `(let ((it ,test))
     (if it ,then ,else)))

;;; ─────────────────────────────────────────
;;; MACRO: awhen (Anaphoric When)
;;; ─────────────────────────────────────────
(defmacro awhen (test &body body)
  "Anaphoric when - binds result of test to IT"
  `(let ((it ,test))
     (when it ,@body)))

(format t "[DSL] Advanced Macros loaded (PG-style)~%")
