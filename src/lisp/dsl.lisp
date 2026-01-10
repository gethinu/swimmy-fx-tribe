;; dsl.lisp - Strategy DSL

(in-package :swimmy.school)

;; Forward declarations to suppress warnings (actual values set in brain.lisp)
(defvar *candle-history* nil)
(defvar *cmd-publisher* nil)
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

(defparameter *allowed* '(+ - * / < > <= >= = and or not if 
                          ind-sma ind-ema ind-rsi ind-macd ind-bb ind-stoch ind-atr ind-cci
                          ind-kalman ind-kalman-velocity ind-kalman-trend
                          cross-above cross-below defstrategy sma ema rsi macd bb stoch atr cci kalman close high low open))

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
(defstruct strategy name indicators entry exit sl tp volume (sharpe 0.0) (category :trend) (indicator-type "sma") (pnl-history nil) (timeframe 1))

(defmacro defstrategy (name &key indicators entry exit sl tp volume (category :trend) (indicator-type "sma") (timeframe 1))
  `(make-strategy :name ,name :indicators ',indicators :entry ',entry :exit ',exit :sl ,sl :tp ,tp :volume ,volume :sharpe 0.0 :category ,category :indicator-type ,indicator-type :timeframe ,timeframe))

(defun validate (e) 
  (cond 
    ((atom e) (or (numberp e) (stringp e) (and (symbolp e) (or (member e *allowed*) (indicator-ref-p e) (keywordp e)))))
    ((listp e) (every #'validate e))))

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
      (let ((form (read-from-string code-string)))
        (if (and (listp form)
                 (eq (car form) 'defstrategy)
                 (validate form))
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

;;; ─────────────────────────────────────────
;;; MACRO: with-tribe-context
;;; ─────────────────────────────────────────
;;; Establish trading context with clan awareness
(defmacro with-tribe-context ((symbol direction clan) &body body)
  "Execute trading action with tribal context"
  `(let ((*current-clan* (get-clan ,clan))
         (*current-symbol* ,symbol)
         (*current-direction* ,direction))
     (format t "[L] ~a prepares for ~a on ~a~%"
             (clan-name *current-clan*) ,direction ,symbol)
     ,@body))

;;; ─────────────────────────────────────────
;;; MACRO: log-philosophy
;;; ─────────────────────────────────────────
;;; Automatically log the "Why" around an action
(defmacro log-philosophy (action-type what &body body)
  "Execute body and log philosophical reasoning"
  `(let ((result (progn ,@body)))
     (when (boundp 'log-philosophy)
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

