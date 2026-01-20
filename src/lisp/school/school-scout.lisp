;;; src/lisp/school/school-scout.lisp
;;; ============================================================================
;;; SCHOOL SCOUT (Phase 11: True Acceleration)
;;; ============================================================================
;;; Replaces tools/recruit_elite.py with native Lisp implementation.
;;; - Connects directly to Guardian (Rust) via Pipe.
;;; - Maintains persistent Guardian process for CSV caching.
;;; - Utilizes Wisdom (strategies_optimized.json) for smart imitation.
;;; ============================================================================

(in-package :swimmy.school)

;;; ----------------------------------------------------------------------------
;;; STATE & CONFIG
;;; ----------------------------------------------------------------------------

(defparameter *wisdom-file* "strategies_optimized.json")
(defparameter *guardian-process* nil "Persistent Guardian subprocess")
(defparameter *guardian-stream-in* nil "Stdin for Guardian")
(defparameter *guardian-stream-out* nil "Stdout for Guardian")

(defparameter *scout-attempts* 10 "Number of attempts per recruit call")
(defparameter *min-sharpe* 0.1 "Minimum Sharpe to recruit")
(defparameter *graveyard-file* "data/memory/graveyard.sexp" "Path to persistence file")
(defparameter *graveyard* nil "Memory of failed strategies")

;;; ----------------------------------------------------------------------------
;;; GUARDIAN INTERFACE (PIPE)
;;; ----------------------------------------------------------------------------

(defun ensure-guardian-running ()
  "Start Guardian if not running."
  (unless (and *guardian-process* (uiop:process-alive-p *guardian-process*))
    (format t "[SCOUT] 🛡️ Launching Persistent Guardian...~%")
    (setf *guardian-process* 
          (uiop:launch-program '("guardian/target/release/guardian" "--backtest-only")
                               :input :stream
                               :output :stream))
    (setf *guardian-stream-in* (uiop:process-info-input *guardian-process*))
    (setf *guardian-stream-out* (uiop:process-info-output *guardian-process*))
    (format t "[SCOUT] ✅ Guardian Connected.~%")))

(defun send-guardian-request (payload-sexp)
  "Send S-Exp payload to Guardian and read response."
  (ensure-guardian-running)
  (handler-case
      (progn
        ;; Write S-Exp string
        (write-line (sexp-to-string payload-sexp) *guardian-stream-in*)
        (force-output *guardian-stream-in*)
        
        ;; Read Response (Native Read)
        (let ((line (read-line *guardian-stream-out* nil nil)))
          (if line
              (read-from-string line) ; Native read!
              (progn
                (format t "[SCOUT] ⚠️ Guardian returned EOF. Restarting...~%")
                (terminate-guardian)
                nil))))
    (error (e)
      (format t "[SCOUT] 💥 Guardian Communication Error: ~a~%" e)
      (terminate-guardian)
      nil)))


(defun terminate-guardian ()
  "Kill the Guardian process and close streams to prevent FD leaks."
  (when *guardian-stream-in*
    (ignore-errors (close *guardian-stream-in*))
    (setf *guardian-stream-in* nil))
  (when *guardian-stream-out*
    (ignore-errors (close *guardian-stream-out*))
    (setf *guardian-stream-out* nil))
  (when *guardian-process*
    (uiop:terminate-process *guardian-process*)
    ;; Optional: (uiop:wait-process *guardian-process*) for zombies, but might block
    (setf *guardian-process* nil)))

;;; ----------------------------------------------------------------------------
;;; WISDOM LOADING
;;; ----------------------------------------------------------------------------

(defun load-wisdom ()
  "Load optimized parameters from JSON."
  (if (uiop:file-exists-p *wisdom-file*)
      (let ((content (uiop:read-file-string *wisdom-file*)))
        (handler-case
            (jsown:parse content)
          (error (e)
            (format t "[SCOUT] ⚠️ Failed to parse wisdom: ~a~%" e)
            nil)))
      nil))

;;; ----------------------------------------------------------------------------
;;; GENERATION LOGIC
;;; ----------------------------------------------------------------------------

(defun generate-scout-candidate (wisdom attempt)
  "Generate a candidate strategy, imitating wisdom or random."
  ;; Phase 8: Inverted Bias (20% Imitation, 80% Random)
  ;; To force M5/M15 pivot, we must ignore the D1-heavy wisdom for now.
  (if (and wisdom (> (length wisdom) 0) (< (random 1.0) 0.2))
      (mutate-wisdom-elite (nth (random (length wisdom)) wisdom) attempt)
      (generate-random-scout attempt)))

(defun mutate-wisdom-elite (elite attempt)
  "Clone and mutate an elite JSON object."
  ;; elite is jsown obj: (:obj ("name" . "...") ("sma_short" . 10) ...)
  (let* ((short (jsown:val elite "sma_short"))
         (long-p (jsown:val elite "sma_long"))
         (tf (jsown:val elite "timeframe"))
         (sl (jsown:val elite "sl"))
         (tp (jsown:val elite "tp")))
    
    ;; Mutate
    (setf short (max 5 (round (* short (+ 0.9 (random 0.2))))))
    (setf long-p (max (+ short 5) (round (* long-p (+ 0.9 (random 0.2))))))
    (setf sl (max 0.01 (min 0.1 (* sl (+ 0.9 (random 0.2))))))
    (setf tp (max 0.01 (min 0.2 (* tp (+ 0.9 (random 0.2))))))
    
    (make-scout-struct 
     :name (format nil "Recruit-Elite-~d-~d" (get-universal-time) attempt)
     :timeframe tf
     :sma-short short
     :sma-long long-p
     :sl sl
     :tp tp)))

(defun pick-weighted-timeframe ()
  "Select a timeframe based on Expert Panel's High-Frequency Pivot (Phase 8).
   Weights: M5 (40%), M15 (30%), H1 (20%), D1 (10%)"
  (let ((r (random 1.0)))
    (cond
      ((< r 0.40) 5)     ; 40%
      ((< r 0.70) 15)    ; 30%
      ((< r 0.90) 60)    ; 20%
      (t 1440))))        ; 10%

(defun generate-random-scout (attempt)
  "Generate purely random parameters with Phase 8 Bias."
  (let ((short (+ 5 (random 45)))
        (long-p (+ 50 (random 150))))
    (make-scout-struct
     :name (format nil "Recruit-Rnd-~d-~d" (get-universal-time) attempt)
     :timeframe (pick-weighted-timeframe)
     :sma-short short
     :sma-long long-p
     :sl (+ 0.01 (random 0.09))
     :tp (+ 0.01 (random 0.14)))))

(defstruct scout-struct name timeframe sma-short sma-long sl tp symbol)

;; V13: Native S-Expression Serializer
(defun sexp-to-string (obj)
  "Serialize Lisp object to S-Expression string compatible with serde-lexpr."
  (cond
    ((null obj) "()")
    ((eq obj t) "#t")
    ((eq obj :f) "#f") ; Special keyword for false
    ((stringp obj) (format nil "~s" obj))
    ((integerp obj) (format nil "~d" obj))
    ((floatp obj) (format nil "~f" obj))
    ((keywordp obj) (string-downcase (symbol-name obj)))
    ((symbolp obj) (string-downcase (symbol-name obj)))
    ((consp obj)
     (if (listp (cdr obj))
         ;; Proper List
         (format nil "(~{~a~^ ~})" (mapcar #'sexp-to-string obj))
         ;; Dotted Pair
         (format nil "(~a . ~a)" (sexp-to-string (car obj)) (sexp-to-string (cdr obj)))))
    (t (format nil "~a" obj))))

(defun scout-to-sexp-payload (scout &optional override-symbol)
  "Convert scout struct to Guardian Request S-Expression."
  (let* ((tf (scout-struct-timeframe scout))
         (tf-str (case tf
                   (60 "H1") (240 "H4") (1440 "D1") (10080 "W1")
                   (t (format nil "M~d" tf))))
         (symbol (or override-symbol (scout-struct-symbol scout) "USDJPY"))
         (path (format nil "data/historical/~a_~a.csv" symbol tf-str)))
    
    ;(format t "[SCOUT] 🎲 Target: ~a (~a)~%" symbol tf-str)
    
    (if (or (uiop:file-exists-p path) (scan-for-csv symbol tf-str)) 
        (list
          ;; Action (String) -> Dotted
          (cons 'action "BACKTEST")
          ;; Strategy (Struct) -> Dotted
          (cons 'strategy 
                (list
                  (cons 'name (scout-struct-name scout))
                  ;; Timeframe omitted from Strategy struct
                  (cons 'sma_short (scout-struct-sma-short scout))
                  (cons 'sma_long (scout-struct-sma-long scout))
                  (cons 'sl (scout-struct-sl scout))
                  (cons 'tp (scout-struct-tp scout))
                  (cons 'volume 0.01)
                  (cons 'indicator_type 'sma) ; Symbol
                  (cons 'filter_enabled (if (< tf 240) t :f)) ; #t or #f
                  ;; Defaults for filter_... omitted
                  ))
          ;; Option<String> -> List (key val)
          (list 'candles_file path)
          (list 'timeframe tf)

          (cons 'strategy_name (scout-struct-name scout))
          (cons 'symbol symbol))
        nil)))

(defun scan-for-csv (symbol tf-str)
  "Fallback to check if file exists (todo: robust path finding)"
  ;; In this simple version we assume path is correct or fail
  nil)

;;; ----------------------------------------------------------------------------
;;; MAIN RECRUIT LOGIC
;;; ----------------------------------------------------------------------------

(defparameter *combine-currencies* '("USDJPY" "EURUSD" "GBPUSD")
  "The Combine: Currencies to test every recruit against.")

(defun run-combine-test (candidate)
  "Run The Combine: Test candidate on all major currencies.
   Returns (best-symbol best-sharpe best-trades) or NIL if all fail."
  (let ((best-symbol nil)
        (best-sharpe *min-sharpe*)
        (best-trades 0))
    
    (dolist (sym *combine-currencies*)
      (let ((payload (scout-to-sexp-payload candidate sym)))
        (when payload
          (let ((resp (send-guardian-request payload)))
            ;; Resp format: ((TYPE . "BACKTEST_RESULT") (RESULT . ((SHARPE . x) ...)))
            ;; Keys are Symbols (upcased). Use default EQL test.
            (if (and (consp resp) 
                     (stringp (cdr (assoc 'type resp))) 
                     (string= (cdr (assoc 'type resp)) "BACKTEST_RESULT"))
                (let* ((res (cdr (assoc 'result resp)))
                       (sharpe (or (cdr (assoc 'sharpe res)) 0.0))
                       (trades (or (cdr (assoc 'trades res)) 0)))
                  
                  (when (> sharpe best-sharpe)
                    (setf best-sharpe sharpe)
                    (setf best-trades trades)
                    (setf best-symbol sym)))
                ;; Else: ERROR
                (when resp
                   (format t "[COMBINE] ⚠️ Guardian Error: ~a~%" resp)))))))
    
    ;; Production Logic: Enforce Min Sharpe and Activity
    (if (and best-symbol (>= best-sharpe *min-sharpe*) (> best-trades 1))
        (list best-symbol best-sharpe best-trades)
        (progn
          ;; V22: Learn from failure
          (add-to-graveyard candidate)
          nil))))

(defun add-to-graveyard (scout)
  "Register failed parameters to avoid retrying them (Permanent)."
  (unless (is-toxic-params scout)
    (let ((corpse (list 
                    :name (scout-struct-name scout)
                    :timeframe (scout-struct-timeframe scout)
                    :sma-short (scout-struct-sma-short scout)
                    :sma-long (scout-struct-sma-long scout))))
      (push corpse *graveyard*)
      ;; Limit size to avoid bloat (e.g. 10000 items)
      (when (> (length *graveyard*) 10000)
        (setf *graveyard* (subseq *graveyard* 0 10000)))
      (save-graveyard))))

(defun save-graveyard ()
  "Persist graveyard to disk."
  (ensure-directories-exist *graveyard-file*)
  (with-open-file (out *graveyard-file* 
                       :direction :output 
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (write *graveyard* :stream out)))

(defun load-graveyard ()
  "Load graveyard from disk."
  (if (probe-file *graveyard-file*)
      (with-open-file (in *graveyard-file* :direction :input)
        (setf *graveyard* (read in nil nil))
        (format t "[SCOUT] 🪦 LIFTED CURSE: Loaded ~d failed strategies from graveyard.~%" (length *graveyard*)))
      (setf *graveyard* nil)))

(defun recruit-scout ()
  "Run the recruitment process in Lisp (V21: The Combine)."
  (load-graveyard) ;; Ensure memory is fresh
  (format t "[SCOUT] 🏟️ THE COMBINE IS RUNNING (Multi-Currency Tryout)...~%")
  (let ((wisdom (load-wisdom))
        (recruited 0))
    
    (dotimes (i *scout-attempts*)
      (let ((candidate (generate-scout-candidate wisdom i)))
        
        ;; Check Graveyard FIRST (Optimization)
        ;; Note: Toxic params check ignores symbol, so it works.
        (unless (is-toxic-params candidate)
          ;; Run The Combine
          (let ((result (run-combine-test candidate)))
            (when result
              (destructuring-bind (sym sharpe trades) result
                (format t "[SCOUT] 🏆 Combine Winner: ~a (Sharpe: ~,2f)~%" sym sharpe)
                ;; Assign Winner Symbol
                (setf (scout-struct-symbol candidate) sym)
                (register-recruit candidate sharpe trades)
                (incf recruited)))))))
    
    (when (> recruited 0)
      (format t "[SCOUT] 📬 Recruited ~d new Founders!~%" recruited))
    recruited))

(defun is-toxic-params (scout)
  "Check against *graveyard* memory."
  (when (boundp '*graveyard*)
    (dolist (corpse *graveyard*)
      ;; Corpse format: '(:name "..." :timeframe 15 :sma-short 10 ...)
      ;; This is a plist.
      (let ((c-tf (getf corpse :timeframe))
            (c-short (getf corpse :sma-short))
            (c-long (getf corpse :sma-long)))
        (when (and (= c-tf (scout-struct-timeframe scout))
                   (= c-short (scout-struct-sma-short scout))
                   (= c-long (scout-struct-sma-long scout)))
          (return-from is-toxic-params t)))))
  nil)

(defun register-recruit (scout sharpe trades)
  "Convert scout to full Strategy object and push to KB."
  (let ((name (scout-struct-name scout))
        (tf (scout-struct-timeframe scout))
        (s (scout-struct-sma-short scout))
        (l (scout-struct-sma-long scout)))
    
    (let ((strat (make-strategy
                   :name name
                   :category :trend
                   :timeframe tf
                   :generation 0
                   :sl (scout-struct-sl scout)
                   :tp (scout-struct-tp scout)
                   :volume 0.01
                   :indicators (list (format nil "SMA-~d" s) (format nil "SMA-~d" l))
                   :entry (format nil "CROSS SMA ~d ~d" s l)
                   :exit (format nil "TP/SL")
                   :tier :incubator
                   :symbol (scout-struct-symbol scout) ;; V20: Persist Symbol
                   :status :active
                   ;; Phase 8: Context Injection (Safety Net)
                   ;; Enforce H1 Filter for any strategy faster than H4
                   :filter-enabled (if (< tf 240) t nil)
                   :filter-tf (if (< tf 240) "H1" "")
                   :filter-period (if (< tf 240) 50 0)
                   :filter-logic (if (< tf 240) "PRICE_ABOVE_SMA" ""))))
      
      ;; Add stats manually to the struct if supported, or just trust the backtest
      ;; (strategy-sharpe strat) isn't in default make-strategy unless defined?
      ;; It seems 'sharpe' slot exists in strategies.lisp
      
      (push strat *strategy-knowledge-base*)
      (save-recruit-to-lisp strat)
      ;; (format t "Drafted ~a (Sharpe: ~,2f)~%" name sharpe)
      )))
