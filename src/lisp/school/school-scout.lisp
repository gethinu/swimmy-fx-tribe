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

;;; ----------------------------------------------------------------------------
;;; GUARDIAN INTERFACE (PIPE)
;;; ----------------------------------------------------------------------------

(defun ensure-guardian-running ()
  "Start Guardian if not running."
  (unless (and *guardian-process* (uiop:process-alive-p *guardian-process*))
    (format t "[SCOUT] 🛡️ Launching Persistent Guardian...~%")
    (setf *guardian-process* 
          (uiop:launch-program '("guardian/target/release/guardian")
                               :input :stream
                               :output :stream))
    (setf *guardian-stream-in* (uiop:process-info-input *guardian-process*))
    (setf *guardian-stream-out* (uiop:process-info-output *guardian-process*))
    (format t "[SCOUT] ✅ Guardian Connected.~%")))

(defun send-guardian-request (payload)
  "Send JSON payload to Guardian and read response."
  (ensure-guardian-running)
  (handler-case
      (progn
        ;; Write JSON
        (write-line (jsown:to-json payload) *guardian-stream-in*)
        (force-output *guardian-stream-in*)
        
        ;; Read Response
        (let ((line (read-line *guardian-stream-out* nil nil)))
          (if line
              (jsown:parse line)
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
  ;; 80% Imitation, 20% Random
  (if (and wisdom (> (length wisdom) 0) (< (random 1.0) 0.8))
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

(defun generate-random-scout (attempt)
  "Generate purely random parameters."
  (let ((tfs '(1 5 15 30 60 240 1440 10080))
        (short (+ 5 (random 45)))
        (long-p (+ 50 (random 150))))
    (make-scout-struct
     :name (format nil "Recruit-Rnd-~d-~d" (get-universal-time) attempt)
     :timeframe (nth (random (length tfs)) tfs)
     :sma-short short
     :sma-long long-p
     :sl (+ 0.01 (random 0.09))
     :tp (+ 0.01 (random 0.14))))) ; V21: Symbol assigned by Combine

(defstruct scout-struct name timeframe sma-short sma-long sl tp symbol)

(defun scout-to-json-payload (scout &optional override-symbol)
  "Convert scout struct to Guardian Request JSON object."
  (let* ((tf (scout-struct-timeframe scout))
         (tf-str (case tf
                   (60 "H1") (240 "H4") (1440 "D1") (10080 "W1")
                   (t (format nil "M~d" tf))))
         ;; V21: The Combine (Override Symbol)
         (symbol (or override-symbol (scout-struct-symbol scout) "USDJPY"))
         (path (format nil "data/historical/~a_~a.csv" symbol tf-str)))
    
    (format t "[SCOUT] 🎲 Target: ~a (~a)~%" symbol tf-str)
    
    (if (or (uiop:file-exists-p path) (scan-for-csv symbol tf-str)) 
        (list :obj 
          (cons "action" "BACKTEST")
          (cons "strategy" (list :obj 
                         (cons "name" (scout-struct-name scout))
                         (cons "timeframe" tf)
                         (cons "sma_short" (scout-struct-sma-short scout))
                         (cons "sma_long" (scout-struct-sma-long scout))
                         (cons "sl" (scout-struct-sl scout))
                         (cons "tp" (scout-struct-tp scout))
                         (cons "volume" 0.01)
                         (cons "indicator_type" "sma")
                         (cons "filter_enabled" :f))) ;; False
          (cons "candles_file" path)
          (cons "strategy_name" (scout-struct-name scout))
          (cons "symbol" symbol)
          (cons "timeframe" tf)
          (cons "from_date" "2024-01-01")
          (cons "to_date" "2025-01-01")
          (cons "spread" 10)
          (cons "balance" 10000))
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
        (best-sharpe -10.0)
        (best-trades 0))
    
    (dolist (sym *combine-currencies*)
      (let ((payload (scout-to-json-payload candidate sym)))
        (when payload
          (let ((resp (send-guardian-request payload)))
            (when (and resp (equal (jsown:val resp "type") "BACKTEST_RESULT"))
              (let* ((res (jsown:val resp "result"))
                     (sharpe (if (jsown:keyp res "sharpe") (jsown:val res "sharpe") 0.0))
                     (trades (if (jsown:keyp res "trades") (jsown:val res "trades") 0)))
                
                ;; (format t "[COMBINE] ~a: Sharpe ~a, Trades ~d~%" sym sharpe trades)
                
                (when (> sharpe best-sharpe)
                  (setf best-sharpe sharpe)
                  (setf best-trades trades)
                  (setf best-symbol sym))))))))
    
    (if (and best-symbol (>= best-sharpe *min-sharpe*) (> best-trades 1))
        (list best-symbol best-sharpe best-trades)
        nil)))

(defun recruit-scout ()
  "Run the recruitment process in Lisp (V21: The Combine)."
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
      (let ((c-tf (getf (cdr corpse) :timeframe))
            (c-short (getf (cdr corpse) :sma-short))
            (c-long (getf (cdr corpse) :sma-long)))
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
                   :status :active)))
      
      ;; Add stats manually to the struct if supported, or just trust the backtest
      ;; (strategy-sharpe strat) isn't in default make-strategy unless defined?
      ;; It seems 'sharpe' slot exists in strategies.lisp
      
      (push strat *strategy-knowledge-base*)
      (save-recruit-to-lisp strat)
      ;; (format t "Drafted ~a (Sharpe: ~,2f)~%" name sharpe)
      )))
