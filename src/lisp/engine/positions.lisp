;;; ============================================================================
;;; engine/positions.lisp - Execution Logic & State Management
;;; ============================================================================
;;; Manages Arm State, Position Lifecycle, and Persistence.
;;; Part of "The Efficient Gardener" refactoring (Phase 6)
;;;
;;; Dependencies: 
;;;   - engine/signals.lisp (Analysis)
;;;   - engine/risk.lisp (Authority)
;;;   - engine/portfolio.lisp (Optimization)
;;; ============================================================================

(in-package :swimmy.engine)

;;; ============================================================================
;;; GLOBAL STATE
;;; ============================================================================

(defparameter *arms* nil "List of all strategy arms")
(defparameter *arm-states* (make-hash-table) "Runtime state of each arm")
(defparameter *portfolio-indices* '(0) "Indices of active strategies")
(defparameter *genome* nil "Raw genome data")
(defparameter *memory* nil "Long-term memory")

;;; ==========================================
;;; DISCORD ALERTS
;;; ==========================================

;; For execution alerts (optional, can be moved to shell/notifications hook)
(defun notify-execution (action symbol idx size pnl)
  "Notify shell about execution (via hook)."
  (cond
    ((string= action "OPEN")
     (when (fboundp 'on-trade-opened)
       (on-trade-opened idx symbol size nil 0))) ; Pass warmup-p as nil for now
    ((string= action "CLOSE")
     (when (fboundp 'on-trade-closed)
       (on-trade-closed idx symbol (> pnl 0) pnl)))))

;;; ==========================================
;;; GENOME PERSISTENCE
;;; ==========================================

(defun load-genome ()
  "Load genome from file."
  (with-open-file (in *genome-path* :direction :input :if-does-not-exist nil)
    (when in
      (setf *genome* (read in))
      (setf *arms* (getf *genome* :arms))
      (setf *portfolio-indices* (or (getf *genome* :portfolio-indices) (list 0)))
      (setf *memory* (getf *genome* :memory))
      (format t "[ENGINE] Loaded ~d arms. Portfolio: ~a~%" (length *arms*) *portfolio-indices*))))

(defun save-genome ()
  "Save genome to file."
  (setf (getf *genome* :arms) *arms*)
  (setf (getf *genome* :portfolio-indices) *portfolio-indices*)
  (setf (getf *genome* :memory) *memory*)
  (with-open-file (out *genome-path* :direction :output :if-exists :supersede)
    (write *genome* :stream out :pretty t)))

;;; ==========================================
;;; ARM STATE MANAGEMENT
;;; ==========================================

(defun arm-benched-p (idx)
  "Check if arm is benched."
  (member idx *benched-arms*))

(defun bench-arm (idx)
  "Bench an arm due to consecutive losses."
  (unless (member idx *benched-arms*)
    (push idx *benched-arms*)
    (format t "[ENGINE] Arm ~d benched~%" idx)))

(defun update-arm-stats (idx won pnl)
  "Update arm statistics after a trade."
  (incf *total-trades*)
  (when (boundp '*daily-trade-count*)
    (incf *daily-trade-count*))
  (incf *daily-pnl* pnl)
  (let* ((arm (nth idx *arms*)) (stats (cdr arm))
         (a (ensure-real (car stats))) (b (ensure-real (cdr stats)))
         (state (gethash idx *arm-states*)))
    (if won
        (progn (incf a) (setf (arm-state-streak state) 0))
        (progn (incf b) (decf (arm-state-streak state))))
    (setf (nth idx *arms*) (cons (car arm) (cons a b)))
    (save-genome)
    
    ;; Risk update
    (when (fboundp 'update-drawdown)
      (update-drawdown pnl))
      
    (when (fboundp 'update-nn-threshold)
      (update-nn-threshold won))
    
    ;; Neural training
    (let ((pos (arm-state-position state)))
      (when (fboundp 'train-neural)
        (cond
          ((and won (eql pos :LONG)) (train-neural 0))
          ((and won (eql pos :SHORT)) (train-neural 1))
          ((and (not won) (eql pos :LONG)) (train-neural 1))
          ((and (not won) (eql pos :SHORT)) (train-neural 0)))))
          
    ;; Bench check
    (when (<= (arm-state-streak state) (- *max-streak-losses*))
      (bench-arm idx))
    
    ;; Rebalance (using engine/portfolio.lisp)
    (when (fboundp 'rebalance-portfolio)
      (rebalance-portfolio))))

;;; ==========================================
;;; EXECUTION LOOP
;;; ==========================================

(defun check-arm (idx symbol bid ask)
  "Check arm for entry/exit signals. Core execution logic."
  ;; 1. Global Risk Check
  (when (and (if (fboundp 'trading-allowed-p) (trading-allowed-p) t)
             (not (arm-benched-p idx))) ; 2. Arm Health Check
    
    (let* ((params (car (nth idx *arms*)))
           (sma-s (nth 0 params))
           (sma-l (nth 1 params))
           (sl-p (nth 2 params))
           (tp-p (nth 3 params))
           ;; Genome vol ignored in favor of risk module
           (state (or (gethash idx *arm-states*)
                      (setf (gethash idx *arm-states*) (make-arm-state :streak 0))))
           (active (member idx *portfolio-indices*)))
      
      ;; 3. Check Exits (Priority)
      (when (arm-state-position state)
        (let ((pos (arm-state-position state))
              (entry (arm-state-entry-price state))
              (pnl 0) 
              (closed nil)
              ;; Re-calc SMAs for exit check (Dead Cross)
              (s-now (calculate-sma sma-s *candle-history*))
              (l-now (calculate-sma sma-l *candle-history*))
              (s-prev (calculate-sma sma-s (cdr *candle-history*)))
              (l-prev (calculate-sma sma-l (cdr *candle-history*))))
          
          (when (and s-now l-now s-prev l-prev)
            (cond
              ((eql pos :LONG)
               (when (or (<= bid (arm-state-sl state))        ; SL
                         (>= bid (arm-state-tp state))        ; TP
                         (and (> s-prev l-prev) (< s-now l-now))) ; Dead Cross
                 (setf pnl (- bid entry) closed t)))
              ((eql pos :SHORT)
               (when (or (>= ask (arm-state-sl state))        ; SL
                         (<= ask (arm-state-tp state))        ; TP
                         (and (< s-prev l-prev) (> s-now l-now))) ; Golden Cross (Exit Short)
                 (setf pnl (- entry ask) closed t))))
            
            (when closed
              (update-arm-stats idx (> pnl 0) pnl)
              (setf (arm-state-position state) nil)
              (when active
                (let ((payload `((type . "CLOSE")
                                 (symbol . ,symbol))))
                  (pzmq:send *cmd-publisher*
                             (swimmy.core::sexp->string payload :package *package*)))
                (notify-execution "CLOSE" symbol idx 0 pnl))))))
      
      ;; 4. Check Entries (using signals.lisp)
      (unless (arm-state-position state)
        (let ((signal (generate-signal params *candle-history*)))
          ;; signal: (TYPE conf sl tp vol)
          (when signal
            (let ((type (first signal))
                  (conf (second signal))
                  (sl-p (nth 2 params)) ; Re-use params for reliable indexing
                  (tp-p (nth 3 params)))
              
              ;; 5. Dynamic Sizing (Risk Authority)
              (let ((dynamic-vol (if (fboundp 'calculate-lot-size) 
                                     (calculate-lot-size) 
                                     0.01)))
                
                ;; 6. Hard Constraints (Taleb Rules)
                (when (if (fboundp 'check-hard-constraints)
                          (check-hard-constraints dynamic-vol symbol)
                          t)
                  
                  (cond
                    ((eql type :BUY)
                     (setf (arm-state-position state) :LONG
                           (arm-state-entry-price state) bid
                           (arm-state-size state) dynamic-vol
                           (arm-state-symbol state) symbol
                           (arm-state-sl state) (- bid sl-p)
                           (arm-state-tp state) (+ bid tp-p))
                     (when active
                       (let* ((order (swimmy.core:make-order-message
                                      "Engine" symbol "BUY" dynamic-vol 0.0
                                      (arm-state-sl state) (arm-state-tp state)))
                              (msg (swimmy.core::sexp->string order :package :swimmy.core)))
                         (pzmq:send *cmd-publisher* msg))
                       (notify-execution "OPEN" symbol idx dynamic-vol 0)))
                    
                    ((eql type :SELL)
                     (setf (arm-state-position state) :SHORT
                           (arm-state-entry-price state) ask
                           (arm-state-size state) dynamic-vol
                           (arm-state-symbol state) symbol
                           (arm-state-sl state) (+ ask sl-p)
                           (arm-state-tp state) (- ask tp-p))
                     (when active
                       (let* ((order (swimmy.core:make-order-message
                                      "Engine" symbol "SELL" dynamic-vol 0.0
                                      (arm-state-sl state) (arm-state-tp state)))
                              (msg (swimmy.core::sexp->string order :package :swimmy.core)))
                         (pzmq:send *cmd-publisher* msg))
                       (notify-execution "OPEN" symbol idx dynamic-vol 0)))))))))))))

(format t "[ENGINE] positions.lisp loaded~%")
