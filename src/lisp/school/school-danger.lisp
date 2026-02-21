;; school-danger.lisp - Danger Avoidance System
(in-package :swimmy.school)

;; ArmadaAC Core runtime guardrails (behavioral replica).
(defun armada-kill-env-value (key)
  "Read env value from process env (and .env fallback when available)."
  (or (ignore-errors
        (when (fboundp 'swimmy.core::getenv-or-dotenv)
          (swimmy.core::getenv-or-dotenv key)))
      (ignore-errors (uiop:getenv key))))

(defun armada-kill-env-bool-or (key default)
  "Parse boolean env KEY with DEFAULT fallback."
  (let ((raw (armada-kill-env-value key)))
    (if (and (stringp raw) (> (length raw) 0))
        (not (null (member (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return) raw))
                           '("1" "true" "yes" "on" "y" "t")
                           :test #'string=)))
        default)))

(defun armada-kill-env-float-or (key default)
  "Parse numeric env KEY as float with DEFAULT fallback."
  (let* ((raw (armada-kill-env-value key))
         (num (and (stringp raw)
                   (> (length raw) 0)
                   (ignore-errors (safe-parse-number raw)))))
    (if (numberp num)
        (float num 1.0)
        (float default 1.0))))

(defun armada-kill-env-int-or (key default)
  "Parse integer env KEY with DEFAULT fallback."
  (let* ((raw (armada-kill-env-value key))
         (num (and (stringp raw)
                   (> (length raw) 0)
                   (ignore-errors (parse-integer raw :junk-allowed t)))))
    (if (integerp num) num default)))

(defparameter *armada-kill-canary-mode-enabled*
  (armada-kill-env-bool-or "SWIMMY_ARMADA_CANARY_MODE" nil)
  "When T, force Armada kill-switch canary preset.")

(defparameter *armada-kill-switch-enabled*
  (armada-kill-env-bool-or "SWIMMY_ARMADA_KILL_SWITCH_ENABLED" nil)
  "When T, enforce Armada runtime kill switch.")
(defparameter *armada-hard-dd-percent*
  (armada-kill-env-float-or "SWIMMY_ARMADA_HARD_DD_PERCENT" 12.0)
  "Hard drawdown cap (% equity). Breach triggers immediate pause.")
(defparameter *armada-weekly-dd-percent*
  (armada-kill-env-float-or "SWIMMY_ARMADA_WEEKLY_DD_PERCENT" 4.0)
  "Weekly drawdown cap (% of base capital).")
(defparameter *armada-rolling-pf-days*
  (armada-kill-env-int-or "SWIMMY_ARMADA_ROLLING_PF_DAYS" 30)
  "Lookback window for rolling PF kill gate.")
(defparameter *armada-rolling-pf-min*
  (armada-kill-env-float-or "SWIMMY_ARMADA_ROLLING_PF_MIN" 1.0)
  "Minimum rolling PF before kill switch trips.")
(defparameter *armada-trade-ratio-days*
  (armada-kill-env-int-or "SWIMMY_ARMADA_TRADE_RATIO_DAYS" 60)
  "Window size (days) for trade-activity decay check.")
(defparameter *armada-trade-ratio-min*
  (armada-kill-env-float-or "SWIMMY_ARMADA_TRADE_RATIO_MIN" 0.5)
  "Minimum ratio: trades(last N days) / trades(previous N days).")
(defparameter *armada-kill-check-interval-seconds*
  (armada-kill-env-int-or "SWIMMY_ARMADA_KILL_CHECK_INTERVAL_SECONDS" 300)
  "Interval for DB-backed kill checks.")
(defparameter *armada-kill-cooldown-seconds*
  (armada-kill-env-int-or "SWIMMY_ARMADA_KILL_COOLDOWN_SECONDS" 86400)
  "Cooldown duration after kill switch pause.")
(defvar *armada-kill-last-check-at* 0)
(defvar *armada-kill-last-notify-reason* nil)

(defun enable-armada-kill-switch-canary-mode ()
  "Enable Armada kill-switch canary preset."
  (setf *armada-kill-switch-enabled* t
        *armada-hard-dd-percent* 12.0
        *armada-weekly-dd-percent* 4.0
        *armada-rolling-pf-days* 30
        *armada-rolling-pf-min* 1.0
        *armada-trade-ratio-days* 60
        *armada-trade-ratio-min* 0.5
        *armada-kill-check-interval-seconds* 300
        *armada-kill-cooldown-seconds* 86400)
  t)

(defun enable-armada-canary-mode ()
  "Enable Armada canary preset for both rank gate and runtime kill switch."
  (when (fboundp 'enable-armada-core-canary-mode)
    (enable-armada-core-canary-mode))
  (enable-armada-kill-switch-canary-mode))

(when *armada-kill-canary-mode-enabled*
  (enable-armada-kill-switch-canary-mode))

(defun check-dynamic-circuit-breaker ()
  (let ((now (get-universal-time)))
    (setf *recent-losses* 
          (remove-if (lambda (ts) (> (- now ts) *max-loss-window-seconds*))
                     *recent-losses*))
    (when (>= (length *recent-losses*) *consecutive-loss-threshold*)
      (setf *circuit-breaker-active* t)
      (setf *breaker-cooldown-end* (+ now *breaker-cooldown-seconds*))
      (when (fboundp 'swimmy.core:notify-discord-alert)
        (when (and (boundp 'swimmy.globals:*system-state*) 
                   (or (eq swimmy.globals:*system-state* :trading) (eq swimmy.globals:*system-state* :warmup)))
          (swimmy.core:notify-discord-alert 
           (format nil "CIRCUIT BREAKER TRIPPED"))))
      (execute-tactical-retreat))))

(defun record-trade-result (result)
  (setf *last-trade-result* result)
  (if (eq result :loss)
      (progn
        (incf *consecutive-losses*)
        (setf *consecutive-wins* 0)
        (activate-danger-cooldown))
      (progn
        (incf *consecutive-wins*)
        (setf *consecutive-losses* 0)
        (when (> *cooldown-tier* 0)
          (setf *cooldown-tier* (max 0 (1- *cooldown-tier*)))))))

(defun get-current-price (symbol type)
  (declare (ignore type))
  (let ((candles (gethash symbol swimmy.globals:*candle-histories*)))
    (if (and candles (first candles))
        (swimmy.globals:candle-close (first candles))
        nil)))

(defun execute-tactical-retreat ()
  (let ((closed-count 0))
    (maphash 
     (lambda (key slot)
       (let* ((symbol (getf slot :symbol))
              (entry (getf slot :entry))
              (direction (getf slot :direction))
              (magic (getf slot :magic))
              (current-bid (get-current-price symbol :bid))
              (current-ask (get-current-price symbol :ask))
              (pnl 0))
         (cond
           ((and (eq direction :long) current-bid)
            (setf pnl (- current-bid entry)))
           ((and (eq direction :short) current-ask)
            (setf pnl (- entry current-ask))))
        (when (< pnl 0)
           (let ((msg (swimmy.core:encode-sexp `((type . "CLOSE")
                                                 (symbol . ,symbol)
                                                 (magic . ,magic)))))
             (pzmq:send swimmy.globals:*cmd-publisher* msg))
          (remhash key swimmy.globals:*slot-allocation*)
           (update-symbol-exposure symbol (or (getf slot :lot) 0.01) :close)
           (incf closed-count))))
     swimmy.globals:*slot-allocation*)
    (when (> closed-count 0)
      (swimmy.core:notify-discord-alert (format nil "TACTICAL RETREAT: Closed ~d losing positions." closed-count)))))

(defun format-duration-short (seconds)
  (cond
    ((< seconds 60) (format nil "~ds" seconds))
    ((< seconds 3600) (format nil "~dm" (floor seconds 60)))
    (t (format nil "~dh" (floor seconds 3600)))))

(defun activate-danger-cooldown ()
  (let* ((max-tier (1- (length swimmy.globals:*cooldown-durations*)))
         (new-tier (min max-tier (1+ swimmy.globals:*cooldown-tier*)))
         (duration-entry (nth new-tier swimmy.globals:*cooldown-durations*)))
    (setf swimmy.globals:*cooldown-tier* new-tier)
    (if (eq duration-entry :eod)
        (progn
          (setf swimmy.globals:*has-resigned-today* t)
          (setf swimmy.globals:*danger-cooldown-until* (+ (get-universal-time) 86400)))
        (setf swimmy.globals:*danger-cooldown-until* (+ (get-universal-time) duration-entry)))))

(defun danger-cooldown-active-p ()
  ;; Be robust: *danger-cooldown-until* can be NIL on fresh boot/hot-reload.
  (let ((until (if (numberp swimmy.globals:*danger-cooldown-until*)
                   swimmy.globals:*danger-cooldown-until*
                   0)))
    (> until (get-universal-time))))

(defun get-cooldown-remaining ()
  (let ((until (if (numberp swimmy.globals:*danger-cooldown-until*)
                   swimmy.globals:*danger-cooldown-until*
                   0)))
    (max 0 (- until (get-universal-time)))))

(defun reset-danger-state ()
  (setf *consecutive-losses* 0)
  (setf *consecutive-wins* 0)
  (setf *danger-level* 0)
  (setf swimmy.globals:*danger-cooldown-until* 0)
  (setf swimmy.globals:*has-resigned-today* nil))

(defun armada-weekly-drawdown-percent ()
  "Return weekly drawdown percent from tracked weekly PnL."
  (let* ((base (cond
                 ((and (boundp 'swimmy.globals:*current-equity*)
                       (numberp swimmy.globals:*current-equity*)
                       (> swimmy.globals:*current-equity* 0))
                  (float swimmy.globals:*current-equity* 1.0))
                 ((and (boundp 'swimmy.globals::*total-capital*)
                       (numberp swimmy.globals::*total-capital*)
                       (> swimmy.globals::*total-capital* 0))
                  (float swimmy.globals::*total-capital* 1.0))
                 (t 0.0)))
         (weekly-pnl (if (and (boundp 'swimmy.globals::*weekly-pnl*)
                              (numberp swimmy.globals::*weekly-pnl*))
                         (float swimmy.globals::*weekly-pnl* 1.0)
                         0.0)))
    (if (and (> base 0.0) (< weekly-pnl 0.0))
        (* 100.0 (/ (- weekly-pnl) base))
        0.0)))

(defun armada-rolling-profit-factor (&key (days *armada-rolling-pf-days*) (now (get-universal-time)))
  "Return rolling PF from trade_logs over DAYS window, or NIL if unavailable."
  (when (and (integerp days) (> days 0) (fboundp 'execute-to-list))
    (let* ((cutoff (- now (* days 86400)))
           (row (first (ignore-errors
	                         (execute-to-list
	                          "SELECT
	                               COALESCE(SUM(CASE WHEN pnl > 0 THEN pnl ELSE 0 END), 0) AS gross_win,
	                               COALESCE(SUM(CASE WHEN pnl < 0 THEN -pnl ELSE 0 END), 0) AS gross_loss
	                             FROM trade_logs
	                            WHERE timestamp >= ?
	                              AND UPPER(COALESCE(execution_mode, 'LIVE')) = 'LIVE'"
	                          cutoff)))))
      (when (and row (consp row))
        (destructuring-bind (gross-win gross-loss) row
          (cond
            ((and (numberp gross-loss) (> gross-loss 0.0) (numberp gross-win))
             (/ (float gross-win 1.0) (float gross-loss 1.0)))
            ((and (numberp gross-win) (> gross-win 0.0)) 99.0)
            (t nil)))))))

(defun armada-trade-count-in-window (from-ts to-ts)
  "Return trade_logs count in [FROM-TS, TO-TS)."
  (if (and (numberp from-ts) (numberp to-ts) (> to-ts from-ts) (fboundp 'execute-single))
	      (or (ignore-errors
	            (execute-single
	             "SELECT COUNT(*) FROM trade_logs
	               WHERE timestamp >= ? AND timestamp < ?
	                 AND UPPER(COALESCE(execution_mode, 'LIVE')) = 'LIVE'"
	             from-ts to-ts))
	          0)
      0))

(defun armada-trade-activity-ratio (&optional (now (get-universal-time)))
  "Return current/previous trade activity ratio over configured N-day windows."
  (let* ((days (max 1 *armada-trade-ratio-days*))
         (span (* days 86400))
         (curr-from (- now span))
         (prev-from (- curr-from span))
         (curr-count (armada-trade-count-in-window curr-from now))
         (prev-count (armada-trade-count-in-window prev-from curr-from)))
    (when (> prev-count 0)
      (/ (float curr-count 1.0) (float prev-count 1.0)))))

(defun armada-kill-switch-reason (&optional (now (get-universal-time)) &key (include-slow-checks t))
  "Return kill reason keyword or NIL when no Armada kill condition is met."
  (declare (ignore now))
  (when *armada-kill-switch-enabled*
    (cond
      ((or (>= (float (or swimmy.globals:*current-drawdown* 0.0) 1.0)
               (float *armada-hard-dd-percent* 1.0))
           (>= (float (or swimmy.globals:*monitoring-drawdown* 0.0) 1.0)
               (float *armada-hard-dd-percent* 1.0)))
       :hard-dd)
      ((>= (armada-weekly-drawdown-percent) (float *armada-weekly-dd-percent* 1.0))
       :weekly-dd)
      ((and include-slow-checks
            (let ((rolling-pf (armada-rolling-profit-factor :days *armada-rolling-pf-days*)))
              (and (numberp rolling-pf)
                   (< rolling-pf (float *armada-rolling-pf-min* 1.0)))))
       :rolling-pf)
      ((and include-slow-checks
            (let ((ratio (armada-trade-activity-ratio)))
              (and (numberp ratio)
                   (< ratio (float *armada-trade-ratio-min* 1.0)))))
       :trade-ratio)
      (t nil))))

(defun enforce-armada-kill-switch (&optional (now (get-universal-time)))
  "Apply Armada kill switch. Returns reason keyword when paused, else NIL."
  (when *armada-kill-switch-enabled*
    (let* ((run-slow-checks (>= (- now *armada-kill-last-check-at*)
                                *armada-kill-check-interval-seconds*))
           (reason (armada-kill-switch-reason now :include-slow-checks run-slow-checks)))
      (when run-slow-checks
        (setf *armada-kill-last-check-at* now))
      (if reason
          (progn
            ;; Keep school/global state in sync; execution guard reads school::*system-state*.
            (setf *system-state* :paused)
            (setf swimmy.globals:*system-state* :paused)
            (setf swimmy.globals:*danger-cooldown-until*
                  (max (if (numberp swimmy.globals:*danger-cooldown-until*)
                           swimmy.globals:*danger-cooldown-until*
                           0)
                       (+ now *armada-kill-cooldown-seconds*)))
            (when (and (fboundp 'swimmy.core:notify-discord-alert)
                       (not (eq reason *armada-kill-last-notify-reason*)))
              (swimmy.core:notify-discord-alert
               (format nil "🛑 Armada kill switch: ~a (system paused)" reason)
               :color 15158332))
            (setf *armada-kill-last-notify-reason* reason)
            reason)
          (progn
            (setf *armada-kill-last-notify-reason* nil)
            nil)))))

(format t "[L] school-danger.lisp loaded~%")
