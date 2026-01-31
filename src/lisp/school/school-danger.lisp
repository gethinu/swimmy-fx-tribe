;; school-danger.lisp - Danger Avoidance System
(in-package :swimmy.school)

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
     (lambda (key warrior)
       (let* ((symbol (getf warrior :symbol))
              (entry (getf warrior :entry))
              (direction (getf warrior :direction))
              (magic (getf warrior :magic))
              (current-bid (get-current-price symbol :bid))
              (current-ask (get-current-price symbol :ask))
              (pnl 0))
         (cond
           ((and (eq direction :long) current-bid)
            (setf pnl (- current-bid entry)))
           ((and (eq direction :short) current-ask)
            (setf pnl (- entry current-ask))))
         (when (< pnl 0)
           (pzmq:send swimmy.globals:*cmd-publisher* (jsown:to-json (jsown:new-js ("action" "CLOSE") ("symbol" symbol) ("magic" magic))))
           (remhash key swimmy.globals:*warrior-allocation*)
           (update-symbol-exposure symbol (or (getf warrior :lot) 0.01) :close)
           (incf closed-count))))
     swimmy.globals:*warrior-allocation*)
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
  (> swimmy.globals:*danger-cooldown-until* (get-universal-time)))

(defun get-cooldown-remaining ()
  (max 0 (- swimmy.globals:*danger-cooldown-until* (get-universal-time))))

(defun reset-danger-state ()
  (setf *consecutive-losses* 0)
  (setf *consecutive-wins* 0)
  (setf *danger-level* 0)
  (setf swimmy.globals:*danger-cooldown-until* 0)
  (setf swimmy.globals:*has-resigned-today* nil))

(format t "[L] school-danger.lisp loaded~%")
