(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :pzmq :silent t)
  (ql:quickload :jsown :silent t))

(defpackage :swimmy.server
  (:use :cl))

(in-package :swimmy.server)

;; --- Parameters (Fintokei) ---
(defparameter *initial-equity* 1000000)     ;; 初期証拠金
(defparameter *current-equity* 1000000)     ;; 現在の証拠金
(defparameter *daily-start-equity* 1000000) ;; 今日の開始時証拠金
(defparameter *max-daily-loss* 50000)       ;; 1日の最大損失額 (-50,000円)

;; --- Logic (Constitution) ---
(defun check-constitution (proposal)
  "提案されたトレードが憲法に違反していないか審査する"
  (let* ((pnl (jsown:val proposal "pnl"))          ;; 今回の損益
         (risk (jsown:val proposal "risk"))        ;; このトレードのリスク
         (new-equity (+ *current-equity* pnl))
         (daily-loss (- *daily-start-equity* new-equity)))
    
    ;; 状態更新 (シミュレーション反映)
    (setf *current-equity* new-equity)

    (cond
      ;; 第1条: 1日の損失限度額を超えてはならない
      ((> daily-loss *max-daily-loss*)
       (values "REJECTED" 
               (format nil "Daily Loss Limit Exceeded! Loss: ~A > Limit: ~A" 
                       daily-loss *max-daily-loss*)))
      
      ;; 第2条: 破産してはならない
      ((<= *current-equity* 0)
       (values "REJECTED" "Account Blown. Game Over."))
      
      ;; 合憲
      (t 
       (values "APPROVED" 
               (format nil "Equity: ~A (Daily PnL: ~A)" 
                       *current-equity* (- daily-loss)))))))

(defun start-brain ()
  (format t "~%🧠 Swimmy Brain (Fintokei Mode) is waking up...~%")
  (format t "   Daily Loss Limit: -~A JPY~%" *max-daily-loss*)
  (force-output)
  
  (let ((ctx (pzmq:ctx-new)))
    (unwind-protect
         (let ((receiver (pzmq:socket ctx :pull))
               (publisher (pzmq:socket ctx :pub)))
           (unwind-protect
                (progn
                  (pzmq:bind receiver "tcp://*:5555")
                  (pzmq:bind publisher "tcp://*:5556")
                  (format t "👂 Listening on tcp://*:5555~%")
                  
                  (loop
                    (format t "Waiting for market data...~%")
                    (force-output)
                    
                    (let ((raw-msg (pzmq:recv-string receiver)))
                      (handler-case
                          (let* ((data (jsown:parse raw-msg))
                                 (iter (jsown:val data "iteration")))
                            
                            (format t "📝 Reviewing Trade #~A... " iter)
                            
                            (multiple-value-bind (verdict reason)
                                (check-constitution data)
                              
                              (format t "[~A] ~A~%" verdict reason)
                              (force-output)
                              
                              (let ((response (jsown:to-json
                                               (jsown:new-js
                                                 ("type" "command")
                                                 ("verdict" verdict)
                                                 ("reason" reason)
                                                 ("reference_id" iter)))))
                                (pzmq:send publisher response))))
                        
                        (error (e)
                          (format t "!! FATAL ERROR: ~A~%" e)
                          (force-output))))))
             (pzmq:close receiver)
             (pzmq:close publisher)))
      (pzmq:ctx-destroy ctx))))
