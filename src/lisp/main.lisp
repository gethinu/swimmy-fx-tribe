;;; src/lisp/main.lisp
;;; =======================================================
;;; SYSTEM ENTRY POINT (Hickey's Decoupling)
;;; =======================================================

;; Package defined in packages.lisp
(in-package :swimmy.main)

;;; UTILS
(defun get-jst-str (&optional (ut (get-universal-time)))
  "Return current time as JST string"
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time ut -9)
    (format nil "~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D" year month day hour min sec)))

;;; EXTERNAL SERVICES
;;; EXTERNAL SERVICES
(defun call-gemini (prompt)
  "Legacy wrapper calling new Inference Worker"
  (swimmy.core:ask-ai prompt))

;;; SHUTDOWN LOGIC
(defun stop-brain ()
  "Graceful shutdown"
  (format t "~%[BRAIN] 🛑 Initiating graceful shutdown...~%")
  (when (fboundp 'cl-user::save-state)
    (funcall 'cl-user::save-state))
  (when (fboundp 'cl-user::save-genome)
    (funcall 'cl-user::save-genome))
    
  (handler-case
      (progn
        (when (and (boundp 'cl-user::*publisher*) (symbol-value 'cl-user::*publisher*))
          (pzmq:close (symbol-value 'cl-user::*publisher*)))
        (when (and (boundp 'cl-user::*cmd-publisher*) (symbol-value 'cl-user::*cmd-publisher*))
          (pzmq:close (symbol-value 'cl-user::*cmd-publisher*)))
        (when (and (boundp 'cl-user::*subscriber*) (symbol-value 'cl-user::*subscriber*))
          (pzmq:close (symbol-value 'cl-user::*subscriber*))))
    (error (e) (format t "[BRAIN] Socket close error: ~a~%" e)))
    
  (format t "[BRAIN] ✅ Shutdown complete. Goodbye.~%")
  (sb-ext:exit :code 0))

;;; INITIALIZATION LOGIC
(defun initialize-system ()
  (format t "[SYSTEM] Initializing subsystems...~%")
  (initialize-tribal-dialect)
  
  (when (fboundp 'load-genome)
    (funcall 'load-genome))
  
  (when (fboundp 'load-state)
    (funcall 'load-state))
  
  (when (fboundp 'load-treasury)
    (funcall 'load-treasury))
    
  (load-hall-of-fame)
  (load-hall-of-fame)
  (setup-symbol-webhooks)
  
  ;; P1: Data Keeper Integration
  (init-data-keeper-client)
  (format t "[SYSTEM] Loading historical data from Data Keeper...~%")
  (dolist (sym *supported-symbols*)
    ;; Load M1 Base (Legacy key: symbol -> list)
    ;; Load M1 Base (Legacy key: symbol -> list)
    ;; V11.0: Reduced to 10,000 for RAM safety. Deep backtests use Direct CSV.
    (let ((history (get-history-from-keeper sym 10000 "M1")))
      (if history
          (progn
            (setf (gethash sym *candle-histories*) history)
            (format t "[SYSTEM] Loaded ~d candles for ~a (M1)~%" (length history) sym)
            ;; Legacy compat for main symbol
            (when (string= sym "USDJPY")
              (setf *candle-history* history)))
          (format t "[SYSTEM] ⚠️ No M1 history available for ~a~%" sym)))
    
    ;; Load Other Timeframes (Nested key: symbol -> tf -> list)
    (let ((timeframes '("M5" "M15" "M30" "H1" "H4" "D1" "W1" "MN")))
    	  (dolist (tf timeframes)
	    ;; V11.0: Reduced to 5,000 for RAM safety
	    (let ((tf-hist (get-history-from-keeper sym 5000 tf)))
          (when (and tf-hist (> (length tf-hist) 0))
             ;; Ensure nested hash exists
             (unless (gethash sym *candle-histories-tf*)
                (setf (gethash sym *candle-histories-tf*) (make-hash-table :test 'equal)))
             (setf (gethash tf (gethash sym *candle-histories-tf*)) tf-hist)
             (format t "[SYSTEM] Loaded ~d candles for ~a (~a)~%" (length tf-hist) sym tf))))))
             
  ;; V9.2: Init School Strategy System (AFTER History Load for Safety Gate)
  (swimmy.school::init-school)
  
  (format t "[SYSTEM] Initialization complete.~%"))

;;; MAIN ENTRY POINT
(defun start-system ()
  (format t "~%")
  (format t "╔════════════════════════════════════════════════════════════╗~%")
  (format t "║  🐟 SWIMMY Ver 41.5 - The Refactored Gardener              ║~%")
  (format t "║     Powering Up via ASDF System Definition                 ║~%")
  (format t "╚════════════════════════════════════════════════════════════╝~%")
  
  (initialize-system)
  
  ;; V7.2: Notify JSON log of startup for real-time sync verification
  (log-info "SYSTEM_STARTUP: Swimmy is powering up (Attack Mode Active)")
  
  ;; V41.6: Notify Apex webhook of system online status
  (swimmy.core:notify-apex "✅ Swimmy System Online & Connected (Recovery Complete)" :color 3066993)
  
  ;; V8.9: Signal Handling for Graceful Shutdown (Save State on SIGTERM)
  (sb-sys:enable-interrupt sb-unix:sigterm 
                           (lambda (signal code scp)
                             (declare (ignore signal code scp))
                             (format t "~%[SYSTEM] 🛑 SIGTERM received. Saving state before exit...~%")
                             (stop-brain)))

  ;; Start Runner (Infinite Loop)
  (start-brain))
