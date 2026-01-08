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
    (let ((history (get-history-from-keeper sym 5000)))
      (if history
          (progn
            (setf (gethash sym *candle-histories*) history)
            (format t "[SYSTEM] Loaded ~d candles for ~a~%" (length history) sym)
            ;; Legacy compat for main symbol
            (when (string= sym "USDJPY")
              (setf *candle-history* history)))
          (format t "[SYSTEM] ⚠️ No history available for ~a~%" sym)))))

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
  (notify-apex "✅ Swimmy System Online & Connected (Recovery Complete)" :color 3066993)
  
  ;; Start Runner (Infinite Loop)
  (start-brain))
