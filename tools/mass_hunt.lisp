(in-package :cl-user)

(require 'asdf)
(push (uiop:getcwd) asdf:*central-registry*)

(handler-case
    (progn
      (format t "[HUNT] 🚀 Loading Swimmy System...~%")
      (ql:quickload :swimmy :silent t)
      
      (in-package :swimmy.school)

      (defun force-generate-scout (attempt target-tf)
        "Generate random parameters with FORCED timeframe."
        ;; Phase 8.5: Mass Hunt Optimization
        ;; 1. Lower standards to capture EVERYTHING (-10.0 Sharpe)
        ;; 2. Exclude GBPUSD (Data Issue causing Guardian Crash)
        (setf swimmy.school::*min-sharpe* -10.0)
        (setf swimmy.school::*combine-currencies* '("USDJPY" "EURUSD"))

        (let ((short (+ 5 (random 45)))
              (long-p (+ 50 (random 150))))
          (swimmy.school::make-scout-struct
           :name (format nil "HUNT-M~d-~d-~d" target-tf (get-universal-time) attempt)
           :timeframe target-tf
           :sma-short short
           :sma-long long-p
           :sl (+ 0.01 (random 0.09))
           :tp (+ 0.01 (random 0.14)))))

      (defun mass-hunt (target-tf target-count)
        (format t "~%[HUNT] 🏹 Targeting ~d strategies for M~d...~%" target-count target-tf)
        (let ((recruited 0)
              (attempts 0))
          
          (loop while (< recruited target-count) do
               (incf attempts)
               (when (> attempts 10000)
                 (format t "[HUNT] ⚠️ Max attempts (10000) reached. Aborting tier.~%")
                 (return))
               (let ((candidate (force-generate-scout attempts target-tf)))
                 ;; Skip graveyard check for speed/brute force? No, keep it.
                 (unless (swimmy.school::is-toxic-params candidate)
                   (format t "[DEBUG] Testing candidate ~a~%" (swimmy.school::scout-struct-name candidate))
                   (let ((result (swimmy.school::run-combine-test candidate)))
                     (when result
                       (format t "[DEBUG] run-combine-test returned: ~a (Type: ~a)~%" result (type-of result))
                       (destructuring-bind (sym sharpe trades) result
                         (format t "[HUNT] 🎯 BAGGED: ~a (Sharpe: ~,2f)~%" sym sharpe)
                         (setf (swimmy.school::scout-struct-symbol candidate) sym)
                         (format t "[DEBUG] Registering recruit...~%")
                         (swimmy.school::register-recruit candidate sharpe trades)
                         (format t "[DEBUG] Registration complete.~%")
                         (incf recruited)
                         ;; Print progress every 10
                         (when (= (mod recruited 10) 0)
                           (format t "[HUNT] 📦 Progress: ~d/~d (Attempts: ~d)~%" recruited target-count attempts))))))))
          
          (format t "[HUNT] ✅ Mission Complete. Recruited ~d strategies in ~d attempts.~%" recruited attempts)
          recruited))

      ;; EXECUTION
      ;; -------------------------------------------
      (let ((m5-count (mass-hunt 5 100))
            (m15-count (mass-hunt 15 100)))
        
        (format t "~%[HUNT] 📝 FINAL REPORT~%")
        (format t "-----------------------------------~%")
        (format t "M5 Strategies: ~d~%" m5-count)
        (format t "M15 Strategies: ~d~%" m15-count)
        
        ;; VERIFICATION (Physical File Check)
        (format t "~%[HUNT] 🕵️ Verifying Disk Persistence...~%")
        (force-output)
        
        (let ((files (directory "data/library/INCUBATOR/HUNT-*.lisp")))
           (format t "[HUNT] 💾 Found ~d 'HUNT' files in INCUBATOR.~%" (length files))
           (if (>= (length files) 200)
               (format t "[HUNT] ✅ VERIFICATION PASSED.~%")
               (format t "[HUNT] ⚠️ CHECK REQUIRED: Expected 200+, found ~d.~%" (length files))))
      ))
      
  (error (e)
    (format t "[HUNT] ❌ CRITICAL ERROR: ~a~%" e)
    (uiop:quit 1)))

(uiop:quit 0)
