
(in-package :cl-user)
(require :asdf)
(asdf:load-system :swimmy :force t) ;; Force reload to ensure fixes

(format t "Diagnostic Backtest Starting...~%")

;; Setup ZMQ
(defparameter *ctx* (pzmq:ctx-new))
(defparameter *cmd* (pzmq:socket *ctx* :pub)) ;; Publish to Guardian SUB (5556? Check brain.lisp)
(defparameter *sub* (pzmq:socket *ctx* :sub)) ;; Subscribe to Guardian PUB (5557)

;; Check ports from config (hardcoded for diagnostic based on makefile/brain.lisp)
;; Brain publishes to 5556. Guardian subscribes 5556.
;; Guardian publishes to 5557. Brain subscribes 5557.

;; Wait, brain.lisp:
;; *cmd-publisher* :pub bound to 5556? No, brain binds pub 5556. Guardian connects sub 5556.
;; OR Brain connects push?
;; Let's assume standard architecture:
;; Brain binds PUB 5556.
;; Guardian connects SUB 5556.
;; Guardian binds PUB 5557.
;; Brain connects SUB 5557.

;; So my script must behave like Brain:
;; Bind PUB 5556? No, 5556 is already bound by the running Brain process!
;; I cannot bind same port.
;; I must use a different way or kill the brain?
;; Or I can use the existing brain to trigger it?

;; Option B: Use existing brain.
;; I can't inject command easily unless I have a command socket.
;; Guardian might have a PULL socket?
;; Guardian src/main.rs:
;; let sub_socket = ctx.socket(zmq::SUB)? ... connect("tcp://localhost:5556")
;; So Guardian connects to Brain.

;; If I want to send command to Guardian, I must BE the Brain (bind 5556).
;; But Brain is running.
;; I must STOP Brain first to run this diagnostic?
;; OR I can rely on `verify_resample_fix.lisp` which showed logic is correct.
;; But User says "numbers are wrong".

;; Let's use `make run` logs inspection instead, it's safer than conflict.
;; BUT I want to see a specific strategy NOW.

;; Alternative:
;; Create a script that uses the Lisp logic to Generate the JSON, print it, but NOT send it.
;; Then I can manually inspect if "Open" and "Close" in JSON are correct.
;; If JSON is correct -> Guardian is broken.
;; If JSON is wrong -> Lisp fix is broken.

(format t "Checking Resample Logic for T-Nakane-Gotobi...~%")

(let* ((strat (find "T-Nakane-Gotobi" swimmy.globals:*strategy-knowledge-base* :key #'swimmy.school:strategy-name :test #'string=))
       (history (subseq swimmy.globals:*candle-history* 0 (min 500 (length swimmy.globals:*candle-history*)))))
  
  (unless strat
    (format t "Strategy not found! Loading default knowledge...~%")
    ;; Force load strategies if empty (script environment)
    ;; (load "src/lisp/strategies.lisp") ... handled by ASD
    )

  ;; Mock History if nil
  (unless history
    (format t "No history found. Generating Mock History (Newest First)...~%")
    (setf history (loop for i from 0 below 10 collect 
                        (swimmy.globals:make-candle :timestamp (- 1000 (* i 60)) :open 100.0 :close 101.0 :high 102.0 :low 99.0 :volume 10))))

  (format t "Original Candle 0 (Newest): O=~a C=~a~%" 
          (swimmy.globals:candle-open (first history))
          (swimmy.globals:candle-close (first history)))
  (format t "Original Candle 1: O=~a C=~a~%" 
          (swimmy.globals:candle-open (second history))
          (swimmy.globals:candle-close (second history)))

  ;; Resample Factor 5
  (let ((resampled (swimmy.school::resample-candles history 5)))
    (format t "Resampled (Factor 5) Count: ~d~%" (length resampled))
    (let ((r (first resampled)))
        (format t "Resampled Candle 0: O=~a C=~a~%" 
            (swimmy.globals:candle-open r)
            (swimmy.globals:candle-close r))
        
        ;; Logic Check
        ;; Input: C0, C1, C2, C3, C4 (Newest..Oldest in chunk)
        ;; Resample Logic: Open should be C4 Open. Close should be C0 Close.
        ;; My Mock: All 100/101.
        ;; Let's make distinct mock.
    )))

(format t "Generating Distinct Mock...~%")
(let* ((c0 (swimmy.globals:make-candle :timestamp 500 :open 10 :close 11 :high 12 :low 9 :volume 1)) ;; Newest
       (c1 (swimmy.globals:make-candle :timestamp 400 :open 11 :close 12 :high 13 :low 10 :volume 1))
       (c2 (swimmy.globals:make-candle :timestamp 300 :open 12 :close 13 :high 14 :low 11 :volume 1))
       (c3 (swimmy.globals:make-candle :timestamp 200 :open 13 :close 14 :high 15 :low 12 :volume 1))
       (c4 (swimmy.globals:make-candle :timestamp 100 :open 14 :close 15 :high 16 :low 13 :volume 1)) ;; Oldest
       (mock-hist (list c0 c1 c2 c3 c4)))
       
   (format t "Input (Newest First):~%")
   (format t "  C0 (Newest): Open=10 Close=11 Time=500~%")
   (format t "  ...~%")
   (format t "  C4 (Oldest): Open=14 Close=15 Time=100~%")
   
   (let ((res (first (swimmy.school::resample-candles mock-hist 5))))
      (format t "Result:~%")
      (format t "  Open: ~a (Target: 14 - from Oldest)~%" (swimmy.globals:candle-open res))
      (format t "  Close: ~a (Target: 11 - from Newest)~%" (swimmy.globals:candle-close res))
      
      (if (and (= (swimmy.globals:candle-open res) 14)
               (= (swimmy.globals:candle-close res) 11))
          (format t "✅ LOGIC VERIFIED: Correct Newest-First Handling~%")
          (format t "❌ SCRIPT FAILED: Logic is INVERTED! Open=~a Close=~a~%" 
                  (swimmy.globals:candle-open res) 
                  (swimmy.globals:candle-close res)))))
