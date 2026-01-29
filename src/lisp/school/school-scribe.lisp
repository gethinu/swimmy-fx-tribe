;;; school-scribe.lisp - Asynchronous I/O Isolation Service
;;; Phase 25: Isolation (Vogels)
;;;
;;; "Everything fails, all the time." - Werner Vogels
;;; "Latency is the mind-killer." - Elon Musk
;;;
;;; This module completely decouples the Main Trading Loop from Disk I/O.
;;; All logs, DB inserts, and file writes are pushed to a ring buffer.
;;; A dedicated "Scribe" thread drains the buffer and handles the slow I/O.

(defpackage :swimmy.school.scribe
  (:use :cl :bordeaux-threads)
  (:export #:start-scribe
           #:stop-scribe
           #:scribe-record
           #:scribe-status))

(in-package :swimmy.school.scribe)

;;; ============================================================================
;;; CONFIGURATION
;;; ============================================================================

(defparameter *scribe-queue* nil)     ; The FIFO Queue (Thread-Safe)
(defparameter *scribe-thread* nil)    ; The Worker Thread
(defparameter *scribe-running* nil)   ; Control Flag
(defparameter *scribe-lock* (make-lock "ScribeLock"))
(defparameter *scribe-cond* (make-condition-variable :name "ScribeCond"))

(defparameter *max-queue-size* 10000) ; Drop logs if backlog > 10000 (Safety)

;;; ============================================================================
;;; THE SCRIBE
;;; ============================================================================

(defun start-scribe ()
  "Start the Scribe background thread."
  (with-lock-held (*scribe-lock*)
    (when *scribe-running*
      (format t "[SCRIBE] ⚠️ Already running.~%")
      (return-from start-scribe))
    
    ;; (setf *scribe-queue* (make-instance 'cl-speedy-queue:speedy-queue :size *max-queue-size*))
    ;; Fallback if speedy-queue not avail? We'll use a simple list + lock for now to enable portability.
    ;; Actually, let's use a standard list with a lock for robustness over speed for now.
    ;; Optimization: Use a proper ring buffer later.
    (setf *scribe-queue* nil) 
    
    (setf *scribe-running* t)
    (setf *scribe-thread* 
          (make-thread #'scribe-worker-loop :name "ScribeWorker"))
    
    (format t "[SCRIBE] 📜 Service Started. Isolation Active.~%")))

(defun stop-scribe ()
  "Signal Scribe to stop (after draining queue)."
  (with-lock-held (*scribe-lock*)
    (setf *scribe-running* nil)
    (condition-notify *scribe-cond*))
  (when *scribe-thread*
    (join-thread *scribe-thread*)
    (setf *scribe-thread* nil))
  (format t "[SCRIBE] 🛑 Service Stopped.~%"))

(defun scribe-worker-loop ()
  "The dedicated I/O loop."
  (format t "[SCRIBE-THREAD] 🧵 Worker Online via ~a~%" (current-thread))
  (loop while (or *scribe-running* *scribe-queue*) do
    (let ((task nil))
      (with-lock-held (*scribe-lock*)
        (loop while (and *scribe-running* (null *scribe-queue*))
              do (condition-wait *scribe-cond* *scribe-lock*))
        
        ;; Pop task
        (when *scribe-queue*
          (setf task (pop *scribe-queue*))))
      
      ;; Process Task outside lock (Blocking I/O happens here!)
      (when task
        (handler-case
            (process-scribe-task task)
          (error (e)
            (format *error-output* "[SCRIBE-ERR] 💥 ~a~%" e)))))))

(defun process-scribe-task (task)
  "Dispatch I/O task.
   Task structure: (:APPENDER payload...)"
  (let ((type (car task))
        (payload (cdr task)))
    (case type
      (:FILE-LOG
       ;; (:FILE-LOG path string)
       (let ((path (first payload))
             (msg (second payload)))
         (with-open-file (s path :direction :output :if-exists :append :if-does-not-exist :create)
           (write-string msg s))))
      
      (:SQL-EXEC
       ;; (:SQL-EXEC query args)
       ;; We need to access the DB connection safely. 
       ;; Ideally, Scribe should own a dedicated DB connection.
       ;; For now, we assume sqlite is thread-safe enough or we use a separate conn.
       ;; (apply #'sqlite:execute-non-query *db-conn* (first payload) (rest payload))
       ;; Mock for now until school-db.lisp integration
       )
      
      (:RECORD-SWAPS
       ;; (:RECORD-SWAPS symbol swap-long swap-short spread)
       ;; Use symbol lookup to avoid circular dependency
       (let ((func (find-symbol "RECORD-SWAP-DATA" "SWIMMY.SCHOOL")))
         (when (and func (fboundp func))
           (apply func payload))))

      (t (format t "[SCRIBE] Unknown Task: ~a~%" type)))))

;;; ============================================================================
;;; PUBLIC API (NON-BLOCKING)
;;; ============================================================================

(defun scribe-record (type &rest args)
  "Push an I/O task to the Scribe queue. Returns IMMEDIATELY."
  (with-lock-held (*scribe-lock*)
    ;; Safety: Drop if queue too full (Head-of-line blocking protection)
    (if (< (length *scribe-queue*) *max-queue-size*)
        (progn
          (setf *scribe-queue* (nconc *scribe-queue* (list (cons type args))))
          (condition-notify *scribe-cond*))
        (format *error-output* "[SCRIBE] ⚠️ Queue Full! Dropping log.~%"))))

(defun scribe-status ()
  (with-lock-held (*scribe-lock*)
    (format nil "Running: ~a | Queue: ~d" *scribe-running* (length *scribe-queue*))))
