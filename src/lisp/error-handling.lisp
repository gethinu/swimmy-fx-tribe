;;; error-handling.lisp - Comprehensive Error Handling for Swimmy
;;; ═══════════════════════════════════════════════════════════════════
;;; "The only truly secure system is one that is powered off."
;;;  - Gene Spafford
;;;
;;; This module provides robust error handling to ensure Swimmy never
;;; crashes unexpectedly and always fails gracefully.

(defpackage :swimmy-errors
  (:use :cl)
  (:export :with-safe-execution
           :with-trade-safety
           :with-network-retry
           :log-error
           :*error-log*))

(in-package :swimmy-errors)

;;; ─────────────────────────────────────────
;;; ERROR LOG
;;; ─────────────────────────────────────────

(defvar *error-log* nil "Log of all errors encountered")
(defvar *max-error-log* 100 "Maximum errors to keep in log")

(defstruct error-entry
  timestamp
  error-type
  message
  context
  stack-trace)

(defun log-error (error-type message &optional context)
  "Log an error with timestamp and context"
  (let ((entry (make-error-entry
                :timestamp (get-universal-time)
                :error-type error-type
                :message message
                :context context
                :stack-trace nil)))
    (push entry *error-log*)
    ;; Trim if too long
    (when (> (length *error-log*) *max-error-log*)
      (setf *error-log* (subseq *error-log* 0 *max-error-log*)))
    
    ;; Log to console
    (format t "[ERROR] ~a: ~a~%" error-type message)
    entry))

;;; ─────────────────────────────────────────
;;; MACRO: with-safe-execution
;;; ─────────────────────────────────────────
;;; Execute code with comprehensive error handling

(defmacro with-safe-execution (context &body body)
  "Execute body with full error handling. Returns nil on error."
  `(handler-case
       (progn ,@body)
     (division-by-zero (e)
       (log-error :division-by-zero (format nil "~a" e) ,context)
       nil)
     (type-error (e)
       (log-error :type-error (format nil "~a" e) ,context)
       nil)
     (undefined-function (e)
       (log-error :undefined-function (format nil "~a" e) ,context)
       nil)
     (error (e)
       (log-error :general-error (format nil "~a" e) ,context)
       nil)))

;;; ─────────────────────────────────────────
;;; MACRO: with-trade-safety
;;; ─────────────────────────────────────────
;;; Extra safety for trade execution

(defmacro with-trade-safety ((symbol direction lot) &body body)
  "Execute trade with safety checks. Returns nil if any check fails."
  `(progn
     ;; Pre-trade validation
     (unless (stringp ,symbol)
       (log-error :trade-validation "Symbol must be string" ,symbol)
       (return-from with-trade-safety nil))
     
     (unless (member ,direction '(:buy :sell))
       (log-error :trade-validation "Invalid direction" ,direction)
       (return-from with-trade-safety nil))
     
     (unless (and (numberp ,lot) (> ,lot 0) (< ,lot 10))
       (log-error :trade-validation "Invalid lot size" ,lot)
       (return-from with-trade-safety nil))
     
     ;; Execute with error handling
     (with-safe-execution (list :trade ,symbol ,direction ,lot)
       ,@body)))

;;; ─────────────────────────────────────────
;;; MACRO: with-network-retry
;;; ─────────────────────────────────────────
;;; Retry network operations

(defmacro with-network-retry ((max-retries delay-seconds) &body body)
  "Retry network operation up to max-retries times with delay between attempts"
  `(loop for attempt from 1 to ,max-retries
         do (handler-case
                (return (progn ,@body))
              (error (e)
                (format t "[NETWORK] Attempt ~d/~d failed: ~a~%" 
                        attempt ,max-retries e)
                (when (< attempt ,max-retries)
                  (sleep ,delay-seconds))))
         finally (log-error :network-failure 
                           (format nil "All ~d attempts failed" ,max-retries))))

;;; ─────────────────────────────────────────
;;; VALIDATION FUNCTIONS
;;; ─────────────────────────────────────────

(defun validate-price (price)
  "Validate that price is a reasonable number"
  (and (numberp price) 
       (> price 0) 
       (< price 1000000)))

(defun validate-lot (lot)
  "Validate lot size"
  (and (numberp lot)
       (>= lot 0.01)
       (<= lot 10.0)))

(defun validate-symbol (symbol)
  "Validate trading symbol"
  (and (stringp symbol)
       (member symbol '("USDJPY" "EURUSD" "GBPUSD" "BTCUSD" "ETHUSD") 
               :test #'string=)))

;;; ─────────────────────────────────────────
;;; ERROR RECOVERY
;;; ─────────────────────────────────────────

(defun recover-from-error (error-type)
  "Attempt to recover from common errors"
  (case error-type
    (:network-failure
     (format t "[RECOVERY] Attempting to reconnect...~%")
     ;; Would implement reconnection logic
     t)
    (:trade-failure
     (format t "[RECOVERY] Checking position status...~%")
     ;; Would implement position check
     t)
    (otherwise
     (format t "[RECOVERY] No auto-recovery available for ~a~%" error-type)
     nil)))

;;; ─────────────────────────────────────────
;;; ERROR REPORTING
;;; ─────────────────────────────────────────

(defun get-error-summary ()
  "Get summary of recent errors"
  (let ((counts (make-hash-table)))
    (dolist (entry *error-log*)
      (incf (gethash (error-entry-error-type entry) counts 0)))
    (format t "~%═══════════════════════════════════════~%")
    (format t "📊 ERROR SUMMARY~%")
    (format t "═══════════════════════════════════════~%")
    (format t "Total errors: ~d~%" (length *error-log*))
    (maphash (lambda (type count)
               (format t "  ~a: ~d~%" type count))
             counts)
    (format t "═══════════════════════════════════════~%~%")))

(format t "[ERRORS] Comprehensive error handling loaded~%")
