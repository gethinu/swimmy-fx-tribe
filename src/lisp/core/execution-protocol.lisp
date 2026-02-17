;;; execution-protocol.lisp - Strict Execution Protocol V2
;;; Phase 7 Implementation (memo3.txt Section 7)
;;;
;;; Responsibility: Define the schema for messages sent to the Execution Agent (MQL5/EA)

(in-package :swimmy.core)

(defparameter +protocol-version+ "2.0")
(defparameter *uuid-entropy-path* "/dev/urandom")

;; Message Types (Strict Dispatch)
(defparameter +MSG-HEARTBEAT+ "HEARTBEAT")
(defparameter +MSG-ORDER-OPEN+ "ORDER_OPEN")
(defparameter +MSG-ORDER-FILL+ "ORDER_FILL")
(defparameter +MSG-ORDER-ACK+ "ORDER_ACK")  ;; Incoming
(defparameter +MSG-ORDER-REJECT+ "ORDER_REJECT") ;; Incoming
(defparameter +MSG-TRADE-CLOSED+ "TRADE_CLOSED") ;; Incoming
(defparameter +MSG-ACCOUNT-INFO+ "ACCOUNT_INFO") ;; Incoming
(defparameter +MSG-HISTORY+ "HISTORY") ;; Incoming
(defparameter +MSG-TICK+ "TICK") ;; Incoming
(defparameter +MSG-SWAP-DATA+ "SWAP_DATA") ;; Incoming (Phase 28: Data Lake)

(defun generate-uuid ()
  "Generate a pseudo-UUID v4 string."
  (labels ((read-entropy-bytes ()
             (handler-case
                 (when *uuid-entropy-path*
                   (with-open-file (s *uuid-entropy-path*
                                      :direction :input
                                      :element-type '(unsigned-byte 8))
                     (let ((bytes (make-array 16 :element-type '(unsigned-byte 8))))
                       (when (= (read-sequence bytes s) 16)
                         bytes))))
               (error () nil)))
           (bytes->uuid (bytes)
             ;; Set version (4) and variant (10) bits.
             (setf (aref bytes 6) (logior #x40 (logand (aref bytes 6) #x0f)))
             (setf (aref bytes 8) (logior #x80 (logand (aref bytes 8) #x3f)))
             (format nil "~(~2,'0x~2,'0x~2,'0x~2,'0x-~2,'0x~2,'0x-~2,'0x~2,'0x-~2,'0x~2,'0x-~2,'0x~2,'0x~2,'0x~2,'0x~2,'0x~2,'0x~)"
                     (aref bytes 0) (aref bytes 1) (aref bytes 2) (aref bytes 3)
                     (aref bytes 4) (aref bytes 5)
                     (aref bytes 6) (aref bytes 7)
                     (aref bytes 8) (aref bytes 9)
                     (aref bytes 10) (aref bytes 11) (aref bytes 12)
                     (aref bytes 13) (aref bytes 14) (aref bytes 15))))
    (let ((bytes (read-entropy-bytes)))
      (if bytes
          (bytes->uuid bytes)
          ;; Mix time-based entropy to avoid collisions across deterministic RNG seeds.
          (let* ((now (get-universal-time))
                 (ticks (get-internal-real-time))
                 (r1 (logxor (random 4294967296) (ldb (byte 32 0) now)))
                 (r2 (logxor (random 65536) (ldb (byte 16 0) ticks)))
                 (r3 (logxor (random 4096) (ldb (byte 12 16) ticks)))
                 (r4 (logxor (random #x4000) (ldb (byte 14 0) now)))
                 (r5 (logxor (random 281474976710656) (ldb (byte 48 0) ticks))))
            (format nil "~(~x-~x-4~3,'0x-~x-~x~)"
                    r1 r2 r3 (logior #x8000 r4) r5))))))

(defun sexp->string (form &key (package :swimmy.core))
  "Render S-expression FORM as a single-line string."
  (with-standard-io-syntax
    (let ((*print-case* :downcase)
          (*print-pretty* nil)
          (*print-right-margin* most-positive-fixnum)
          (*print-escape* t)
          (*print-readably* nil)
          (*package* (find-package package)))
      (format nil "~s" form))))

(defun normalize-sexp-key (key)
  (cond
    ((symbolp key) key)
    ((stringp key) (intern (string-upcase key) :swimmy.core))
    (t key)))

(defun sexp-alist-get (alist key)
  (let ((key-str (string-upcase (if (symbolp key) (symbol-name key) key))))
    (cdr (find-if (lambda (pair)
                    (and (consp pair)
                         (symbolp (car pair))
                         (string-equal (symbol-name (car pair)) key-str)))
                  alist))))

(defun encode-sexp (alist)
  "Serialize an alist into an S-expression string with stable formatting."
  (let ((*print-case* :downcase)
        (*print-pretty* nil)
        (*print-right-margin* most-positive-fixnum)
        (*print-escape* t)
        (*package* (find-package :swimmy.core)))
    (format nil "~s"
            (mapcar (lambda (pair)
                      (if (consp pair)
                          (cons (normalize-sexp-key (car pair)) (cdr pair))
                          pair))
                    alist))))

(defun make-protocol-message (type payload)
  "Construct a standard protocol message wrapper."
  (append `((type . ,type)
            (id . ,(generate-uuid))
            (timestamp . ,(local-time:format-timestring nil (local-time:now)
                                                       :format '(:year "-" (:month 2) "-" (:day 2) "T" (:hour 2) ":" (:min 2) ":" (:sec 2) "Z")))
            (version . ,+protocol-version+))
          (mapcar (lambda (pair)
                    (cons (normalize-sexp-key (car pair)) (cdr pair)))
                  payload)))

(defun make-protocol-sexp (type payload-alist)
  "Construct a standard protocol alist for S-expression transport."
  (let ((base `((type . ,type)
                (id . ,(generate-uuid))
                (timestamp . ,(local-time:format-timestring nil (local-time:now)
                                                           :format '(:year "-" (:month 2) "-" (:day 2) "T" (:hour 2) ":" (:min 2) ":" (:sec 2) "Z")))
                (version . ,+protocol-version+))))
    (append base payload-alist)))

(defun split-order-comment-context (comment-text)
  "Split COMMENT-TEXT into (strategy timeframe) by first | separator."
  (let ((sep (and (stringp comment-text) (position #\| comment-text))))
    (when sep
      (values (subseq comment-text 0 sep)
              (subseq comment-text (1+ sep))))))

(defun invalid-order-comment-reason (comment)
  "Return keyword reason when COMMENT is invalid execution context.
Valid format is \"strategy|timeframe\" with non-empty, non-NIL tokens."
  (let* ((raw (if comment
                  (string-trim '(#\Space #\Tab #\Newline #\Return) (format nil "~a" comment))
                  ""))
         (up (string-upcase raw)))
    (cond
      ((string= raw "") :missing-comment)
      ((not (position #\| raw)) :invalid-format)
      ((or (string= up "NIL") (search "NIL|" up)) :missing-strategy)
      (t
       (multiple-value-bind (strategy timeframe) (split-order-comment-context raw)
         (let ((s (string-trim '(#\Space #\Tab #\Newline #\Return) (or strategy "")))
               (tf (string-trim '(#\Space #\Tab #\Newline #\Return) (or timeframe ""))))
           (cond
             ((or (string= s "") (string= (string-upcase s) "NIL")) :missing-strategy)
             ((or (string= tf "") (string= (string-upcase tf) "NIL")) :missing-timeframe)
             (t nil))))))))

(defun make-order-message (strategy-id symbol side lot price sl tp &key (magic 0) (comment ""))
  "Construct an ORDER_OPEN message (Protocol V2).
   
   Args:
     strategy-id: String identifier of the strategy (e.g. 'Trend-Follow-v1')
     symbol: Instrument symbol (e.g. 'USDJPY')
     side: :BUY or :SELL (or string)
     lot: Volume
     price: Target price (0 for market)
     sl: Stop Loss
     tp: Take Profit
     magic: Magic number
     comment: Order comment"
  (let* ((side-str (if (keywordp side) (string-upcase (symbol-name side)) (string-upcase side)))
         (comment-reason (invalid-order-comment-reason comment)))
    (when comment-reason
      (error "Invalid ORDER_OPEN comment (~a): ~a"
             comment-reason
             (or comment "")))
    (make-protocol-message +MSG-ORDER-OPEN+
      `((strategy_id . ,strategy-id)
        (instrument . ,symbol)
        (side . ,side-str)
        (lot . ,(/ (round (* (float lot) 100.0)) 100.0)) ;; Ensure 2 decimal formatting
        (price . ,(float price))
        (sl . ,(float sl))
        (tp . ,(float tp))
        (magic . ,(if (numberp magic)
                      magic
                      (parse-integer (format nil "~a" magic) :junk-allowed t)))
        (comment . ,comment)))))

(defun make-heartbeat-message (&optional (status "OK"))
  "Construct a HEARTBEAT message."
  (make-protocol-message +MSG-HEARTBEAT+
    `((source . "BRAIN")
      (status . ,status))))
