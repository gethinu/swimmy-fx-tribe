;;; execution-protocol.lisp - Strict Execution Protocol V2
;;; Phase 7 Implementation (memo3.txt Section 7)
;;;
;;; Responsibility: Define the schema for messages sent to the Execution Agent (MQL5/EA)

(in-package :swimmy.core)

(defparameter +protocol-version+ "2.0")

;; Message Types (Strict Dispatch)
(defparameter +MSG-HEARTBEAT+ "HEARTBEAT")
(defparameter +MSG-ORDER-OPEN+ "ORDER_OPEN")
(defparameter +MSG-ORDER-FILL+ "ORDER_FILL")
(defparameter +MSG-ORDER-ACK+ "ORDER_ACK")  ;; Incoming
(defparameter +MSG-TRADE-CLOSED+ "TRADE_CLOSED") ;; Incoming
(defparameter +MSG-ACCOUNT-INFO+ "ACCOUNT_INFO") ;; Incoming
(defparameter +MSG-HISTORY+ "HISTORY") ;; Incoming
(defparameter +MSG-TICK+ "TICK") ;; Incoming
(defparameter +MSG-SWAP-DATA+ "SWAP_DATA") ;; Incoming (Phase 28: Data Lake)

(defun generate-uuid ()
  "Generate a pseudo-UUID v4 string."
  ;; Lisp implementation of simple UUID generation
  (format nil "~(~x-~x-4~3,'0x-~x-~x~)"
          (random 4294967296)
          (random 65536)
          (random 4096)
          (logior #x8000 (random #x4000))
          (random 281474976710656)))

(defun sexp->string (form &key (package :swimmy.core))
  "Render S-expression FORM as a single-line string."
  (with-standard-io-syntax
    (let ((*print-case* :downcase)
          (*print-pretty* nil)
          (*print-right-margin* most-positive-fixnum)
          (*print-escape* t)
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
  (let ((side-str (if (keywordp side) (string-upcase (symbol-name side)) (string-upcase side))))
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
