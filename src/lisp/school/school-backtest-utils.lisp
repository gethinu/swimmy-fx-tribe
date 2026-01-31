;;; school-backtest-utils.lisp - Backtest Utilities & Persistence
;;; Part of SRP refactoring to reduce school-backtest.lisp size

(in-package :swimmy.school)

(defvar *backtest-cache* (make-hash-table :test 'equal)
  "In-memory cache of backtest results. Key: STRATEGY-NAME, Value: RESULT-PLIST")

(defvar *backtest-cache-file* "data/backtest_cache.json"
  "Path to the persistent backtest cache file.")

(defvar *backtest-cache-validity* (* 24 3600)
  "Validity period for backtest results (24 hours).")

(defun normalize-json-key (key)
  "Normalize a symbol/string key into a JSON object key string."
  (cond
    ((stringp key) key)
    ((symbolp key) (string-downcase (symbol-name key)))
    (t (format nil "~a" key))))

(defun alist-to-json (alist)
  "Convert an alist with symbol/string keys to a jsown object."
  (let ((obj (jsown:empty-object)))
    (dolist (pair alist obj)
      (let ((key-str (normalize-json-key (car pair)))
            (val (cdr pair)))
        (setf (jsown:val obj key-str) val)))))

(defun send-zmq-msg (msg &key (target :cmd))
  "Helper to send ZMQ message with throttling.
   TARGET: :backtest routes to Backtest Service; :cmd routes to main Guardian."
  ;; V27: Throttle to prevent Guardian EOF (Rust Buffer Overflow)
  (sleep 0.005)
  (cond
    ((and (eq target :backtest)
          (boundp 'swimmy.globals:*backtest-requester*)
          swimmy.globals:*backtest-requester*)
     (pzmq:send swimmy.globals:*backtest-requester* msg))
    ((and (boundp 'swimmy.globals:*cmd-publisher*)
          swimmy.globals:*cmd-publisher*)
     (when (eq target :backtest)
       (format t "[ZMQ] ⚠️ Backtest requester missing. Falling back to CMD publisher.~%"))
     (pzmq:send swimmy.globals:*cmd-publisher* msg))
    (t
     (format t "[ZMQ] ❌ No publisher bound for target ~a. Msg dropped.~%" target))))

(defun load-backtest-cache ()
  "Load backtest results from disk."
  (handler-case
      (when (probe-file *backtest-cache-file*)
        (let ((content (uiop:read-file-string *backtest-cache-file*)))
          (when (> (length content) 0)
            (let ((json (jsown:parse content)))
              (clrhash *backtest-cache*)
              (dolist (obj json)
                (let ((name (jsown:val obj "name"))
                      (timestamp (jsown:val obj "timestamp"))
                      (result (jsown:val obj "result")))
                  (setf (gethash name *backtest-cache*)
                        (list :timestamp timestamp :result result))))
              (format t "[BACKTEST] 📂 Loaded ~d cached results.~%" (hash-table-count *backtest-cache*))))))
    (error (e)
      (format t "[BACKTEST] ⚠️ Failed to load cache: ~a~%" e))))

(defun save-backtest-cache ()
  "Save backtest results to disk."
  (handler-case
      (let ((list nil))
        (maphash (lambda (k v)
                   (push (jsown:new-js
                           ("name" k)
                           ("timestamp" (getf v :timestamp))
                           ("result" (getf v :result)))
                         list))
                 *backtest-cache*)
        (ensure-directories-exist *backtest-cache-file*)
        (with-open-file (s *backtest-cache-file* :direction :output :if-exists :supersede)
          (write-string (jsown:to-json list) s)))
    (error (e)
      (format t "[BACKTEST] ❌ Failed to save cache: ~a~%" e))))

(defun cache-backtest-result (strategy-name result)
  "Cache a backtest result."
  (setf (gethash strategy-name *backtest-cache*)
        (list :timestamp (get-universal-time)
              :result result))
  ;; Save on every update (low volume)
  (save-backtest-cache))

(defun get-cached-backtest (strategy-name)
  "Get a valid cached backtest result. Returns NIL if expired or missing."
  (let ((cached (gethash strategy-name *backtest-cache*)))
    (when cached
      (let ((timestamp (getf cached :timestamp))
            (now (get-universal-time)))
        (if (< (- now timestamp) *backtest-cache-validity*)
            (getf cached :result)
            (progn
              (remhash strategy-name *backtest-cache*)
              nil))))))

(defun candles-to-alist (candles)
  "Convert list of candle structs to alist for Guardian S-Expression/JSON protocol."
  (mapcar (lambda (c)
            `((t . ,(candle-timestamp c))
              (o . ,(candle-open c))
              (h . ,(candle-high c))
              (l . ,(candle-low c))
              (c . ,(candle-close c))
              (v . ,(candle-volume c))))
          candles))

(defun candles-to-sexp (candles)
  "Convert list of candle structs to S-expression (alist list) for backtest protocol."
  (candles-to-alist candles))

(defun candles-to-json (candles)
  "Convert list of candle structs to JSON array of jsown objects."
  (mapcar #'alist-to-json (candles-to-alist candles)))

(defun find-balanced-end (str start)
  "Find the end of a balanced s-expression starting at position start"
  (let ((depth 0) (in-string nil))
    (loop for i from start below (length str)
          for c = (char str i)
          do (cond
               ((and (char= c #\\) (not in-string) (< (1+ i) (length str)))
                (incf i))  ; skip escaped char
               ((char= c #\") (setf in-string (not in-string)))
               ((and (not in-string) (char= c #\()) (incf depth))
               ((and (not in-string) (char= c #\)))
                (decf depth)
                (when (zerop depth) (return (1+ i)))))
          finally (return nil))))

(defun generate-data-id (candles &optional (suffix ""))
  "Generate a stable ID for a set of candles based on timestamps and count."
  (if (and candles (listp candles))
      (let* ((first (first candles))
             (last (car (last candles)))
             (count (length candles)))
        (format nil "DATA-~A-~A-~D~A" 
                (candle-timestamp first) 
                (candle-timestamp last) 
                count 
                suffix))
      (format nil "DATA-EMPTY~A" suffix)))
