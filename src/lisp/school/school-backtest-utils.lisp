;;; school-backtest-utils.lisp - Backtest Utilities & Persistence
;;; Part of SRP refactoring to reduce school-backtest.lisp size

(in-package :swimmy.school)

(defvar *backtest-cache* (make-hash-table :test 'equal)
  "In-memory cache of backtest results. Key: STRATEGY-NAME, Value: RESULT-PLIST")

(defvar *backtest-cache-file* "data/backtest_cache.json"
  "Path to the persistent backtest cache file.")

(defvar *backtest-cache-validity* (* 24 3600)
  "Validity period for backtest results (24 hours).")

(defun send-zmq-msg (msg)
  "Helper to send ZMQ message with Throttling (Speed Demon Fix)"
  ;; V27: Throttle to prevent Guardian EOF (Rust Buffer Overflow)
  (sleep 0.005) 
  (if (and (boundp '*backtest-requester*) *backtest-requester*)
      (pzmq:send *backtest-requester* msg)
      (pzmq:send *cmd-publisher* msg)))

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

(defun candles-to-json (candles)
  "Convert list of candle structs to JSON-friendly list of plists."
  (mapcar (lambda (c)
            `((time . ,(candle-timestamp c))
              (open . ,(candle-open c))
              (high . ,(candle-high c))
              (low . ,(candle-low c))
              (close . ,(candle-close c))
              (volume . ,(candle-volume c))))
          candles))

(defun candles-to-alist (candles)
  "Convert list of candle structs to alist for S-Expression protocol."
  (candles-to-json candles)) ; Same structure for alists

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
