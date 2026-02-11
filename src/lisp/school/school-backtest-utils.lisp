;;; school-backtest-utils.lisp - Backtest Utilities & Persistence
;;; Part of SRP refactoring to reduce school-backtest.lisp size

(in-package :swimmy.school)

(defvar *backtest-cache* (make-hash-table :test 'equal)
  "In-memory cache of backtest results. Key: STRATEGY-NAME, Value: RESULT-PLIST")

(defvar *backtest-cache-file* "data/backtest_cache.sexp"
  "Path to the persistent backtest cache file.")

(defvar *backtest-cache-validity* (* 24 3600)
  "Validity period for backtest results (24 hours).")

(defvar *backtest-send-queue* nil
  "Queue of backtest messages awaiting requester readiness.")

(defvar *backtest-send-queue-max* 5000
  "Max queued backtest messages before dropping.")

(defparameter *backtest-queue-last-flush* 0
  "Last time the backtest send queue was flushed.")

(defparameter *backtest-queue-flush-interval-sec* 1
  "Minimum seconds between periodic flush checks for backtest send queue.")

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

(defun backtest-pending-count ()
  (let ((recv (if (boundp 'swimmy.main::*backtest-recv-count*)
                  swimmy.main::*backtest-recv-count*
                  0)))
    (max 0 (- swimmy.globals::*backtest-submit-count* recv))))

(defun backtest-now-seconds ()
  "Return current time in seconds with sub-second resolution."
  (/ (float (get-internal-real-time) 1.0d0) internal-time-units-per-second))

(defun backtest-send-allowed-p ()
  (let* ((now (backtest-now-seconds))
         (pending (backtest-pending-count))
         (max-pending swimmy.globals::*backtest-max-pending*)
         (rate swimmy.globals::*backtest-rate-limit-per-sec*)
         (interval (if (and rate (> rate 0)) (/ 1.0d0 rate) 0.0d0))
         (elapsed (- now swimmy.globals::*backtest-last-send-ts*)))
    (and (< pending max-pending)
         (or (<= interval 0.0) (>= elapsed interval)))))

(defun enqueue-backtest-msg (msg)
  "Enqueue a backtest message if queue capacity allows."
  (when (< (length *backtest-send-queue*) *backtest-send-queue-max*)
    (setf *backtest-send-queue* (nconc *backtest-send-queue* (list msg)))
    t))

(defun flush-backtest-queue ()
  "Flush queued backtest messages once requester is ready."
  (when (and (boundp 'swimmy.globals:*backtest-requester*)
             swimmy.globals:*backtest-requester*)
    (loop while *backtest-send-queue*
          do (when (>= (backtest-pending-count) swimmy.globals::*backtest-max-pending*)
               (return))
             (let ((msg (pop *backtest-send-queue*)))
               (incf swimmy.globals::*backtest-submit-count*)
               (setf swimmy.globals::*backtest-last-send-ts* (backtest-now-seconds))
               (pzmq:send swimmy.globals:*backtest-requester* msg)))))

(defun flush-backtest-send-queue ()
  "Backward-compatible alias for flushing queued backtest messages."
  (flush-backtest-queue))

(defun maybe-flush-backtest-send-queue (&optional (now (get-universal-time)))
  "Flush queued backtest messages when interval has elapsed."
  (when (and *backtest-send-queue*
             (> (- now *backtest-queue-last-flush*)
                *backtest-queue-flush-interval-sec*))
    (setf *backtest-queue-last-flush* now)
    (flush-backtest-send-queue)))

(defun init-external-cmd-zmq ()
  "Initialize PUB socket for external command channel (Guardian 5559).
   Intended for non-brain processes (evolution daemon) so CMD sends don't drop."
  (unless (and (boundp 'swimmy.globals:*cmd-publisher*)
               swimmy.globals:*cmd-publisher*)
    (handler-case
        (let* ((ctx (pzmq:ctx-new))
               (pub (pzmq:socket ctx :pub))
               (endpoint (swimmy.core:zmq-connect-endpoint swimmy.core:*port-external*)))
          (pzmq:connect pub endpoint)
          (setf swimmy.globals:*cmd-publisher* pub)
          (format t "[ZMQ] 🔌 Connected CMD publisher (PUB -> ~d)~%" swimmy.core:*port-external*)
          t)
      (error (e)
        (format t "[ZMQ] ❌ Failed to init CMD publisher: ~a~%" e)
        nil)))
  (and (boundp 'swimmy.globals:*cmd-publisher*)
       swimmy.globals:*cmd-publisher*))

(defun send-zmq-msg (msg &key (target :cmd))
  "Helper to send ZMQ message with throttling.
   TARGET: :backtest routes to Backtest Service; :cmd routes to main Guardian."
  (when (eq target :backtest)
    (unless (backtest-send-allowed-p)
      (format t "[BACKTEST] ⏳ Throttled send (pending=~d max=~d)~%"
              (backtest-pending-count) swimmy.globals::*backtest-max-pending*)
      (return-from send-zmq-msg :throttled))
    (when (and swimmy.core:*backtest-service-enabled*
               (or (not (boundp 'swimmy.globals:*backtest-requester*))
                   (null swimmy.globals:*backtest-requester*)))
      (when (enqueue-backtest-msg msg)
        (format t "[BACKTEST] 📥 Queued until requester is ready.~%"))
      (return-from send-zmq-msg :queued))
    (incf swimmy.globals::*backtest-submit-count*)
    (setf swimmy.globals::*backtest-last-send-ts* (backtest-now-seconds)))
  (cond
    ((and (eq target :backtest)
          (boundp 'swimmy.globals:*backtest-requester*)
          swimmy.globals:*backtest-requester*)
     (handler-case
         (progn
           (pzmq:send swimmy.globals:*backtest-requester* msg)
           t)
       (error (e)
         (format t "[ZMQ] ❌ Backtest send failed: ~a~%" e)
         nil)))
    ((and (boundp 'swimmy.globals:*cmd-publisher*)
          swimmy.globals:*cmd-publisher*)
     ;; V27: Throttle to prevent Guardian EOF (Rust Buffer Overflow)
     (sleep 0.005)
     (when (eq target :backtest)
       (format t "[ZMQ] ⚠️ Backtest requester missing. Falling back to CMD publisher.~%"))
     (handler-case
         (progn
           (pzmq:send swimmy.globals:*cmd-publisher* msg)
           t)
       (error (e)
         (format t "[ZMQ] ❌ CMD send failed: ~a~%" e)
         nil)))
    (t
     (format t "[ZMQ] ❌ No publisher bound for target ~a. Msg dropped.~%" target)
     nil)))

(defun load-backtest-cache ()
  "Load backtest results from disk."
  (handler-case
      (when (probe-file *backtest-cache-file*)
        (let* ((sexp (swimmy.core:read-sexp-file *backtest-cache-file* :package :swimmy.school))
               (entries (and (listp sexp) (cdr (assoc 'entries sexp)))))
          (when (listp entries)
            (clrhash *backtest-cache*)
            (dolist (entry entries)
              (let ((name (cdr (assoc 'name entry)))
                    (timestamp (cdr (assoc 'timestamp entry)))
                    (result (cdr (assoc 'result entry))))
                (when name
                  (setf (gethash name *backtest-cache*)
                        (list :timestamp timestamp :result result)))))
            (format t "[BACKTEST] 📂 Loaded ~d cached results.~%" (hash-table-count *backtest-cache*)))))
    (error (e)
      (format t "[BACKTEST] ⚠️ Failed to load cache: ~a~%" e))))

(defun save-backtest-cache ()
  "Save backtest results to disk."
  (handler-case
      (let ((entries nil))
        (maphash (lambda (k v)
                   (push `((name . ,k)
                           (timestamp . ,(getf v :timestamp))
                           (result . ,(getf v :result)))
                         entries))
                 *backtest-cache*)
        (swimmy.core:write-sexp-atomic
         *backtest-cache-file*
         `((schema_version . 1)
           (entries . ,(nreverse entries)))))
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
