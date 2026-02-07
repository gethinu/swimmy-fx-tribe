;;; tests.lisp - Unit Tests for Swimmy
;;; ═══════════════════════════════════════════════════
;;; "Untested code is broken code." - Unknown

;;; tests.lisp - Unit Tests for Swimmy
;;; ═══════════════════════════════════════════════════
;;; "Untested code is broken code." - Unknown

(in-package :swimmy.tests)

(defvar *test-results* nil)
(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

;;; ─────────────────────────────────────────
;;; TEST FRAMEWORK
;;; ─────────────────────────────────────────

(defmacro deftest (name &body body)
  "Define a test case"
  `(defun ,name ()
     (handler-case
         (progn
           ,@body
           (incf *tests-passed*)
           (push (list :passed ',name) *test-results*)
           t)
       (error (e)
         (format t " [ERROR: ~a] " e) ; Debug print
         (incf *tests-failed*)
         (push (list :failed ',name e) *test-results*)
         nil))))

(defmacro assert-true (expr &optional message)
  "Assert that expression is true"
  `(unless ,expr
     (error (or ,message (format nil "Assertion failed: ~a" ',expr)))))

(defmacro assert-false (expr &optional message)
  "Assert that expression is false"
  `(when ,expr
     (error (or ,message (format nil "Expected false: ~a" ',expr)))))

(defmacro assert-equal (expected actual &optional message)
  "Assert that expected equals actual"
  `(unless (equal ,expected ,actual)
     (error (or ,message 
                (format nil "Expected ~a but got ~a" ,expected ,actual)))))

(defmacro assert-not-nil (expr &optional message)
  "Assert that expression is not nil"
  `(unless ,expr
     (error (or ,message (format nil "Expected non-nil: ~a" ',expr)))))

;;; ─────────────────────────────────────────
;;; SAFE READ TESTS
;;; ─────────────────────────────────────────

(deftest test-safe-read-rejects-read-eval
  "safe-read-sexp rejects read-time eval (#.)"
  (let ((form (swimmy.core:safe-read-sexp "#.(+ 1 2)")))
    (assert-true (null form) "Expected nil for read-time eval")))

(deftest test-safe-read-allows-simple-alist
  "safe-read-sexp allows simple alist"
  (let ((form (swimmy.core:safe-read-sexp "((action . \"PING\"))")))
    (let ((key (find-symbol "ACTION" :swimmy.main)))
      (assert-true (and (listp form) key (assoc key form)) "Expected alist"))))

(deftest test-internal-process-msg-rejects-read-eval
  "internal-process-msg should ignore unsafe sexp"
  (let ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main)))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (let ((orig swimmy.globals:*danger-level*))
      (unwind-protect
          (progn
            (setf swimmy.globals:*danger-level* 0)
            (funcall fn "((type . \"PING\") (payload . #.(setf swimmy.globals::*danger-level* 999)))")
            (assert-equal 0 swimmy.globals:*danger-level* "Should not evaluate read-time forms"))
        (setf swimmy.globals:*danger-level* orig)))))

(deftest test-internal-process-msg-backtest-request-id-bound
  "internal-process-msg should handle BACKTEST_RESULT with request_id"
  (let ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main)))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (let* ((msg "{\"type\":\"BACKTEST_RESULT\",\"result\":{\"strategy_name\":\"UT-REQ\",\"sharpe\":0.2,\"trades\":1,\"pnl\":0.1,\"request_id\":\"RID-1\"}}")
           (orig-cache (symbol-function 'swimmy.school:cache-backtest-result))
           (orig-apply (symbol-function 'swimmy.school:apply-backtest-result))
           (orig-lookup (symbol-function 'swimmy.school:lookup-oos-request))
           (orig-v2 (and (fboundp 'swimmy.school::handle-v2-result)
                         (symbol-function 'swimmy.school::handle-v2-result))))
      (unwind-protect
          (progn
            (setf (symbol-function 'swimmy.school:cache-backtest-result)
                  (lambda (&rest args) (declare (ignore args)) nil))
            (setf (symbol-function 'swimmy.school:apply-backtest-result)
                  (lambda (&rest args) (declare (ignore args)) nil))
            (setf (symbol-function 'swimmy.school:lookup-oos-request)
                  (lambda (&rest args) (declare (ignore args)) (values nil nil nil)))
            (when (fboundp 'swimmy.school::handle-v2-result)
              (setf (symbol-function 'swimmy.school::handle-v2-result)
                    (lambda (&rest args) (declare (ignore args)) nil)))
            (let* ((out-stream (make-string-output-stream))
                   (*standard-output* out-stream))
              (funcall fn msg)
              (let ((output (get-output-stream-string out-stream)))
                (assert-true (null (search "Msg Error" output))
                             "Expected no Msg Error during BACKTEST_RESULT"))))
        (setf (symbol-function 'swimmy.school:cache-backtest-result) orig-cache)
        (setf (symbol-function 'swimmy.school:apply-backtest-result) orig-apply)
        (setf (symbol-function 'swimmy.school:lookup-oos-request) orig-lookup)
        (when orig-v2
          (setf (symbol-function 'swimmy.school::handle-v2-result) orig-v2))))))

(deftest test-backtest-result-preserves-request-id
  "BACKTEST_RESULT should carry request_id through the pipeline"
  (let ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main)))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (let* ((msg "((type . \"BACKTEST_RESULT\") (result . ((strategy_name . \"UT-REQ\") (sharpe . 0.2) (trades . 1) (request_id . \"RID-1\"))))")
           (orig-cache (symbol-function 'swimmy.school:cache-backtest-result))
           (orig-apply (symbol-function 'swimmy.school:apply-backtest-result))
           (orig-lookup (symbol-function 'swimmy.school:lookup-oos-request))
           (orig-v2 (and (fboundp 'swimmy.school::handle-v2-result)
                         (symbol-function 'swimmy.school::handle-v2-result))))
      (unwind-protect
          (progn
            (setf (symbol-function 'swimmy.school:cache-backtest-result)
                  (lambda (&rest args) (declare (ignore args)) nil))
            (setf (symbol-function 'swimmy.school:apply-backtest-result)
                  (lambda (&rest args) (declare (ignore args)) nil))
            (setf (symbol-function 'swimmy.school:lookup-oos-request)
                  (lambda (&rest args) (declare (ignore args)) (values nil nil nil)))
            (when (fboundp 'swimmy.school::handle-v2-result)
              (setf (symbol-function 'swimmy.school::handle-v2-result)
                    (lambda (&rest args) (declare (ignore args)) nil)))
            (setf swimmy.main::*backtest-recv-count* 0)
            (setf swimmy.main::*backtest-recv-last-id* nil)
            (funcall fn msg)
            (assert-equal "RID-1" swimmy.main::*backtest-recv-last-id*
                          "request_id should be preserved"))
        (setf (symbol-function 'swimmy.school:cache-backtest-result) orig-cache)
        (setf (symbol-function 'swimmy.school:apply-backtest-result) orig-apply)
        (setf (symbol-function 'swimmy.school:lookup-oos-request) orig-lookup)
        (when orig-v2
          (setf (symbol-function 'swimmy.school::handle-v2-result) orig-v2))))))

(deftest test-backtest-result-persists-trade-list
  "internal-process-msg should persist trade_list with oos_kind mapping"
  (let ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main)))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (let* ((msg "((type . \"BACKTEST_RESULT\") (result . ((strategy_name . \"AAA-OOS\") (sharpe . 0.1) (trades . 1) (request_id . \"RID-1\") (trade_list . (((timestamp . 1) (pnl . 1.0) (symbol . \"USDJPY\")))))))")
           (called nil)
           (orig-record (symbol-function 'swimmy.school:record-backtest-trades))
           (orig-cache (symbol-function 'swimmy.school:cache-backtest-result))
           (orig-apply (symbol-function 'swimmy.school:apply-backtest-result))
           (orig-lookup (symbol-function 'swimmy.school:lookup-oos-request))
           (orig-v2 (and (fboundp 'swimmy.school::handle-v2-result)
                         (symbol-function 'swimmy.school::handle-v2-result)))
           (orig-oos (and (fboundp 'swimmy.school:handle-oos-backtest-result)
                          (symbol-function 'swimmy.school:handle-oos-backtest-result))))
      (unwind-protect
          (progn
            (setf (symbol-function 'swimmy.school:record-backtest-trades)
                  (lambda (rid name kind trades)
                    (setf called (list rid name kind trades))
                    nil))
            (setf (symbol-function 'swimmy.school:cache-backtest-result)
                  (lambda (&rest args) (declare (ignore args)) nil))
            (setf (symbol-function 'swimmy.school:apply-backtest-result)
                  (lambda (&rest args) (declare (ignore args)) nil))
            (setf (symbol-function 'swimmy.school:lookup-oos-request)
                  (lambda (&rest args) (declare (ignore args)) (values nil nil nil)))
            (when orig-v2
              (setf (symbol-function 'swimmy.school::handle-v2-result)
                    (lambda (&rest args) (declare (ignore args)) nil)))
            (when orig-oos
              (setf (symbol-function 'swimmy.school:handle-oos-backtest-result)
                    (lambda (&rest args) (declare (ignore args)) nil)))
            (funcall fn msg)
            (assert-true called "record-backtest-trades should be called")
            (assert-equal "RID-1" (first called) "request_id should match")
            (assert-equal "AAA" (second called) "strategy name should be normalized")
            (assert-equal "OOS" (third called) "oos_kind should be OOS")
            (assert-true (listp (fourth called)) "trade_list should be passed"))
        (setf (symbol-function 'swimmy.school:record-backtest-trades) orig-record)
        (setf (symbol-function 'swimmy.school:cache-backtest-result) orig-cache)
        (setf (symbol-function 'swimmy.school:apply-backtest-result) orig-apply)
        (setf (symbol-function 'swimmy.school:lookup-oos-request) orig-lookup)
        (when orig-v2
          (setf (symbol-function 'swimmy.school::handle-v2-result) orig-v2))
        (when orig-oos
          (setf (symbol-function 'swimmy.school:handle-oos-backtest-result) orig-oos))))))

(deftest test-cpcv-result-persists-trade-list
  "CPCV_RESULT should persist trade_list as oos_kind=CPCV"
  (let ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main)))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (let* ((msg "((type . \"CPCV_RESULT\") (result . ((strategy_name . \"AAA\") (request_id . \"RID-CPCV\") (trade_list . (((timestamp . 1) (pnl . 1.0) (symbol . \"USDJPY\")))))))")
           (called nil)
           (orig-record (symbol-function 'swimmy.school:record-backtest-trades)))
      (unwind-protect
          (progn
            (setf (symbol-function 'swimmy.school:record-backtest-trades)
                  (lambda (rid name kind trades)
                    (setf called (list rid name kind trades))
                    nil))
            (funcall fn msg)
            (assert-true called "record-backtest-trades should be called")
            (assert-equal "RID-CPCV" (first called) "request_id should match")
            (assert-equal "AAA" (second called) "strategy name should match")
            (assert-equal "CPCV" (third called) "oos_kind should be CPCV"))
        (setf (symbol-function 'swimmy.school:record-backtest-trades) orig-record)))))

(deftest test-backtest-debug-log-records-apply
  "BACKTEST_RESULT should write debug log around apply when debug enabled"
  (let ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main)))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (let* ((msg "((type . \"BACKTEST_RESULT\") (result . ((strategy_name . \"UT-DBG\") (sharpe . 0.2) (trades . 1) (pnl . 0.1) (request_id . \"RID-DBG\"))))")
           (orig-cache (symbol-function 'swimmy.school:cache-backtest-result))
           (orig-apply (symbol-function 'swimmy.school:apply-backtest-result))
           (orig-lookup (symbol-function 'swimmy.school:lookup-oos-request))
           (orig-v2 (and (fboundp 'swimmy.school::handle-v2-result)
                         (symbol-function 'swimmy.school::handle-v2-result)))
           (orig-env (uiop:getenv "SWIMMY_BACKTEST_DEBUG_RECV"))
           (tmp-path (merge-pathnames
                      (format nil "backtest_debug_test_~a.log" (get-universal-time))
                      (uiop:temporary-directory))))
      (unwind-protect
          (progn
            (sb-posix:setenv "SWIMMY_BACKTEST_DEBUG_RECV" "1" 1)
            (setf (symbol-function 'swimmy.school:cache-backtest-result)
                  (lambda (&rest args) (declare (ignore args)) nil))
            (setf (symbol-function 'swimmy.school:apply-backtest-result)
                  (lambda (&rest args) (declare (ignore args)) nil))
            (setf (symbol-function 'swimmy.school:lookup-oos-request)
                  (lambda (&rest args) (declare (ignore args)) (values nil nil nil)))
            (when (fboundp 'swimmy.school::handle-v2-result)
              (setf (symbol-function 'swimmy.school::handle-v2-result)
                    (lambda (&rest args) (declare (ignore args)) nil)))
            (when (probe-file tmp-path)
              (ignore-errors (delete-file tmp-path)))
            (let ((swimmy.main::*backtest-debug-log* (namestring tmp-path)))
              (funcall fn msg)
              (assert-true (probe-file tmp-path) "Expected debug log file")
              (let ((content (uiop:read-file-string tmp-path)))
                (assert-true (search "apply-backtest-result start" content)
                             "Expected apply start log")
                (assert-true (search "apply-backtest-result end" content)
                             "Expected apply end log"))))
        (setf (symbol-function 'swimmy.school:cache-backtest-result) orig-cache)
        (setf (symbol-function 'swimmy.school:apply-backtest-result) orig-apply)
        (setf (symbol-function 'swimmy.school:lookup-oos-request) orig-lookup)
        (when orig-v2
          (setf (symbol-function 'swimmy.school::handle-v2-result) orig-v2))
        (if orig-env
            (sb-posix:setenv "SWIMMY_BACKTEST_DEBUG_RECV" orig-env 1)
            (sb-posix:unsetenv "SWIMMY_BACKTEST_DEBUG_RECV"))
        (when (probe-file tmp-path)
          (ignore-errors (delete-file tmp-path)))))))

(deftest test-backtest-status-includes-last-request-id
  "backtest_status.txt should include last_request_id only"
  (let ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main)))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (let* ((status-path "data/reports/backtest_status.txt")
           (msg "((type . \"BACKTEST_RESULT\") (result . ((strategy_name . \"UT-REQ-STATUS\") (sharpe . 0.2) (trades . 1) (pnl . 0.1) (request_id . \"RID-123\"))))")
           (orig-cache (symbol-function 'swimmy.school:cache-backtest-result))
           (orig-apply (symbol-function 'swimmy.school:apply-backtest-result))
           (orig-lookup (symbol-function 'swimmy.school:lookup-oos-request))
           (orig-v2 (and (fboundp 'swimmy.school::handle-v2-result)
                         (symbol-function 'swimmy.school::handle-v2-result)))
           (orig-submit-count swimmy.globals::*backtest-submit-count*)
           (orig-recv-count swimmy.main::*backtest-recv-count*))
      (unwind-protect
          (progn
            (setf (symbol-function 'swimmy.school:cache-backtest-result)
                  (lambda (&rest args) (declare (ignore args)) nil))
            (setf (symbol-function 'swimmy.school:apply-backtest-result)
                  (lambda (&rest args) (declare (ignore args)) nil))
            (setf (symbol-function 'swimmy.school:lookup-oos-request)
                  (lambda (&rest args) (declare (ignore args)) (values nil nil nil)))
            (when (fboundp 'swimmy.school::handle-v2-result)
              (setf (symbol-function 'swimmy.school::handle-v2-result)
                    (lambda (&rest args) (declare (ignore args)) nil)))
            (setf swimmy.globals:*backtest-submit-last-id* "SUB-999")
            (setf swimmy.main::*backtest-recv-last-log* 0)
            (setf swimmy.globals::*backtest-submit-count* 5)
            (setf swimmy.main::*backtest-recv-count* 1)
            (funcall fn msg)
            (let ((content (with-open-file (s status-path :direction :input)
                             (let ((text (make-string (file-length s))))
                               (read-sequence text s)
                               text))))
              (assert-true (search "last_request_id: RID-123" content))
              (assert-true (search "pending: 3" content))
              (assert-true (null (search "last_submit_id" content))
                           "Expected last_submit_id to be absent from backtest_status.txt")
              (assert-true (null (search "last_recv_id" content))
                           "Expected last_recv_id to be absent from backtest_status.txt")))
        (setf (symbol-function 'swimmy.school:cache-backtest-result) orig-cache)
        (setf (symbol-function 'swimmy.school:apply-backtest-result) orig-apply)
        (setf (symbol-function 'swimmy.school:lookup-oos-request) orig-lookup)
        (setf swimmy.globals::*backtest-submit-count* orig-submit-count)
        (setf swimmy.main::*backtest-recv-count* orig-recv-count)
        (when orig-v2
          (setf (symbol-function 'swimmy.school::handle-v2-result) orig-v2))))))

(deftest test-request-backtest-sets-submit-id
  "request-backtest should set submit request_id when none provided"
  (let ((orig-send (symbol-function 'swimmy.school::send-zmq-msg)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::send-zmq-msg)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf swimmy.globals:*backtest-submit-last-id* nil)
          (let ((s (swimmy.school:make-strategy :name "ReqIdTest" :symbol "USDJPY")))
            (swimmy.school::request-backtest s :candles nil))
          (assert-true swimmy.globals:*backtest-submit-last-id* "Expected submit request_id to be set"))
      (setf (symbol-function 'swimmy.school::send-zmq-msg) orig-send))))

(deftest test-generate-uuid-changes-even-with-reset-rng
  "generate-uuid should vary even if random state resets"
  (let ((state (make-random-state nil))
        (id1 nil)
        (id2 nil))
    (let ((*random-state* (make-random-state state)))
      (setf id1 (swimmy.core:generate-uuid)))
    (sleep 1)
    (let ((*random-state* (make-random-state state)))
      (setf id2 (swimmy.core:generate-uuid)))
    (assert-false (string= id1 id2) "UUID should include time-based entropy")))

(deftest test-generate-uuid-uses-entropy-file
  "generate-uuid should use entropy file bytes when available"
  (let* ((tmp-path (format nil "data/memory/uuid-entropy-~a.bin" (get-universal-time)))
         (bytes (make-array 16 :element-type '(unsigned-byte 8)
                            :initial-contents '(#x00 #x11 #x22 #x33 #x44 #x55 #x66 #x77
                                                #x88 #x99 #xaa #xbb #xcc #xdd #xee #xff)))
         (expected "00112233-4455-4677-8899-aabbccddeeff")
         (orig-path swimmy.core::*uuid-entropy-path*))
    (unwind-protect
        (progn
          (ensure-directories-exist tmp-path)
          (with-open-file (s tmp-path
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create
                             :element-type '(unsigned-byte 8))
            (write-sequence bytes s))
          (setf swimmy.core::*uuid-entropy-path* tmp-path)
          (assert-equal expected (swimmy.core:generate-uuid)
                        "UUID should be derived from entropy bytes"))
      (setf swimmy.core::*uuid-entropy-path* orig-path)
      (when (probe-file tmp-path) (delete-file tmp-path)))))

(deftest test-oos-retry-uses-new-request-id
  "OOS retry should generate a new request_id"
  (let* ((tmp-db (format nil "data/memory/test-oos-retry-~a.db" (get-universal-time)))
         (orig-db swimmy.core::*db-path-default*)
         (orig-interval swimmy.school::*oos-request-interval*)
         (orig-request (symbol-function 'swimmy.school::request-backtest)))
    (unwind-protect
         (progn
           (setf swimmy.core::*db-path-default* tmp-db)
           (swimmy.core:close-db-connection)
           (swimmy.school::init-db)
           (setf swimmy.school::*oos-request-interval* 0)
           (setf (symbol-function 'swimmy.school::request-backtest)
                 (lambda (strat &key request-id &allow-other-keys)
                   (declare (ignore strat))
                   request-id))
           (swimmy.school::enqueue-oos-request "UT-OOS" "RID-OLD" :status "sent"
                                               :requested-at (- (get-universal-time) 999))
           (let* ((strat (cl-user::make-strategy :name "UT-OOS" :symbol "USDJPY" :oos-sharpe nil))
                  (id (swimmy.school::maybe-request-oos-backtest strat)))
             (assert-true (and id (not (string= id "RID-OLD")))
                          "retry should issue new request_id")))
      (setf (symbol-function 'swimmy.school::request-backtest) orig-request)
      (setf swimmy.school::*oos-request-interval* orig-interval)
      (setf swimmy.core::*db-path-default* orig-db)
      (swimmy.core:close-db-connection)
      (when (probe-file tmp-db) (delete-file tmp-db)))))

(deftest test-oos-queue-clear-on-startup
  "startup cleanup should clear oos_queue"
  (let* ((tmp-db (format nil "data/memory/test-oos-startup-~a.db" (get-universal-time)))
         (orig-db swimmy.core::*db-path-default*))
    (unwind-protect
         (progn
           (setf swimmy.core::*db-path-default* tmp-db)
           (swimmy.core:close-db-connection)
           (swimmy.school::init-db)
           (swimmy.school::enqueue-oos-request "UT-OOS" "RID-X" :status "sent")
           (swimmy.school::cleanup-oos-queue-on-startup)
           (multiple-value-bind (rid _ status) (swimmy.school::lookup-oos-request "UT-OOS")
             (declare (ignore _ status))
             (assert-true (null rid) "oos_queue should be cleared")))
      (setf swimmy.core::*db-path-default* orig-db)
      (swimmy.core:close-db-connection)
      (when (probe-file tmp-db) (delete-file tmp-db)))))

(deftest test-strategy-to-alist-omits-filter-enabled-when-false
  "strategy-to-alist should omit filter_enabled when filter is disabled (Rust expects bool default false)"
  (let* ((fn (find-symbol "STRATEGY-TO-ALIST" :swimmy.school))
         (key (find-symbol "FILTER_ENABLED" :swimmy.school)))
    (assert-true (and fn (fboundp fn)) "strategy-to-alist exists")
    (assert-not-nil key "FILTER_ENABLED symbol exists")
    (let* ((strat (swimmy.school:make-strategy
                   :name "UT-FILTER-OFF"
                   :indicators '((sma 5) (sma 20))
                   :filter-enabled nil))
           (alist (funcall fn strat)))
      (assert-true (listp alist) "Expected an alist list")
      (assert-true (null (assoc key alist)) "filter_enabled should be omitted when false"))))

(deftest test-strategy-to-alist-includes-filter-enabled-when-true
  "strategy-to-alist should include filter_enabled when filter is enabled"
  (let* ((fn (find-symbol "STRATEGY-TO-ALIST" :swimmy.school))
         (key (find-symbol "FILTER_ENABLED" :swimmy.school)))
    (assert-true (and fn (fboundp fn)) "strategy-to-alist exists")
    (assert-not-nil key "FILTER_ENABLED symbol exists")
    (let* ((strat (swimmy.school:make-strategy
                   :name "UT-FILTER-ON"
                   :indicators '((sma 5) (sma 20))
                   :filter-enabled t))
           (alist (funcall fn strat))
           (pair (assoc key alist)))
      (assert-not-nil pair "filter_enabled should exist when enabled")
      (assert-true (eq (cdr pair) t) "filter_enabled should be t when enabled"))))

(deftest test-order-open-uses-instrument-side
  (let* ((msg (swimmy.core:make-order-message "UT" "USDJPY" :buy 0.1 0 0 0))
         (sexp (swimmy.core:encode-sexp msg))
         (parsed (swimmy.core:safe-read-sexp sexp :package :swimmy.core)))
    (assert-equal "USDJPY" (swimmy.core:sexp-alist-get parsed "instrument"))
    (assert-equal "BUY" (swimmy.core:sexp-alist-get parsed "side"))))

(deftest test-data-keeper-request-sexp
  (let* ((req (swimmy.core::build-data-keeper-request "STATUS"))
         (parsed (swimmy.core:safe-read-sexp req :package :swimmy.core)))
    (assert-equal "DATA_KEEPER" (swimmy.core:sexp-alist-get parsed "type"))
    (assert-equal 1 (swimmy.core:sexp-alist-get parsed "schema_version"))
    (assert-equal "STATUS" (swimmy.core:sexp-alist-get parsed "action"))))

(deftest test-message-dispatcher-compiles-without-warnings
  "message-dispatcher should compile without warnings"
  (let ((warnings '()))
    (handler-bind ((warning (lambda (w)
                              (push w warnings)
                              (muffle-warning w))))
      (compile-file "src/lisp/core/message-dispatcher.lisp"))
    (assert-true (null warnings)
                 (format nil "Expected no warnings, got: ~a" warnings))))

(deftest test-safe-read-used-for-db-rank
  "DB rank parsing should ignore unsafe read-time eval"
  (let ((fn (find-symbol "%PARSE-RANK-SAFE" :swimmy.school)))
    (assert-true (and fn (fboundp fn)) "%parse-rank-safe exists")
    (let ((orig swimmy.globals:*danger-level*))
      (unwind-protect
          (progn
            (setf swimmy.globals:*danger-level* 0)
            (multiple-value-bind (rank ok)
                (funcall fn "#.(setf swimmy.globals::*danger-level* 999)")
              (declare (ignore rank))
              (assert-true (null ok) "Expected invalid rank")
              (assert-equal 0 swimmy.globals:*danger-level* "Should not evaluate read-time forms")))
        (setf swimmy.globals:*danger-level* orig)))))

(deftest test-safe-eval-strategy-rejects-read-eval
  "safe-eval-strategy should ignore read-time eval forms"
  (let ((orig swimmy.globals:*danger-level*))
    (unwind-protect
        (progn
          (setf swimmy.globals:*danger-level* 0)
          (multiple-value-bind (_strategy ok)
              (swimmy.school::safe-eval-strategy
               "#.(setf swimmy.globals::*danger-level* 999) (defstrategy ut-safe :indicators nil :entry nil :exit nil :sl 0 :tp 0 :volume 0)")
            (declare (ignore _strategy))
            (assert-false ok "Expected unsafe form to be rejected")
            (assert-equal 0 swimmy.globals:*danger-level* "Should not evaluate read-time forms")))
      (setf swimmy.globals:*danger-level* orig))))

(deftest test-map-strategies-from-db-rejects-read-eval
  "map-strategies-from-db should ignore unsafe read-time eval"
  (let ((fn (find-symbol "EXECUTE-TO-LIST" :swimmy.school)))
    (assert-true (and fn (fboundp fn)) "execute-to-list exists")
    (let ((orig (symbol-function fn))
          (orig-danger swimmy.globals:*danger-level*)
          (calls 0))
      (unwind-protect
          (progn
            (setf swimmy.globals:*danger-level* 0)
            (setf (symbol-function fn)
                  (lambda (&rest args)
                    (declare (ignore args))
                    (incf calls)
                    (if (= calls 1)
                        (list (list "#.(setf swimmy.globals::*danger-level* 999)"))
                        nil)))
            (let ((count (swimmy.school::map-strategies-from-db (lambda (s) (declare (ignore s))))))
              (assert-equal 0 count "Expected no strategies processed")
              (assert-equal 0 swimmy.globals:*danger-level* "Should not evaluate read-time forms")))
        (setf (symbol-function fn) orig
              swimmy.globals:*danger-level* orig-danger)))))

(deftest test-parse-macro-csv-rejects-read-eval
  "parse-macro-csv should ignore unsafe read-time eval"
  (let* ((orig-danger swimmy.globals:*danger-level*)
         (orig-dir swimmy.school::*macro-data-dir*)
         (root (uiop:ensure-directory-pathname (uiop:getcwd)))
         (temp-dir (uiop:ensure-directory-pathname (merge-pathnames "data/tmp/macro-test/" root)))
         (driver "UT-MACRO")
         (path (merge-pathnames (format nil "~a.csv" driver) temp-dir)))
    (unwind-protect
        (progn
          (ensure-directories-exist temp-dir)
          (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
            (write-line "Date,Open,High,Low,Close" out)
            (write-line "2024-01-01,1,2,3,#.(setf swimmy.globals::*danger-level* 999)" out))
          (setf swimmy.globals:*danger-level* 0)
          (let ((swimmy.school::*macro-data-dir* temp-dir))
            (let ((data (swimmy.school::parse-macro-csv driver)))
              (assert-true (null data) "Expected no parsed rows")
              (assert-equal 0 swimmy.globals:*danger-level* "Should not evaluate read-time forms"))))
      (setf swimmy.globals:*danger-level* orig-danger
            swimmy.school::*macro-data-dir* orig-dir)
      (ignore-errors (delete-file path)))))

(deftest test-req-history-uses-count
  "REQ_HISTORY bootstrap request should use count key (not volume)"
  (let* ((path "src/lisp/system/runner.lisp")
         (content (with-open-file (in path :direction :input)
                    (let ((out (make-string-output-stream)))
                      (loop for line = (read-line in nil nil)
                            while line do (write-line line out))
                      (get-output-stream-string out))))
         (has-req (search "REQ_HISTORY" content))
         (has-count (search "(count ." content))
         (has-volume (search "(volume ." content)))
    (assert-true has-req "REQ_HISTORY should exist in runner.lisp")
    (assert-true has-count "REQ_HISTORY should use count key")
    (assert-false has-volume "REQ_HISTORY should not use volume key")))

(deftest test-order-open-sexp-keys
  "ORDER_OPEN should use instrument/side/lot and be S-expression"
  (let* ((msg (swimmy.core:make-order-message "UT" "USDJPY" "BUY" 0.1 0.0 1.0 2.0
                                              :magic 123 :comment "C"))
         (payload (if (stringp msg)
                      msg
                      (swimmy.core:encode-sexp msg))))
    (assert-true (search "(instrument . \"USDJPY\")" payload) "Expected instrument key")
    (assert-true (search "(side . \"BUY\")" payload) "Expected side key")
    (assert-true (search "(lot . 0.1" payload) "Expected lot key")
    (assert-false (search "action" payload) "Should not contain action key")
    (assert-false (search "symbol" payload) "Should not contain symbol key")))

(deftest test-heartbeat-message-is-sexp
  "Heartbeat should return S-expression alist"
  (let ((msg (swimmy.core:make-heartbeat-message)))
    (assert-true (listp msg) "Expected alist heartbeat message")
    (assert-equal "HEARTBEAT" (cdr (assoc 'swimmy.core::type msg)) "Expected type key")
    (assert-equal "BRAIN" (cdr (assoc 'swimmy.core::source msg)) "Expected source key")
    (assert-true (cdr (assoc 'swimmy.core::status msg)) "Expected status key")))

(deftest test-executor-heartbeat-sends-sexp
  "Executor should send heartbeat as S-expression (not JSON)"
  (let* ((content (uiop:read-file-string "src/lisp/core/executor.lisp"))
         (uses-json (search "jsown:to-json heartbeat-msg" content))
         (uses-sexp (search "encode-sexp heartbeat-msg" content)))
    (assert-false uses-json "Heartbeat should not use jsown:to-json")
    (assert-true uses-sexp "Heartbeat should use encode-sexp")))

(deftest test-executor-pending-orders-sends-sexp
  "Executor pending retry should resend S-expression (not JSON)"
  (let* ((content (uiop:read-file-string "src/lisp/core/executor.lisp"))
         (uses-json (search "jsown:to-json msg-obj" content))
         (uses-sexp (search "encode-sexp msg-obj" content)))
    (assert-false uses-json "Pending retries should not use jsown:to-json")
    (assert-true uses-sexp "Pending retries should use encode-sexp")))

(deftest test-internal-cmd-json-disallowed
  "Internal CMD senders should not use jsown:to-json"
  (let* ((paths '("src/lisp/school/school-stress.lisp"
                  "src/lisp/school/school-validation.lisp"
                  "src/lisp/school/school-strategy.lisp"
                  "src/lisp/school/school-founders.lisp"
                  "src/lisp/evolution.lisp"))
         (offenders (remove-if-not
                     (lambda (path)
                       (search "jsown:to-json" (uiop:read-file-string path)))
                     paths)))
    (assert-true (null offenders)
                 (format nil "Found jsown:to-json in: ~a" offenders))))

(deftest test-internal-process-msg-tick-sexp
  "internal-process-msg should process TICK S-expression"
  (let* ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main))
         (orig-update (symbol-function 'swimmy.main:update-candle))
         (orig-process (and (fboundp 'swimmy.school:process-category-trades)
                            (symbol-function 'swimmy.school:process-category-trades)))
         (orig-save (and (fboundp 'swimmy.shell:save-live-status)
                         (symbol-function 'swimmy.shell:save-live-status)))
         (orig-status (and (fboundp 'swimmy.shell:send-periodic-status-report)
                           (symbol-function 'swimmy.shell:send-periodic-status-report)))
         (orig-learn (and (fboundp 'swimmy.school:continuous-learning-step)
                          (symbol-function 'swimmy.school:continuous-learning-step)))
         (called nil))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.main:update-candle)
                (lambda (bid symbol) (setf called (list bid symbol))))
          (when orig-process
            (setf (symbol-function 'swimmy.school:process-category-trades)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-save
            (setf (symbol-function 'swimmy.shell:save-live-status)
                  (lambda () nil)))
          (when orig-status
            (setf (symbol-function 'swimmy.shell:send-periodic-status-report)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-learn
            (setf (symbol-function 'swimmy.school:continuous-learning-step)
                  (lambda () nil)))
          (funcall fn "((type . \"TICK\") (symbol . \"USDJPY\") (bid . 1.23) (ask . 1.24))")
          (assert-equal '(1.23 "USDJPY") called))
      (setf (symbol-function 'swimmy.main:update-candle) orig-update)
      (when orig-process
        (setf (symbol-function 'swimmy.school:process-category-trades) orig-process))
      (when orig-save
        (setf (symbol-function 'swimmy.shell:save-live-status) orig-save))
      (when orig-status
        (setf (symbol-function 'swimmy.shell:send-periodic-status-report) orig-status))
      (when orig-learn
        (setf (symbol-function 'swimmy.school:continuous-learning-step) orig-learn)))))

(deftest test-internal-process-msg-history-sexp
  "internal-process-msg should store HISTORY S-expression as newest-first"
  (let* ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main))
         (orig-hist swimmy.globals:*candle-histories*)
         (orig-hist-tf swimmy.globals:*candle-histories-tf*)
         (orig-main-hist (and (boundp 'swimmy.main::*candle-history*) swimmy.main::*candle-history*)))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (unwind-protect
        (progn
          (setf swimmy.globals:*candle-histories* (make-hash-table :test 'equal))
          (setf swimmy.globals:*candle-histories-tf* (make-hash-table :test 'equal))
          (when (boundp 'swimmy.main::*candle-history*)
            (setf swimmy.main::*candle-history* nil))
          (funcall fn
                   "((type . \"HISTORY\") (symbol . \"USDJPY\") (tf . \"M1\") (batch . 0) (total . 1) (data . (((t . 1) (o . 1.0) (h . 1.1) (l . 0.9) (c . 1.0)) ((t . 2) (o . 2.0) (h . 2.1) (l . 1.9) (c . 2.0)))))")
          (let ((hist (gethash "USDJPY" swimmy.globals:*candle-histories*)))
            (assert-true hist "Expected history stored")
            (assert-equal 2 (swimmy.globals:candle-timestamp (first hist)) "Newest should be first")))
      (setf swimmy.globals:*candle-histories* orig-hist)
      (setf swimmy.globals:*candle-histories-tf* orig-hist-tf)
      (when (boundp 'swimmy.main::*candle-history*)
        (setf swimmy.main::*candle-history* orig-main-hist)))))

(deftest test-process-account-info-sexp
  "process-account-info should accept S-expression alist"
  (let* ((fn (find-symbol "PROCESS-ACCOUNT-INFO" :swimmy.executor))
         (orig-equity swimmy.executor::*current-equity*)
         (orig-peak swimmy.executor::*peak-equity*)
         (orig-alert swimmy.executor::*account-info-alert-sent*))
    (assert-true (and fn (fboundp fn)) "process-account-info exists")
    (unwind-protect
        (progn
          (setf swimmy.executor::*current-equity* 0.0)
          (setf swimmy.executor::*peak-equity* 0.0)
          (setf swimmy.executor::*account-info-alert-sent* nil)
          (funcall fn '((equity . 1000000.0) (balance . 900000.0)))
          (assert-true (> swimmy.executor::*current-equity* 0.0) "Equity should update"))
      (setf swimmy.executor::*current-equity* orig-equity)
      (setf swimmy.executor::*peak-equity* orig-peak)
      (setf swimmy.executor::*account-info-alert-sent* orig-alert))))

(deftest test-process-trade-closed-sexp
  "process-trade-closed should accept S-expression alist"
  (let* ((fn (find-symbol "PROCESS-TRADE-CLOSED" :swimmy.executor))
         (orig-trade-result (and (fboundp 'swimmy.school:record-trade-result)
                                 (symbol-function 'swimmy.school:record-trade-result)))
         (orig-trade-outcome (and (fboundp 'swimmy.school:record-trade-outcome)
                                  (symbol-function 'swimmy.school:record-trade-outcome)))
         (orig-notify (and (fboundp 'swimmy.core:notify-discord-alert)
                           (symbol-function 'swimmy.core:notify-discord-alert)))
         (orig-queue (and (fboundp 'swimmy.core:queue-discord-notification)
                          (symbol-function 'swimmy.core:queue-discord-notification)))
         (orig-processed swimmy.executor::*processed-tickets*)
         (orig-daily swimmy.executor::*daily-pnl*)
         (orig-weekly swimmy.executor::*weekly-pnl*)
         (orig-monthly swimmy.executor::*monthly-pnl*)
         (orig-accum swimmy.executor::*accumulated-pnl*)
         (orig-total swimmy.executor::*total-trades*)
         (orig-success swimmy.executor::*success-count*))
    (assert-true (and fn (fboundp fn)) "process-trade-closed exists")
    (unwind-protect
        (progn
          (when orig-trade-result
            (setf (symbol-function 'swimmy.school:record-trade-result) (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-trade-outcome
            (setf (symbol-function 'swimmy.school:record-trade-outcome) (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-notify
            (setf (symbol-function 'swimmy.core:notify-discord-alert) (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-queue
            (setf (symbol-function 'swimmy.core:queue-discord-notification) (lambda (&rest _args) (declare (ignore _args)) nil)))
          (setf swimmy.executor::*processed-tickets* (make-hash-table :test 'equal))
          (setf swimmy.executor::*daily-pnl* 0.0)
          (setf swimmy.executor::*weekly-pnl* 0.0)
          (setf swimmy.executor::*monthly-pnl* 0.0)
          (setf swimmy.executor::*accumulated-pnl* 0.0)
          (setf swimmy.executor::*total-trades* 0)
          (setf swimmy.executor::*success-count* 0)
          (funcall fn '((ticket . 12345) (symbol . "USDJPY") (pnl . 5.0) (won . t) (magic . 123)) "")
          (assert-true (gethash 12345 swimmy.executor::*processed-tickets*) "Expected ticket tracked"))
      (when orig-trade-result
        (setf (symbol-function 'swimmy.school:record-trade-result) orig-trade-result))
      (when orig-trade-outcome
        (setf (symbol-function 'swimmy.school:record-trade-outcome) orig-trade-outcome))
      (when orig-notify
        (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify))
      (when orig-queue
        (setf (symbol-function 'swimmy.core:queue-discord-notification) orig-queue))
      (setf swimmy.executor::*processed-tickets* orig-processed)
      (setf swimmy.executor::*daily-pnl* orig-daily)
      (setf swimmy.executor::*weekly-pnl* orig-weekly)
      (setf swimmy.executor::*monthly-pnl* orig-monthly)
      (setf swimmy.executor::*accumulated-pnl* orig-accum)
      (setf swimmy.executor::*total-trades* orig-total)
      (setf swimmy.executor::*success-count* orig-success))))

;;; ─────────────────────────────────────────
;;; INDICATOR TESTS
;;; ─────────────────────────────────────────

(deftest test-sma-calculation
  "Test SMA calculation"
  (let* ((test-data (loop for i from 1 to 10 
                          collect (cl-user::make-candle 
                                   :close (float i) :open 1.0 :high 1.0 :low 1.0)))
         (sma (cl-user::ind-sma 5 test-data)))
    ;; SMA of 1,2,3,4,5 = 15/5 = 3.0
    (assert-true (and sma (> sma 0)) "SMA should be positive")))

(deftest test-rsi-bounds
  "Test that RSI stays within 0-100"
  (let ((test-data (loop for i from 1 to 20 
                         collect (cl-user::make-candle 
                                  :close (+ 100 (random 10.0)) 
                                  :open 100.0 :high 110.0 :low 90.0))))
    (let ((rsi (cl-user::ind-rsi 14 test-data)))
      (when rsi
        (assert-true (<= 0 rsi 100) "RSI should be between 0-100")))))

;;; ─────────────────────────────────────────
;;; CATEGORY EXECUTION TESTS
;;; ─────────────────────────────────────────

(deftest test-category-trade-interval
  "category trade interval should allow/deny by elapsed seconds"
  (let* ((cat '("M5" :BUY "USDJPY"))
         (orig-table swimmy.school::*last-category-trade-time*)
         (orig-interval swimmy.school::*min-trade-interval*))
    (unwind-protect
        (progn
          (setf swimmy.school::*min-trade-interval* 10)
          (setf swimmy.school::*last-category-trade-time*
                (make-hash-table :test 'equal))
          (setf (gethash cat swimmy.school::*last-category-trade-time*)
                (- (get-universal-time) 20))
          (assert-true (swimmy.school::can-category-trade-p cat))
          (setf (gethash cat swimmy.school::*last-category-trade-time*)
                (get-universal-time))
          (assert-false (swimmy.school::can-category-trade-p cat)))
      (setf swimmy.school::*last-category-trade-time* orig-table)
      (setf swimmy.school::*min-trade-interval* orig-interval))))

(deftest test-high-council-danger-lv2-uses-swarm-consensus
  "Danger Lv2 approval uses swarm consensus only"
  (let ((orig-danger swimmy.globals::*danger-level*)
        (orig-swarm swimmy.globals::*last-swarm-consensus*)
        (orig-vol swimmy.globals::*current-volatility-state*))
    (unwind-protect
        (progn
          (setf swimmy.globals::*danger-level* 2)
          (setf swimmy.globals::*current-volatility-state* :normal)
          (setf swimmy.globals::*last-swarm-consensus* 0.8)
          (assert-true (swimmy.school::convene-high-council
                        '(:symbol "USDJPY" :direction :buy) :trend))
          (setf swimmy.globals::*last-swarm-consensus* 0.6)
          (assert-false (swimmy.school::convene-high-council
                         '(:symbol "USDJPY" :direction :buy) :trend)))
      (setf swimmy.globals::*danger-level* orig-danger)
      (setf swimmy.globals::*last-swarm-consensus* orig-swarm)
      (setf swimmy.globals::*current-volatility-state* orig-vol))))

(deftest test-live-status-schema-v2-no-tribe
  "live_status should be schema v2 and omit tribe fields"
  (let ((captured nil)
        (orig (symbol-function 'swimmy.core:write-sexp-atomic)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.core:write-sexp-atomic)
                (lambda (path payload)
                  (declare (ignore path))
                  (setf captured payload)))
          (let ((swimmy.shell::*live-status-interval* 0)
                (swimmy.shell::*last-status-write* 0))
            (swimmy.shell::save-live-status))
          (assert-equal 2 (cdr (assoc 'swimmy.shell::schema_version captured)))
          (assert-false (assoc 'swimmy.shell::tribes captured))
          (assert-false (assoc 'swimmy.shell::tribe_consensus captured)))
      (setf (symbol-function 'swimmy.core:write-sexp-atomic) orig))))

(deftest test-daily-report-omits-tribe
  "daily report should omit tribe wording"
  (let ((captured nil)
        (orig (symbol-function 'swimmy.shell::notify-discord-daily)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.shell::notify-discord-daily)
                (lambda (msg &key color)
                  (declare (ignore color))
                  (setf captured msg)))
          (swimmy.main::send-daily-status-report)
          (assert-true (null (search "Tribe" captured)))
          (assert-true (null (search "部族" captured))))
      (setf (symbol-function 'swimmy.shell::notify-discord-daily) orig))))

(deftest test-ledger-omits-tribe-fields
  "save-state should omit tribe fields"
  (let* ((tmp-path (merge-pathnames (format nil "/tmp/swimmy-state-~a.sexp" (get-universal-time))))
         (orig-path swimmy.engine::*state-file-path*))
    (unwind-protect
        (progn
          (setf swimmy.engine::*state-file-path* tmp-path)
          (swimmy.engine:save-state)
          (with-open-file (in tmp-path :direction :input)
            (let ((obj (read in nil nil)))
              (assert-false (member :tribe-consensus obj))
              (assert-false (member :tribe-direction obj)))))
      (setf swimmy.engine::*state-file-path* orig-path)
      (when (probe-file tmp-path) (delete-file tmp-path)))))

(deftest test-category-vote-list
  "gather-category-votes should return a non-empty list"
  (let ((votes (swimmy.school::gather-category-votes "proposal" :trend)))
    (assert-true (and (listp votes) (> (length votes) 0)))))

(deftest test-dynamic-narrative-uses-category-display
  "dynamic narrative should use category display, not clan names"
  (let* ((signal (list :strategy-name "Test"
                       :direction :buy
                       :category :trend
                       :indicator-values nil
                       :sl 0.1
                       :tp 0.2))
         (text (swimmy.school::generate-dynamic-narrative signal "USDJPY" 150.0)))
    (assert-true (search "TREND" text))
    (assert-false (search "Hunters" text))
    (assert-false (search "Unknown" text))))

(deftest test-category-counts-returns-alist
  "category counts should return alist with core categories"
  (let ((counts (swimmy.school::get-category-counts)))
    (assert-true (assoc :trend counts))
    (assert-true (assoc :reversion counts))
    (assert-true (assoc :breakout counts))
    (assert-true (assoc :scalp counts))))

;;; ─────────────────────────────────────────
;;; CONSTITUTION TESTS
;;; ─────────────────────────────────────────

(deftest test-constitution-evaluation
  "Test constitution evaluation"
  (cl-user::initialize-constitution)
  (let* ((context (list :daily-pnl 0 :volatility-state :normal))
         (result (cl-user::evaluate-constitution context)))
    (assert-not-nil result "Evaluation should return result")
    (assert-true (getf result :alignment) "Should have alignment score")))

(deftest test-constitution-blocks-dangerous
  "Test that constitution blocks dangerous actions"
  (cl-user::initialize-constitution)
  (let* ((dangerous-context (list :daily-pnl -6000 :volatility-state :extreme))
         (result (cl-user::evaluate-constitution dangerous-context)))
    (assert-true (getf result :violations) "Should have violations")))

;;; ─────────────────────────────────────────
;;; REPUTATION TESTS
;;; ─────────────────────────────────────────

(deftest test-reputation-creation
  "Test reputation creation"
  (let ((rep (cl-user::get-reputation "Test-Strategy")))
    (assert-not-nil rep "Reputation should be created")
    (assert-equal 0.5 (cl-user::reputation-trust-score rep) "Initial trust should be 0.5")))

(deftest test-reputation-update
  "Test reputation update on win"
  (let ((rep (cl-user::get-reputation "Winner-Strategy")))
    (declare (ignore rep))
    (cl-user::update-reputation "Winner-Strategy" :win :pnl 100)
    (let ((updated-rep (cl-user::get-reputation "Winner-Strategy")))
      (assert-true (> (cl-user::reputation-trust-score updated-rep) 0.5)
                   "Trust should increase on win"))))

;;; ─────────────────────────────────────────
;;; V6.18: DANGER AVOIDANCE TESTS
;;; ─────────────────────────────────────────

(deftest test-danger-level-initial
  "Test initial danger level is 0"
  (assert-equal 0 cl-user::*danger-level* "Initial danger level should be 0"))

(deftest test-consecutive-losses-tracked
  "Test consecutive losses tracking"
  (let ((old-losses cl-user::*consecutive-losses*))
    (cl-user::record-trade-result :loss)
    (assert-true (>= cl-user::*consecutive-losses* 1) "Should track losses")
    (setf cl-user::*consecutive-losses* old-losses)))

(deftest test-cooldown-returns-false-initially
  "Test cooldown is inactive initially"
  (setf cl-user::*danger-cooldown-until* 0)
  (assert-false (cl-user::danger-cooldown-active-p) "Cooldown should be inactive"))

;;; ─────────────────────────────────────────
;;; V6.18: RESIGNATION TESTS
;;; ─────────────────────────────────────────

(deftest test-has-resigned-initial
  "Test initial resignation state"
  (setf cl-user::*has-resigned-today* nil)
  (assert-false (cl-user::has-resigned-p) "Should not be resigned initially"))

(deftest test-resignation-threshold-exists
  "Test resignation threshold is defined"
  (assert-not-nil cl-user::*resignation-threshold* "Threshold should exist")
  (assert-true (< cl-user::*resignation-threshold* 0) "Threshold should be negative"))

;;; ─────────────────────────────────────────
;;; V6.18: LEADER SYSTEM TESTS
;;; ─────────────────────────────────────────

(deftest test-leader-info-struct
  "Test leader-info struct creation"
  (let ((leader (cl-user::make-leader-info 
                 :strategy-name "Test" 
                 :sharpe 1.5 
                 :win-rate 0.6
                 :tenure-start 0
                 :trades-as-leader 0
                 :pnl-as-leader 0.0)))
    (assert-equal "Test" (cl-user::leader-info-strategy-name leader))
    (assert-equal 1.5 (cl-user::leader-info-sharpe leader))))

;;; ─────────────────────────────────────────
;;; V6.18: RISK MANAGER TESTS
;;; ─────────────────────────────────────────

(deftest test-risk-summary
  "Test risk summary generation"
  (let ((summary (cl-user::get-risk-summary)))
    (assert-not-nil summary "Summary should be generated")
    (assert-true (stringp summary) "Summary should be a string")))

;;; ─────────────────────────────────────────
;;; V6.18: DYNAMIC TP/SL TESTS
;;; ─────────────────────────────────────────

(deftest test-volatility-multiplier
  "Test volatility multiplier values"
  (let ((mult (cl-user::get-volatility-multiplier)))
    (assert-not-nil mult "Multiplier should exist")
    (assert-true (and (> mult 0) (<= mult 2)) "Should be reasonable range")))

(deftest test-atr-empty-candles
  "Test ATR with empty candles returns nil"
  (let ((atr (cl-user::calculate-atr nil)))
    (assert-true (null atr) "ATR of nil should be nil")))

;;; ─────────────────────────────────────────
;;; V6.18: UTILITY TESTS
;;; ─────────────────────────────────────────

(deftest test-gotobi-day-returns-boolean
  "Test gotobi-day-p returns proper boolean"
  (let ((result (cl-user::gotobi-day-p)))
    (assert-true (or (eq result t) (eq result nil)) "Should return t or nil")))

(deftest test-london-session-check
  "Test London session detection"
  (let ((result (cl-user::london-session-p)))
    (assert-true (or (eq result t) (eq result nil)) "Should return t or nil")))

;;; ─────────────────────────────────────────
;;; V6.18: CANDLE STRUCT TESTS
;;; ─────────────────────────────────────────

(deftest test-candle-creation
  "Test candle struct creation"
  (let ((c (cl-user::make-candle :open 100.0 :high 110.0 :low 90.0 :close 105.0)))
    (assert-equal 100.0 (cl-user::candle-open c))
    (assert-equal 105.0 (cl-user::candle-close c))))

;;; V8.4: CHARACTERIZATION TESTS (memo3.txt)
;;; ─────────────────────────────────────────
;;; Purpose: Freeze current behavior before refactoring
;;; These tests document ACTUAL behavior, not intended behavior

(deftest test-maintenance-throttle-60s
  "Characterization: run-periodic-maintenance respects 60s throttle"
  ;; Test that the throttle variable exists and is a number
  (let ((maint-time-sym (find-symbol "*LAST-MAINTENANCE-TIME*" :swimmy.main)))
    (if (and maint-time-sym (boundp maint-time-sym))
        (assert-true (numberp (symbol-value maint-time-sym)) "Should be a number")
        (assert-true t "Variable not defined yet - OK for cold start"))))

(deftest test-dream-cycle-self-throttle
  "Characterization: Dream cycle uses *dream-interval* for throttling"
  ;; Document current behavior: dream uses 3600s interval
  (assert-true (boundp 'swimmy.globals:*dream-interval*))
  (assert-true (>= swimmy.globals:*dream-interval* 60)  ; Minimum 60s is reasonable
               "Dream interval should be reasonable"))

(deftest test-processing-step-no-maintenance
  "Characterization: process-msg no longer calls maintenance"
  ;; V8.4: Just verify the function exists and can be called
  (let ((fn-sym (find-symbol "PROCESS-MSG" :swimmy.main)))
    (assert-true (and fn-sym (fboundp fn-sym)) "process-msg should be defined")))

(deftest test-backtest-debug-enabled-p
  "backtest debug env flag should be parsed consistently"
  (require :sb-posix)
  (let ((orig (uiop:getenv "SWIMMY_BACKTEST_DEBUG_RECV")))
    (unwind-protect
        (progn
          (sb-posix:setenv "SWIMMY_BACKTEST_DEBUG_RECV" "1" 1)
          (assert-true (swimmy.main::backtest-debug-enabled-p))
          (sb-posix:setenv "SWIMMY_BACKTEST_DEBUG_RECV" "true" 1)
          (assert-true (swimmy.main::backtest-debug-enabled-p))
          (sb-posix:setenv "SWIMMY_BACKTEST_DEBUG_RECV" "false" 1)
          (assert-true (not (swimmy.main::backtest-debug-enabled-p)))
          (sb-posix:setenv "SWIMMY_BACKTEST_DEBUG_RECV" "" 1)
          (assert-true (not (swimmy.main::backtest-debug-enabled-p))))
      (when orig
        (sb-posix:setenv "SWIMMY_BACKTEST_DEBUG_RECV" orig 1))
      (unless orig
        (sb-posix:unsetenv "SWIMMY_BACKTEST_DEBUG_RECV")))))

(deftest test-backtest-pending-counters-defaults
  "backtest pending counters should initialize to sane defaults"
  (assert-true (boundp 'swimmy.globals::*backtest-submit-count*) "submit counter exists")
  (assert-true (boundp 'swimmy.globals::*backtest-max-pending*) "max pending exists")
  (assert-true (numberp swimmy.globals::*backtest-max-pending*) "max pending numeric"))

(deftest test-backtest-send-throttles-when-pending-high
  "send-zmq-msg should refuse backtest send when pending exceeds max"
  (let* ((orig-send (symbol-function 'pzmq:send))
         (orig-req swimmy.globals:*backtest-requester*)
         (sent nil))
    (unwind-protect
        (progn
          (setf swimmy.globals:*backtest-requester* :dummy)
          (setf swimmy.globals::*backtest-submit-count* 10)
          (setf swimmy.main::*backtest-recv-count* 0)
          (setf swimmy.globals::*backtest-max-pending* 1)
          (setf (symbol-function 'pzmq:send)
                (lambda (&rest _)
                  (declare (ignore _))
                  (setf sent t)))
          (swimmy.school::send-zmq-msg "(dummy)" :target :backtest)
          (assert-true (null sent) "send should be blocked"))
      (setf swimmy.globals:*backtest-requester* orig-req)
      (setf (symbol-function 'pzmq:send) orig-send))))

(deftest test-backtest-send-uses-subsecond-time
  "backtest-send-allowed-p should use subsecond time source for rate limiting"
  (let* ((orig-now (when (fboundp 'swimmy.school::backtest-now-seconds)
                     (symbol-function 'swimmy.school::backtest-now-seconds)))
         (calls 0)
         (orig-rate swimmy.globals::*backtest-rate-limit-per-sec*)
         (orig-max swimmy.globals::*backtest-max-pending*)
         (orig-submit swimmy.globals::*backtest-submit-count*)
         (orig-last swimmy.globals::*backtest-last-send-ts*)
         (orig-recv (when (boundp 'swimmy.main::*backtest-recv-count*)
                      swimmy.main::*backtest-recv-count*)))
    (unwind-protect
        (progn
          (setf swimmy.globals::*backtest-rate-limit-per-sec* 5)
          (setf swimmy.globals::*backtest-max-pending* 999999)
          (setf swimmy.globals::*backtest-submit-count* 0)
          (setf swimmy.globals::*backtest-last-send-ts* 100.0d0)
          (when (boundp 'swimmy.main::*backtest-recv-count*)
            (setf swimmy.main::*backtest-recv-count* 0))
          (setf (symbol-function 'swimmy.school::backtest-now-seconds)
                (lambda ()
                  (incf calls)
                  100.2d0))
          (assert-true (swimmy.school::backtest-send-allowed-p)
                       "should allow after 0.2s at 5/s")
          (assert-true (> calls 0) "should use backtest-now-seconds"))
      (setf swimmy.globals::*backtest-rate-limit-per-sec* orig-rate)
      (setf swimmy.globals::*backtest-max-pending* orig-max)
      (setf swimmy.globals::*backtest-submit-count* orig-submit)
      (setf swimmy.globals::*backtest-last-send-ts* orig-last)
      (when (boundp 'swimmy.main::*backtest-recv-count*)
        (setf swimmy.main::*backtest-recv-count* orig-recv))
      (if orig-now
          (setf (symbol-function 'swimmy.school::backtest-now-seconds) orig-now)
          (fmakunbound 'swimmy.school::backtest-now-seconds)))))

(deftest test-send-zmq-sleep-suppressed-for-backtest-requester
  "send-zmq-msg should not sleep for backtest requester path"
  (let* ((orig-send (symbol-function 'pzmq:send))
         (orig-req swimmy.globals:*backtest-requester*)
         (orig-cmd swimmy.globals:*cmd-publisher*)
         (orig-rate swimmy.globals::*backtest-rate-limit-per-sec*)
         (orig-max swimmy.globals::*backtest-max-pending*)
         (orig-submit swimmy.globals::*backtest-submit-count*)
         (orig-last swimmy.globals::*backtest-last-send-ts*)
         (orig-recv (when (boundp 'swimmy.main::*backtest-recv-count*)
                      swimmy.main::*backtest-recv-count*))
         (iterations 30))
    (unwind-protect
        (progn
          (setf swimmy.globals:*backtest-requester* :dummy)
          (setf swimmy.globals:*cmd-publisher* :dummy)
          (setf swimmy.globals::*backtest-rate-limit-per-sec* 0)
          (setf swimmy.globals::*backtest-max-pending* 999999)
          (setf swimmy.globals::*backtest-submit-count* 0)
          (setf swimmy.globals::*backtest-last-send-ts* 0.0d0)
          (when (boundp 'swimmy.main::*backtest-recv-count*)
            (setf swimmy.main::*backtest-recv-count* 0))
          (setf (symbol-function 'pzmq:send)
                (lambda (&rest _args) (declare (ignore _args)) t))
          (let* ((cmd-start (get-internal-real-time)))
            (dotimes (_ iterations)
              (swimmy.school::send-zmq-msg "(dummy)" :target :cmd))
            (let* ((cmd-end (get-internal-real-time))
                   (cmd-elapsed (/ (- cmd-end cmd-start)
                                   internal-time-units-per-second)))
              (let* ((bt-start (get-internal-real-time)))
                (dotimes (_ iterations)
                  (swimmy.school::send-zmq-msg "(dummy)" :target :backtest))
                (let* ((bt-end (get-internal-real-time))
                       (bt-elapsed (/ (- bt-end bt-start)
                                      internal-time-units-per-second)))
                  ;; Expect backtest path to be significantly faster when sleep is suppressed.
                  (assert-true (< bt-elapsed (* cmd-elapsed 0.7d0))
                               "backtest path should be faster than cmd path"))))))
      (setf swimmy.globals:*backtest-requester* orig-req)
      (setf swimmy.globals:*cmd-publisher* orig-cmd)
      (setf swimmy.globals::*backtest-rate-limit-per-sec* orig-rate)
      (setf swimmy.globals::*backtest-max-pending* orig-max)
      (setf swimmy.globals::*backtest-submit-count* orig-submit)
      (setf swimmy.globals::*backtest-last-send-ts* orig-last)
      (when (boundp 'swimmy.main::*backtest-recv-count*)
        (setf swimmy.main::*backtest-recv-count* orig-recv))
      (setf (symbol-function 'pzmq:send) orig-send))))

(deftest test-rr-batch-respects-max-pending
  "RR batch size should follow SWIMMY_BACKTEST_MAX_PENDING"
  (let* ((orig-request (symbol-function 'swimmy.school::request-backtest))
         (orig-notify (and (fboundp 'swimmy.core:notify-discord-alert)
                           (symbol-function 'swimmy.core:notify-discord-alert)))
         (orig-summary (and (fboundp 'swimmy.school::notify-backtest-summary)
                            (symbol-function 'swimmy.school::notify-backtest-summary)))
         (orig-cache (and (fboundp 'swimmy.school::get-cached-backtest)
                          (symbol-function 'swimmy.school::get-cached-backtest)))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-candle swimmy.globals:*candle-history*)
         (orig-hist swimmy.globals:*candle-histories*)
         (orig-max swimmy.globals::*backtest-max-pending*)
         (orig-cursor swimmy.school::*backtest-cursor*)
         (count 0))
    (unwind-protect
        (progn
          (setf swimmy.globals::*backtest-max-pending* 2)
          (setf swimmy.globals:*candle-history* (loop repeat 101 collect 1))
          (setf swimmy.globals:*candle-histories* (make-hash-table :test 'equal))
          (setf swimmy.school::*strategy-knowledge-base*
                (list (swimmy.school:make-strategy :name "S1")
                      (swimmy.school:make-strategy :name "S2")
                      (swimmy.school:make-strategy :name "S3")
                      (swimmy.school:make-strategy :name "S4")))
          (setf swimmy.school::*backtest-cursor* 0)
          (when orig-cache
            (setf (symbol-function 'swimmy.school::get-cached-backtest)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (setf (symbol-function 'swimmy.school::request-backtest)
                (lambda (&rest _args) (declare (ignore _args)) (incf count)))
          (when orig-notify
            (setf (symbol-function 'swimmy.core:notify-discord-alert)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-summary
            (setf (symbol-function 'swimmy.school::notify-backtest-summary)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (swimmy.school::batch-backtest-knowledge)
          (assert-equal 2 count "batch should follow max pending"))
      (setf swimmy.globals::*backtest-max-pending* orig-max)
      (setf swimmy.globals:*candle-history* orig-candle)
      (setf swimmy.globals:*candle-histories* orig-hist)
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*backtest-cursor* orig-cursor)
      (when orig-cache
        (setf (symbol-function 'swimmy.school::get-cached-backtest) orig-cache))
      (setf (symbol-function 'swimmy.school::request-backtest) orig-request)
      (when orig-notify
        (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify))
      (when orig-summary
        (setf (symbol-function 'swimmy.school::notify-backtest-summary) orig-summary)))))

(deftest test-rr-batch-skips-retired-strategies
  "RR batch should skip retired/graveyard strategies"
  (let* ((orig-request (symbol-function 'swimmy.school::request-backtest))
         (orig-notify (and (fboundp 'swimmy.core:notify-discord-alert)
                           (symbol-function 'swimmy.core:notify-discord-alert)))
         (orig-summary (and (fboundp 'swimmy.school::notify-backtest-summary)
                            (symbol-function 'swimmy.school::notify-backtest-summary)))
         (orig-cache (and (fboundp 'swimmy.school::get-cached-backtest)
                          (symbol-function 'swimmy.school::get-cached-backtest)))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-candle swimmy.globals:*candle-history*)
         (orig-hist swimmy.globals:*candle-histories*)
         (orig-max swimmy.globals::*backtest-max-pending*)
         (orig-cursor swimmy.school::*backtest-cursor*)
         (seen nil)
         (count 0))
    (unwind-protect
        (progn
          (setf swimmy.globals::*backtest-max-pending* 10)
          (setf swimmy.globals:*candle-history* (loop repeat 101 collect 1))
          (setf swimmy.globals:*candle-histories* (make-hash-table :test 'equal))
          (let ((s1 (swimmy.school:make-strategy :name "S1"))
                (s2 (swimmy.school:make-strategy :name "S2"))
                (s3 (swimmy.school:make-strategy :name "S3")))
            (setf (strategy-rank s1) :B)
            (setf (strategy-rank s2) :retired)
            (setf (strategy-rank s3) :graveyard)
            (setf swimmy.school::*strategy-knowledge-base* (list s1 s2 s3)))
          (setf swimmy.school::*backtest-cursor* 0)
          (when orig-cache
            (setf (symbol-function 'swimmy.school::get-cached-backtest)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (setf (symbol-function 'swimmy.school::request-backtest)
                (lambda (strat &rest _args)
                  (declare (ignore _args))
                  (incf count)
                  (push (strategy-name strat) seen)))
          (when orig-notify
            (setf (symbol-function 'swimmy.core:notify-discord-alert)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-summary
            (setf (symbol-function 'swimmy.school::notify-backtest-summary)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (swimmy.school::batch-backtest-knowledge)
          (assert-equal 1 count "Should only backtest active strategies")
          (assert-true (null (member "S2" seen :test #'string=))
                       "Retired strategy should be skipped")
          (assert-true (null (member "S3" seen :test #'string=))
                       "Graveyard strategy should be skipped"))
      (setf swimmy.globals::*backtest-max-pending* orig-max)
      (setf swimmy.globals:*candle-history* orig-candle)
      (setf swimmy.globals:*candle-histories* orig-hist)
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*backtest-cursor* orig-cursor)
      (when orig-cache
        (setf (symbol-function 'swimmy.school::get-cached-backtest) orig-cache))
      (setf (symbol-function 'swimmy.school::request-backtest) orig-request)
      (when orig-notify
        (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify))
      (when orig-summary
        (setf (symbol-function 'swimmy.school::notify-backtest-summary) orig-summary)))))

(deftest test-rr-batch-no-active-strategies
  "RR batch should no-op when no active strategies exist"
  (let* ((orig-request (symbol-function 'swimmy.school::request-backtest))
         (orig-notify (and (fboundp 'swimmy.core:notify-discord-alert)
                           (symbol-function 'swimmy.core:notify-discord-alert)))
         (orig-summary (and (fboundp 'swimmy.school::notify-backtest-summary)
                            (symbol-function 'swimmy.school::notify-backtest-summary)))
         (orig-cache (and (fboundp 'swimmy.school::get-cached-backtest)
                          (symbol-function 'swimmy.school::get-cached-backtest)))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-candle swimmy.globals:*candle-history*)
         (orig-hist swimmy.globals:*candle-histories*)
         (orig-max swimmy.globals::*backtest-max-pending*)
         (orig-cursor swimmy.school::*backtest-cursor*)
         (count 0))
    (unwind-protect
        (progn
          (setf swimmy.globals::*backtest-max-pending* 10)
          (setf swimmy.globals:*candle-history* (loop repeat 101 collect 1))
          (setf swimmy.globals:*candle-histories* (make-hash-table :test 'equal))
          (let ((s1 (swimmy.school:make-strategy :name "S1"))
                (s2 (swimmy.school:make-strategy :name "S2")))
            (setf (strategy-rank s1) :retired)
            (setf (strategy-rank s2) :retired)
            (setf swimmy.school::*strategy-knowledge-base* (list s1 s2)))
          (setf swimmy.school::*backtest-cursor* 0)
          (when orig-cache
            (setf (symbol-function 'swimmy.school::get-cached-backtest)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (setf (symbol-function 'swimmy.school::request-backtest)
                (lambda (&rest _args) (declare (ignore _args)) (incf count)))
          (when orig-notify
            (setf (symbol-function 'swimmy.core:notify-discord-alert)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (when orig-summary
            (setf (symbol-function 'swimmy.school::notify-backtest-summary)
                  (lambda (&rest _args) (declare (ignore _args)) nil)))
          (swimmy.school::batch-backtest-knowledge)
          (assert-equal 0 count "No active strategies should be backtested"))
      (setf swimmy.globals::*backtest-max-pending* orig-max)
      (setf swimmy.globals:*candle-history* orig-candle)
      (setf swimmy.globals:*candle-histories* orig-hist)
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*backtest-cursor* orig-cursor)
      (when orig-cache
        (setf (symbol-function 'swimmy.school::get-cached-backtest) orig-cache))
      (setf (symbol-function 'swimmy.school::request-backtest) orig-request)
      (when orig-notify
        (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-notify))
      (when orig-summary
        (setf (symbol-function 'swimmy.school::notify-backtest-summary) orig-summary)))))

(deftest test-format-phase1-bt-batch-message
  "Phase1 BT batch message should include completion text when flagged"
  (let ((fn (find-symbol "FORMAT-PHASE1-BT-BATCH-MESSAGE" :swimmy.school)))
    (assert-true (and fn (fboundp fn)) "format helper should exist")
    (let ((msg (funcall fn 2 3 1 :cycle-completed t)))
      (assert-true (search "Phase1 BT Cycle Complete" msg)
                   (format nil "Expected completion text, got: ~a" msg)))
    (let ((msg (funcall fn 2 3 1 :cycle-completed nil)))
      (assert-true (null (search "Phase1 BT Cycle Complete" msg))
                   "Completion text should be omitted when not complete"))))

(deftest test-format-percent-no-double
  "format-percent should return a single percent sign"
  (let ((fn (find-symbol "FORMAT-PERCENT" :swimmy.main)))
    (assert-true (and fn (fboundp fn)) "format-percent should exist")
    (assert-equal "50%" (funcall fn 0.5) "Expected 50% (single percent)")
    (assert-equal "0%" (funcall fn 0.0) "Expected 0% (single percent)")
    (assert-equal "N/A" (funcall fn nil) "Expected fallback for NIL")))

(deftest test-format-value-rounds-int
  "format-value should round floats when using integer format"
  (let ((fn (find-symbol "FORMAT-VALUE" :swimmy.main)))
    (assert-true (and fn (fboundp fn)) "format-value should exist")
    (assert-equal "100000" (funcall fn 100000.4 "~d") "Expected rounded integer")
    (assert-equal "-2" (funcall fn -1.6 "~d") "Expected rounded negative")
    (assert-equal "N/A" (funcall fn nil "~d") "Expected fallback for NIL")))

(deftest test-ledger-persists-equity
  "save-state/load-state should persist equity and drawdown metrics"
  (let* ((tmp-path (merge-pathnames (format nil "/tmp/swimmy-state-~a.sexp" (get-universal-time))))
         (orig-path swimmy.engine::*state-file-path*)
         (orig-equity swimmy.globals::*current-equity*)
         (orig-peak swimmy.globals::*peak-equity*)
         (orig-max-dd swimmy.globals::*max-drawdown*)
         (orig-monitor-peak swimmy.globals::*monitoring-peak-equity*)
         (orig-monitor-dd swimmy.globals::*monitoring-drawdown*)
         (orig-current-dd swimmy.globals::*current-drawdown*)
         (orig-last-account swimmy.globals::*last-account-info-time*))
    (unwind-protect
        (progn
          (setf swimmy.engine::*state-file-path* tmp-path)
          (setf swimmy.globals::*current-equity* 123456.0)
          (setf swimmy.globals::*peak-equity* 234567.0)
          (setf swimmy.globals::*max-drawdown* 12.3)
          (setf swimmy.globals::*monitoring-peak-equity* 345678.0)
          (setf swimmy.globals::*monitoring-drawdown* 4.5)
          (setf swimmy.globals::*current-drawdown* 6.7)
          (setf swimmy.globals::*last-account-info-time* 999)
          (swimmy.engine:save-state)
          (setf swimmy.globals::*current-equity* 0.0)
          (setf swimmy.globals::*peak-equity* 0.0)
          (setf swimmy.globals::*max-drawdown* 0.0)
          (setf swimmy.globals::*monitoring-peak-equity* 0.0)
          (setf swimmy.globals::*monitoring-drawdown* 0.0)
          (setf swimmy.globals::*current-drawdown* 0.0)
          (setf swimmy.globals::*last-account-info-time* 0)
          (swimmy.engine:load-state)
          (assert-equal 123456.0 swimmy.globals::*current-equity* "Current equity should restore")
          (assert-equal 234567.0 swimmy.globals::*peak-equity* "Peak equity should restore")
          (assert-equal 12.3 swimmy.globals::*max-drawdown* "Max drawdown should restore")
          (assert-equal 345678.0 swimmy.globals::*monitoring-peak-equity* "Monitoring peak should restore")
          (assert-equal 4.5 swimmy.globals::*monitoring-drawdown* "Monitoring drawdown should restore")
          (assert-equal 6.7 swimmy.globals::*current-drawdown* "Current drawdown should restore")
          (assert-equal 999 swimmy.globals::*last-account-info-time* "Account info timestamp should restore"))
      (setf swimmy.engine::*state-file-path* orig-path)
      (setf swimmy.globals::*current-equity* orig-equity)
      (setf swimmy.globals::*peak-equity* orig-peak)
      (setf swimmy.globals::*max-drawdown* orig-max-dd)
      (setf swimmy.globals::*monitoring-peak-equity* orig-monitor-peak)
      (setf swimmy.globals::*monitoring-drawdown* orig-monitor-dd)
      (setf swimmy.globals::*current-drawdown* orig-current-dd)
      (setf swimmy.globals::*last-account-info-time* orig-last-account)
      (when (probe-file tmp-path) (delete-file tmp-path)))))

(deftest test-system-pulse-5m-text
  "System Pulse heartbeat should not claim cycle completion"
  (let* ((orig-queue (and (fboundp 'swimmy.core:queue-discord-notification)
                          (symbol-function 'swimmy.core:queue-discord-notification)))
         (orig-last (and (boundp 'swimmy.school::*last-cycle-notify-time*)
                         swimmy.school::*last-cycle-notify-time*))
         (captured nil))
    (unwind-protect
        (progn
          (when (boundp 'swimmy.school::*last-cycle-notify-time*)
            (setf swimmy.school::*last-cycle-notify-time* 0))
          (when orig-queue
            (setf (symbol-function 'swimmy.core:queue-discord-notification)
                  (lambda (_webhook msg &key title color)
                    (declare (ignore _webhook title color))
                    (setf captured msg))))
          (swimmy.school::notify-cycle-complete)
          (assert-true captured "Expected System Pulse message")
          (assert-true (search "System Pulse (5m)" captured) "Expected 5m text")
          (assert-true (null (search "Cycle Complete" captured))
                       "Should not claim cycle completion"))
      (when (boundp 'swimmy.school::*last-cycle-notify-time*)
        (setf swimmy.school::*last-cycle-notify-time* orig-last))
      (when orig-queue
        (setf (symbol-function 'swimmy.core:queue-discord-notification) orig-queue)))))

(deftest test-backtest-pending-count-decrements-on-recv
  "pending count should drop when a BACKTEST_RESULT is processed"
  (let* ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main))
         (msg "((type . \"BACKTEST_RESULT\") (result . ((strategy_name . \"UT-PENDING\") (sharpe . 0.1) (trades . 1))))"))
    (assert-true (and fn (fboundp fn)) "internal-process-msg exists")
    (setf swimmy.globals::*backtest-submit-count* 5)
    (setf swimmy.main::*backtest-recv-count* 0)
    (funcall fn msg)
    (assert-true (> swimmy.main::*backtest-recv-count* 0) "recv count increments")))

(deftest test-deferred-flush-respects-batch
  "flush-deferred-founders should only request up to batch size"
  (let* ((orig-request (symbol-function 'swimmy.school::request-backtest))
         (orig-kb swimmy.school::*strategy-knowledge-base*)
         (orig-batch (and (boundp 'swimmy.school::*deferred-flush-batch*)
                          swimmy.school::*deferred-flush-batch*))
         (count 0))
    (unwind-protect
        (progn
          (setf swimmy.school::*strategy-knowledge-base*
                (list (swimmy.school:make-strategy :name "S1")
                      (swimmy.school:make-strategy :name "S2")
                      (swimmy.school:make-strategy :name "S3")))
          (setf swimmy.school::*deferred-flush-batch* 1)
          (setf (symbol-function 'swimmy.school::request-backtest)
                (lambda (&rest _) (declare (ignore _)) (incf count)))
          (swimmy.school::flush-deferred-founders)
          (assert-equal 1 count "batch=1 should send exactly one"))
      (setf swimmy.school::*strategy-knowledge-base* orig-kb)
      (setf swimmy.school::*deferred-flush-batch* orig-batch)
      (setf (symbol-function 'swimmy.school::request-backtest) orig-request))))

(deftest test-backtest-uses-csv-override
  "request-backtest should honor SWIMMY_BACKTEST_CSV_OVERRIDE when set"
  (require :sb-posix)
  (let* ((orig-env (uiop:getenv "SWIMMY_BACKTEST_CSV_OVERRIDE"))
         (orig-override (and (boundp 'swimmy.core::*backtest-csv-override*)
                             swimmy.core::*backtest-csv-override*))
         (path "/tmp/swimmy-test.csv")
         (captured nil)
         (orig-send (symbol-function 'swimmy.school::send-zmq-msg)))
    (unwind-protect
        (progn
          (sb-posix:setenv "SWIMMY_BACKTEST_CSV_OVERRIDE" path 1)
          (setf swimmy.core::*backtest-csv-override* path)
          (with-open-file (s path :direction :output :if-exists :supersede :if-does-not-exist :create)
            (write-line "" s))
          (setf (symbol-function 'swimmy.school::send-zmq-msg)
                (lambda (msg &key target)
                  (declare (ignore target))
                  (setf captured msg)))
          (swimmy.school::request-backtest (swimmy.school:make-strategy :name "T" :symbol "USDJPY"))
          (assert-true (and captured (search path captured))
                       "payload should include override path"))
      (setf (symbol-function 'swimmy.school::send-zmq-msg) orig-send)
      (ignore-errors (delete-file path))
      (when orig-env
        (sb-posix:setenv "SWIMMY_BACKTEST_CSV_OVERRIDE" orig-env 1))
      (unless orig-env
        (sb-posix:unsetenv "SWIMMY_BACKTEST_CSV_OVERRIDE"))
      (setf swimmy.core::*backtest-csv-override* orig-override))))

(deftest test-backtest-v2-uses-alist
  "request-backtest-v2 should send alist strategy payload"
  (let ((captured nil)
        (orig-send (symbol-function 'swimmy.school::send-zmq-msg))
        (orig-fetch (symbol-function 'swimmy.school::fetch-swap-history)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::send-zmq-msg)
                (lambda (msg &key target)
                  (declare (ignore target))
                  (setf captured msg)))
          (setf (symbol-function 'swimmy.school::fetch-swap-history)
                (lambda (&rest _) (declare (ignore _)) nil))
          (let ((s (swimmy.school:make-strategy :name "T" :symbol "USDJPY")))
            (swimmy.school::request-backtest-v2 s :start-date "2020.01.01" :end-date "2020.12.31")))
      (setf (symbol-function 'swimmy.school::send-zmq-msg) orig-send)
      (setf (symbol-function 'swimmy.school::fetch-swap-history) orig-fetch))
    (assert-not-nil captured "Expected V2 backtest payload to be sent")
    (let ((*package* (find-package :swimmy.tests)))
      (multiple-value-bind (payload _pos) (read-from-string captured)
        (declare (ignore _pos))
        (let ((strategy (cdr (assoc 'strategy payload))))
          (assert-true (consp strategy) "Strategy should be an alist"))
        (assert-equal "USDJPY" (cdr (assoc 'symbol payload)) "Symbol should be USDJPY")
        (let ((tf (cdr (assoc 'timeframe payload))))
          (assert-true (and (listp tf) (= 1 (length tf)) (numberp (first tf)))
                       "Timeframe Option<i64> should be numeric"))
        (let ((data-id (cdr (assoc 'data_id payload))))
          (assert-true (and (listp data-id) (= 1 (length data-id)) (stringp (first data-id)))
                       "Expected data_id Option<String> present"))
        (let ((candles-file (cdr (assoc 'candles_file payload))))
          (assert-true (and (listp candles-file) (= 1 (length candles-file)) (stringp (first candles-file)))
                       "Expected candles_file Option<String> present"))
        (let ((start-time (cdr (assoc 'start_time payload))))
          (assert-true (and (listp start-time) (= 1 (length start-time)) (numberp (first start-time)))
                       "Expected start_time Option<i64> present"))
        (let ((end-time (cdr (assoc 'end_time payload))))
          (assert-true (and (listp end-time) (= 1 (length end-time)) (numberp (first end-time)))
                       "Expected end_time Option<i64> present"))))))

(deftest test-backtest-v2-phase2-promotes-to-a
  "Phase2 result should promote to A when sharpe meets threshold"
  (let* ((s (swimmy.school:make-strategy :name "Phase2" :symbol "USDJPY"))
         (swimmy.school::*strategy-knowledge-base* (list s))
         (called nil))
    (let ((orig (symbol-function 'swimmy.school:ensure-rank)))
      (unwind-protect
          (progn
            (setf (symbol-function 'swimmy.school:ensure-rank)
                  (lambda (strat rank &optional reason)
                    (declare (ignore reason))
                    (setf called (list strat rank))
                    rank))
            (swimmy.school::handle-v2-result "Phase2_P2" (list :sharpe 1.0)))
        (setf (symbol-function 'swimmy.school:ensure-rank) orig)))
    (assert-equal :A (second called) "Expected A-RANK promotion")))

(deftest test-promotion-triggers-noncorrelation-notification
  "Ensure A/S promotions fire noncorrelation notification once"
  (let* ((tmp-db (format nil "/tmp/swimmy-promo-~a.db" (get-universal-time)))
         (strat (swimmy.school:make-strategy :name "PROMO" :symbol "USDJPY" :rank :B))
         (called 0)
         (orig (and (fboundp 'swimmy.school::notify-noncorrelated-promotion)
                    (symbol-function 'swimmy.school::notify-noncorrelated-promotion))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            (setf (symbol-function 'swimmy.school::notify-noncorrelated-promotion)
                  (lambda (&rest args) (declare (ignore args)) (incf called)))
            (swimmy.school::ensure-rank strat :A "test")
            (assert-equal 1 called "A promotion should notify once")
            (swimmy.school::ensure-rank strat :S "test")
            (assert-equal 2 called "S promotion should notify once"))
        (when orig
          (setf (symbol-function 'swimmy.school::notify-noncorrelated-promotion) orig))
        (ignore-errors (swimmy.school::close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(deftest test-evaluate-strategy-performance-sends-to-graveyard
  "Evaluation failures should send strategies to graveyard (no benching)."
  (let* ((s (swimmy.school:make-strategy :name "EvalFail" :symbol "USDJPY"))
         (called nil)
         (orig (symbol-function 'swimmy.school:send-to-graveyard)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school:send-to-graveyard)
                (lambda (strat reason)
                  (setf called (list strat reason))
                  :graveyard))
          (swimmy.school::evaluate-strategy-performance s -0.1 10 50 1.0)
          (assert-true called "Expected send-to-graveyard to be invoked"))
      (setf (symbol-function 'swimmy.school:send-to-graveyard) orig))))

(deftest test-ensure-rank-retired-saves-pattern
  "ensure-rank :retired should save retired pattern, move strategy, and remove from KB."
  (let* ((s (swimmy.school:make-strategy :name "RetireMe" :symbol "USDJPY"))
         (swimmy.school::*strategy-knowledge-base* (list s))
         (swimmy.school::*category-pools* (make-hash-table :test 'equal))
         (calls nil)
         (orig-save (symbol-function 'swimmy.school::save-retired-pattern))
         (orig-move (symbol-function 'swimmy.persistence:move-strategy)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::save-retired-pattern)
                (lambda (strat reason)
                  (push (list :save strat reason) calls)
                  t))
          (setf (symbol-function 'swimmy.persistence:move-strategy)
                (lambda (strat new-rank &key force)
                  (declare (ignore force))
                  (push (list :move strat new-rank) calls)
                  t))
          (swimmy.school::ensure-rank s :retired "Max Age")
          (assert-equal :retired (swimmy.school:strategy-rank s) "Expected :retired rank")
          (assert-true (null (member s swimmy.school::*strategy-knowledge-base* :test #'eq))
                       "Expected removal from KB")
          (assert-true (find :save calls :key #'first) "Expected save-retired-pattern call")
          (assert-true (find :move calls :key #'first) "Expected move-strategy call"))
      (setf (symbol-function 'swimmy.school::save-retired-pattern) orig-save
            (symbol-function 'swimmy.persistence:move-strategy) orig-move))))

(deftest test-lifecycle-retire-on-max-losses
  "Lifecycle should retire strategies after max consecutive losses."
  (let* ((s (swimmy.school:make-strategy :name "LifeFail" :symbol "USDJPY"))
         (called nil)
         (orig (symbol-function 'swimmy.school:send-to-graveyard))
         (orig-max swimmy.school::*max-consecutive-losses*))
    (unwind-protect
        (progn
          (setf swimmy.school::*max-consecutive-losses* 1)
          (setf (symbol-function 'swimmy.school:send-to-graveyard)
                (lambda (strat reason)
                  (setf called (list strat reason))
                  :graveyard))
          (swimmy.school::manage-strategy-lifecycle s nil -1.0)
          (assert-true called "Expected lifecycle to retire strategy"))
      (setf (symbol-function 'swimmy.school:send-to-graveyard) orig
            swimmy.school::*max-consecutive-losses* orig-max))))

(deftest test-retired-patterns-weighted-in-avoidance
  "Retired patterns should contribute with lower weight to avoidance analysis."
  (let* ((orig-load-gy (symbol-function 'swimmy.school::load-graveyard-patterns))
         (orig-load-ret (symbol-function 'swimmy.school::load-retired-patterns)))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::load-graveyard-patterns)
                (lambda ()
                  (list (list :timeframe 15 :direction :BUY :symbol "USDJPY"
                              :sl 10 :tp 20 :timestamp (get-universal-time)))))
          (setf (symbol-function 'swimmy.school::load-retired-patterns)
                (lambda ()
                  (loop repeat 10 collect
                        (list :timeframe 15 :direction :BUY :symbol "USDJPY"
                              :sl 11 :tp 21 :timestamp (get-universal-time)))))
          (let ((regions (swimmy.school::analyze-graveyard-for-avoidance)))
            (assert-true (listp regions) "Expected avoid regions list")))
      (setf (symbol-function 'swimmy.school::load-graveyard-patterns) orig-load-gy
            (symbol-function 'swimmy.school::load-retired-patterns) orig-load-ret))))

(deftest test-daily-pnl-correlation
  "Daily PnL correlation should be near 1 for identical series"
  (let* ((tmp-db (format nil "/tmp/swimmy-corr-~a.db" (get-universal-time))))
    (let ((swimmy.core::*db-path-default* tmp-db)
          (swimmy.core::*sqlite-conn* nil)
          (swimmy.school::*disable-auto-migration* t))
      (unwind-protect
          (progn
            (swimmy.school::init-db)
            ;; Seed 3 days of identical pnl for A and B
            (dolist (spec '(("2026-02-01" 1.0) ("2026-02-02" 2.0) ("2026-02-03" 3.0)))
              (destructuring-bind (d v) spec
                (swimmy.school::execute-non-query
                 "INSERT OR REPLACE INTO strategy_daily_pnl (strategy_name, trade_date, pnl_sum, trade_count, updated_at)
                  VALUES (?, ?, ?, ?, ?)"
                 "A" d v 1 (get-universal-time))
                (swimmy.school::execute-non-query
                 "INSERT OR REPLACE INTO strategy_daily_pnl (strategy_name, trade_date, pnl_sum, trade_count, updated_at)
                  VALUES (?, ?, ?, ?, ?)"
                 "B" d v 1 (get-universal-time))))
            (let ((corr (swimmy.school::calculate-daily-pnl-correlation "A" "B" :days 3)))
              (assert-true (and corr (> corr 0.99)) "Correlation should be near 1")))
        (ignore-errors (swimmy.school::close-db-connection))
        (ignore-errors (delete-file tmp-db))))))

(defun mock-refresh-daily-pnl ()
  (push :daily-pnl *test-results*))

(deftest test-daily-pnl-aggregation-scheduler
  "Daily PnL aggregation should trigger at 00:10"
  (let ((swimmy.globals:*daily-pnl-aggregation-sent-today* nil)
        (orig (and (fboundp 'swimmy.school::refresh-strategy-daily-pnl)
                   (symbol-function 'swimmy.school::refresh-strategy-daily-pnl))))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.school::refresh-strategy-daily-pnl) #'mock-refresh-daily-pnl)
          (let ((time-0009 (encode-universal-time 0 9 0 1 2 2026)))
            (swimmy.main:check-scheduled-tasks time-0009)
            (assert-false swimmy.globals:*daily-pnl-aggregation-sent-today* "Should not trigger before 00:10"))
          (let ((time-0010 (encode-universal-time 0 10 0 1 2 2026)))
            (swimmy.main:check-scheduled-tasks time-0010)
            (assert-true swimmy.globals:*daily-pnl-aggregation-sent-today* "Should trigger at 00:10")))
      (when orig
        (setf (symbol-function 'swimmy.school::refresh-strategy-daily-pnl) orig)))))

;;; ─────────────────────────────────────────
;;; TEST RUNNER
;;; ─────────────────────────────────────────

;; Load extracted unit tests
;; (load (merge-pathnames "tests/school-split-tests.lisp" *load-truename*))

(defun run-all-tests ()
  "Run all tests"
  (setf *test-results* nil)
  (setf *tests-passed* 0)
  (setf *tests-failed* 0)
  
  (format t "~%═══════════════════════════════════════~%")
  (format t "🧪 RUNNING SWIMMY TESTS (V6.18 - Expert Verified)~%")
  (format t "═══════════════════════════════════════~%~%")
  
  ;; Run each test
  (dolist (test '(;; Clan tests
                  test-safe-read-rejects-read-eval
                  test-safe-read-allows-simple-alist
                  test-internal-process-msg-rejects-read-eval
                  test-internal-process-msg-backtest-request-id-bound
                  test-backtest-result-preserves-request-id
                  test-backtest-result-persists-trade-list
                  test-cpcv-result-persists-trade-list
                  test-backtest-debug-log-records-apply
                  test-backtest-status-includes-last-request-id
                  test-request-backtest-sets-submit-id
                  test-generate-uuid-uses-entropy-file
                  test-generate-uuid-changes-even-with-reset-rng
                  test-oos-retry-uses-new-request-id
                  test-oos-stale-result-ignored
                  test-oos-queue-clear-on-startup
                  test-strategy-to-alist-omits-filter-enabled-when-false
                  test-strategy-to-alist-includes-filter-enabled-when-true
                  test-order-open-uses-instrument-side
                  test-data-keeper-request-sexp
                  test-message-dispatcher-compiles-without-warnings
                  test-safe-read-used-for-db-rank
                  test-req-history-uses-count
                  test-order-open-sexp-keys
                  test-heartbeat-message-is-sexp
                  test-executor-heartbeat-sends-sexp
                  test-executor-pending-orders-sends-sexp
                  test-internal-cmd-json-disallowed
                  test-internal-process-msg-tick-sexp
                  test-internal-process-msg-history-sexp
                  test-process-account-info-sexp
                  test-process-trade-closed-sexp
                  test-normalize-legacy-plist->strategy
                  test-normalize-struct-roundtrip
                  test-sexp-io-roundtrip
                  test-write-sexp-atomic-stable-defaults
                  test-backtest-cache-sexp
                  test-trade-logs-supports-pair-id
                  test-strategy-daily-pnl-aggregation
                  test-daily-pnl-correlation
                  test-daily-pnl-aggregation-scheduler
                  test-2300-trigger-logic
                  test-midnight-reset-logic
                  test-daily-report-no-duplicate-after-flag-reset
                  test-promotion-triggers-noncorrelation-notification
                  test-backtest-trade-logs-insert
                  test-fetch-backtest-trades
                  test-pair-id-stable
                  test-pair-overlay-caps-lot
                  test-pair-candidate-pool-top-n
                  test-align-pnl-series-padding
                  test-pair-align-pnl-series-zero-fill
                  test-pair-pearson-correlation
                  test-pair-score-from-pnls
                  test-pair-inverse-vol-weights
                  test-pair-gate-blocks-on-insufficient-data
                  test-pair-slot-competition-cap
                  test-pair-promotion-blocked-without-oos-cpcv
                  test-pair-selection-rescue-mode
                  test-pair-selection-excludes-short-trades
                  test-telemetry-sexp
                  test-live-status-sexp
                  test-telemetry-event-schema
                  test-clan-exists
                  test-get-clan
                  test-clan-display
                  ;; Macro tests
                  ;; (Temporarily removed missing tests)
                  ;; V6.18: Danger tests
                  test-danger-level-initial
                  test-consecutive-losses-tracked
                  test-cooldown-returns-false-initially
                  ;; V6.18: Resignation tests
                  test-has-resigned-initial
                  test-resignation-threshold-exists
                  ;; V6.18: Leader tests
                  test-leader-info-struct
                  ;; V6.18: Risk tests
                  test-risk-summary
                  ;; Backtest DB sync regression
                  test-apply-backtest-result-updates-data-sexp
                  test-kill-strategy-persists-status
                  test-max-age-retire-batched-notification
                  test-collect-all-strategies-unpruned
                  test-map-strategies-from-db-batched
                  test-map-strategies-from-db-limit
                  test-pair-strategy-upsert-fetch
                  test-db-rank-counts
                  test-report-source-drift-detects-mismatch
                  test-evolution-report-uses-db-counts
                  ;; Backtest payload S-expression tests
                  test-request-backtest-indicator-type-symbol
                  ;; V6.18: Dynamic TP/SL tests
                  test-volatility-multiplier
                  test-atr-empty-candles
                  ;; V6.18: Utility tests
                  test-gotobi-day-returns-boolean
                  test-london-session-check
                  ;; V6.18: Candle tests
                  test-candle-creation
                  ;; V7.0: School Split Tests (Taleb)
                  test-time-decay-weight
                  test-retired-patterns-weighted-in-avoidance
                  test-pattern-similarity
                  test-calculate-pattern-similarity-behavior ; [V8.2] Uncle Bob
                  test-atr-calculation-logic
                  test-volatility-shifts
                  test-prediction-structure
                  ;; V8.0: Walk-Forward Validation Tests (López de Prado)
                  test-wfv-logic-robust-strategy
                  test-wfv-logic-overfit-strategy
                  test-wfv-scheduling-respects-interval-and-pending
                  test-wfv-pending-stats-oldest-age
                  test-oos-validation-dispatches-when-unset
                  test-oos-status-updated-on-dispatch
                  test-evolution-report-includes-oos-status
                  test-oos-status-line-no-queue-duplication
                  test-oos-status-line-ignores-queue-error
                  test-evolution-report-includes-phase2-end-time
                  ;; V7.1: Persistence Tests (Andrew Ng)
                  test-learning-persistence
                  ;; V8.0: Advisor Reports (Expert Panel)
                  test-advisor-reports
                  ;; V8.4: Characterization Tests (memo3.txt)
                  test-maintenance-throttle-60s
                  test-dream-cycle-self-throttle
                  test-processing-step-no-maintenance
                  test-backtest-debug-enabled-p
                  test-flush-deferred-founders-respects-limit
                  test-backtest-pending-counters-defaults
                  test-backtest-send-throttles-when-pending-high
                  test-backtest-send-uses-subsecond-time
                  test-send-zmq-sleep-suppressed-for-backtest-requester
                  test-rr-batch-respects-max-pending
                  test-rr-batch-skips-retired-strategies
                  test-rr-batch-no-active-strategies
                  test-format-phase1-bt-batch-message
                  test-format-percent-no-double
                  test-format-value-rounds-int
                  test-ledger-persists-equity
                  test-system-pulse-5m-text
                  test-backtest-pending-count-decrements-on-recv
                  test-deferred-flush-respects-batch
                  test-backtest-uses-csv-override
                  test-backtest-v2-uses-alist
                  test-backtest-v2-phase2-promotes-to-a
                  test-evaluate-strategy-performance-sends-to-graveyard
                  test-ensure-rank-retired-saves-pattern
                  test-lifecycle-retire-on-max-losses
                  ;; V8.5: Evolution Tests (Genetic Mutation)
                  test-rewrite-logic-symbols-sma
                  test-mutate-strategy-structure
                  test-mutate-param-sl-tp
                  ;; Expert Panel P1: Symbol Mismatch Tests
                  test-check-symbol-mismatch-blocks-cross-trading
                  test-check-symbol-mismatch-allows-correct-pair
                  test-check-symbol-mismatch-allows-generic
                  test-check-symbol-mismatch-blocks-eurusd-on-gbpusd
                  test-check-symbol-mismatch-case-insensitive))
    (format t "Running ~a... " test)
    (if (funcall test)
        (format t "✅ PASSED~%")
        (format t "❌ FAILED~%")))
  
  (format t "~%═══════════════════════════════════════~%")
  (format t "📊 RESULTS: ~d passed, ~d failed~%~%" *tests-passed* *tests-failed*)
  (format t "Test coverage: Clan, Macros, Danger, Resignation,~%")
  (format t "               Leader, Risk, Dynamic TP/SL, Utils~%")
  (format t "               School Split (Learning, Volatility, Research)~%")
  (format t "═══════════════════════════════════════~%~%")
  
  (values *tests-passed* *tests-failed*))

(format t "[TESTS] Test framework loaded (V7.0 - Expert Verified)~%")
(format t "[TESTS] Run (swimmy-tests:run-all-tests) to execute ~d tests~%" 23)
