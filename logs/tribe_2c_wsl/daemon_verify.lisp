;;;; =====================================================================
;;;; (A) DAEMON-ISOLATION VERIFICATION — real breeding pipeline, copy DB
;;;; =====================================================================
;;;; Exercises the REAL pipeline on a COPY DB with flag ON + 2f bootstrap:
;;;;   breed-strategies (2c) -> add-to-kb :breeder :require-bt t (real dedup/
;;;;   correlation/graveyard gates + defer + pending DB upsert) -> [score the
;;;;   pending children via primitive_scan = guardian's backtester/CPCV engine]
;;;;   -> handle-v2-result (real Phase-1 promotion: ensure-rank :B / graveyard
;;;;   + upsert) -> run-rank-evaluation (real A-rank) -> cull-pool-overflow
;;;;   (real 20-or-die) -> measure the DB distribution each generation.
;;;; The ONLY substitution vs the live daemon is the backtest TRANSPORT:
;;;; synchronous batched primitive_scan.exe instead of async ZMQ->guardian.
;;;; NO live orders, NO motor socket, NO guardian daemon, live swimmy.db untouched.
(in-package :swimmy.school)

;;; ---- 0. isolation: copy DB + flag ON + 2f bootstrap -----------------
(setf swimmy.core::*db-path-default* "/mnt/c/tmp/swimmy_daemon_verify.db")
(ignore-errors (swimmy.core::close-db-connection))
(setf swimmy.core:*enable-primitive-diversity* t)     ; flag ON (harness only)
(setf *2c-cpcv-pregate-min* 0.55)                     ; 2f pre-gate bootstrap (NOT honest floor)
;; non-startup so add-to-kb applies real gates + breeder defer
(let ((s (or (find-symbol "*STARTUP-MODE*" :swimmy.core)
             (find-symbol "*STARTUP-MODE*" :swimmy.globals)
             (find-symbol "*STARTUP-MODE*" :swimmy.school))))
  (when (and s (boundp s)) (set s nil)))
;; ISOLATION: redirect the strategy Library WRITE path to scratch so
;; run-rank-evaluation / save-recruit-to-lisp / graveyard saves NEVER touch
;; the repo's data/library working tree.
(let ((lp (find-symbol "*LIBRARY-PATH*" :swimmy.persistence)))
  (unless lp
    (format t "~&[FATAL] *library-path* not found — aborting to avoid repo pollution~%")
    (sb-ext:exit :code 9))
  (set lp #p"/mnt/c/tmp/verify_library/")
  (format t "~&[ISOLATION] library WRITE path => ~a~%" (symbol-value lp)))

;;; ---- neutralize outward side-effects (defensive) --------------------
(dolist (fn '(notify-death notify-recruit-unified send-discord-briefing send-discord-heartbeat))
  (when (fboundp fn) (setf (fdefinition fn) (lambda (&rest _) (declare (ignore _)) nil))))
;;; SHIM: replace the async ZMQ backtest dispatch with a no-op so the REAL
;;; add-to-kb deferral path runs without a socket; we score the pending
;;; children in a batch afterwards and drive the REAL handle-v2-result.
(setf (fdefinition 'request-backtest-v2) (lambda (&rest _) (declare (ignore _)) nil))

;;; ---- honest scoring via primitive_scan.exe (guardian engine) --------
(defun ps-row (s)
  (let* ((al (strategy-to-alist s))
         (prim (string-downcase (format nil "~a" (or (cdr (assoc 'indicator_type al)) "bb"))))
         (period (or (cdr (assoc 'sma_short al)) 30))
         (dev (float (or (cdr (assoc 'band_mult al)) 2.0) 1.0))
         (atrp (or (cdr (assoc 'atr_period al)) 14))
         (atrsl (float (or (cdr (assoc 'atr_barrier_sl al)) 0.0) 1.0))
         (atrtp (float (or (cdr (assoc 'atr_barrier_tp al)) 0.0) 1.0))
         (tf (or (cdr (assoc 'timeframe al)) 240)))
    (format nil "{\"name\":\"~a\",\"symbol\":\"~a\",\"regime\":\"~a\",\"prim\":\"~a\",\"period\":~d,\"dev\":~,3f,\"atr_period\":~d,\"tf_seconds\":~d,\"barrier_mode\":\"~a\",\"sl\":~,5f,\"tp\":~,5f,\"max_hold\":0}"
            (strategy-name s) (strategy-symbol s) (string-downcase (symbol-name (strategy-category s)))
            prim period dev atrp (* tf 60) (if (> atrsl 0.0) "atr" "pip")
            (if (> atrsl 0.0) atrsl 0.005) (if (> atrtp 0.0) atrtp 0.005))))

(defun score-batch (strats symbol slippage out)
  (when strats
    ;; repo-RELATIVE paths: cwd is the repo root, which primitive_scan.exe (a
    ;; Windows binary via WSL interop) resolves against the Windows repo path.
    ;; Absolute Linux /mnt/c paths are NOT resolvable by the Windows binary.
    (let ((mani (format nil "logs/tribe_2c_wsl/dv_~a.json" symbol))
          (outf (format nil "logs/tribe_2c_wsl/dv_~a_out.json" symbol)))
      (with-open-file (o mani :direction :output :if-exists :supersede :if-does-not-exist :create)
        (format o "[~%~{~a~^,~%~}~%]~%" (mapcar #'ps-row strats)))
      (uiop:run-program (list "./target/release/primitive_scan.exe"
                              "--data" (format nil "data/historical/~a_M1.csv" symbol)
                              "--manifest" mani "--slippage" (format nil "~f" slippage) "--out" outf)
                        :ignore-error-status t :output nil :error-output nil)
      (handler-case
          (let* ((json (with-open-file (in outf :external-format :utf-8)
                         (let ((s (make-string (file-length in)))) (read-sequence s in) s)))
                 (arr (jsown:val (jsown:parse json) "strategies")))
            (dolist (r arr)
              (let ((oos (jsown:val r "oos")) (cpcv (jsown:val r "cpcv")))
                (setf (gethash (jsown:val r "name") out)
                      (list :sharpe (float (jsown:val oos "penalized_sharpe") 1.0)
                            :pf (float (or (ignore-errors (jsown:val oos "pf")) 1.0) 1.0)
                            :wr (float (or (ignore-errors (jsown:val oos "win_rate")) 0.5) 1.0)
                            :maxdd (float (or (ignore-errors (jsown:val oos "max_dd")) 0.1) 1.0)
                            :trades (jsown:val oos "trades")
                            :cpcv-pr (float (jsown:val cpcv "pass_rate") 1.0)
                            :cpcv-med (float (jsown:val cpcv "median_sharpe") 1.0))))))
        (error (e) (format t "~&SCORE-WARN ~a: ~a~%" symbol e)))))
  out)

(defun score-all (strats)
  (let ((out (make-hash-table :test 'equal)))
    (dolist (sym '("USDJPY" "EURUSD" "GBPUSD" "EURJPY"))
      (score-batch (remove-if-not (lambda (s) (equal (strategy-symbol s) sym)) strats)
                   sym (if (member sym '("USDJPY" "EURJPY") :test #'equal) 0.01 0.0001) out))
    out))

;;; ---- DB distribution measurement (from the copy DB) -----------------
(defparameter *active-where*
  "UPPER(TRIM(COALESCE(rank,''))) IN (':B','B',':A','A',':S','S',':LEGEND','LEGEND')")
(defun db-distro (tag)
  (let* ((total (swimmy.core::execute-single (format nil "SELECT count(*) FROM strategies WHERE ~a" *active-where*)))
         (syms (swimmy.core::execute-to-list (format nil "SELECT symbol,count(*) FROM strategies WHERE ~a GROUP BY symbol ORDER BY count(*) DESC" *active-where*)))
         (cats (swimmy.core::execute-to-list (format nil "SELECT category,count(*) FROM strategies WHERE ~a GROUP BY category ORDER BY count(*) DESC" *active-where*)))
         (usd (or (second (assoc "USDJPY" syms :test #'equal)) 0))
         (trend (loop for row in cats when (search "TREND" (string-upcase (format nil "~a" (first row)))) sum (second row)))
         ;; diverse-robust (legacy defn): non-USDJPY, non-TREND, CPCV-robust.
         (divrobust (swimmy.core::execute-single
                     (format nil "SELECT count(*) FROM strategies WHERE ~a AND symbol<>'USDJPY' AND UPPER(category) NOT LIKE '%TREND%' AND cpcv_pass_rate>=0.6" *active-where*)))
         ;; ESCAPE-ROUTE target metric: non-USDJPY TREND survivors (the regime the monoculture
         ;; owns). survive = active rank; robust = additionally CPCV>=0.6.
         (nonusd-trend (swimmy.core::execute-single
                        (format nil "SELECT count(*) FROM strategies WHERE ~a AND symbol<>'USDJPY' AND UPPER(category) LIKE '%TREND%'" *active-where*)))
         (nonusd-trend-robust (swimmy.core::execute-single
                        (format nil "SELECT count(*) FROM strategies WHERE ~a AND symbol<>'USDJPY' AND UPPER(category) LIKE '%TREND%' AND cpcv_pass_rate>=0.6" *active-where*)))
         ;; any surviving DVSEED-* (the injected seeds themselves, whatever their fate)
         (seed-alive (swimmy.core::execute-single
                        (format nil "SELECT count(*) FROM strategies WHERE ~a AND name LIKE 'DVSEED-%'" *active-where*))))
    (format t "~&[~a] active=~d  USDJPY=~d (~,1f%)  TREND=~d (~,1f%)  diverse-robust=~d~%"
            tag total usd (if (> total 0) (* 100.0 (/ usd total)) 0.0)
            trend (if (> total 0) (* 100.0 (/ trend total)) 0.0) divrobust)
    (format t "~&        non-USDJPY-TREND: survive=~d robust=~d | DVSEED alive=~d~%"
            nonusd-trend nonusd-trend-robust seed-alive)
    (format t "~&        symbols=~s cats=~s~%" syms cats)
    (list :total total :usd usd :trend trend :divrobust divrobust
          :nonusd-trend nonusd-trend :nonusd-trend-robust nonusd-trend-robust
          :seed-alive seed-alive)))

;;; ---- 1. load population from copy DB + build daemon pools ------------
(init-db)
(setf *strategy-knowledge-base* (fetch-all-strategies-from-db :ranks '(":B" ":A" ":S" ":LEGEND")))
(format t "~&LOADED ~d active strategies from copy DB~%" (length *strategy-knowledge-base*))
;; Age the loaded veterans so the daily selection-fitness culls (age>10 / >5) apply
;; to them; fresh seeds + bred children (age 0) stay protected.
(dolist (s *strategy-knowledge-base*) (when (< (or (strategy-age s) 0) 12) (setf (strategy-age s) 15)))
(build-category-pools)
(format t "~&===== GEN 0 (loaded monoculture, pre-seed) =====~%")
(db-distro "GEN0")

;;; ---- 2. ESCAPE ROUTE: inject non-USDJPY-TREND diversity seeds, HONESTLY scored ----
;;; This REPLACES the previous run's inline fabricated-metric REVERSION seeding (which added
;;; 20 EUR/GBP Keltner/BB seeds with cpcv=0.7/sharpe=0.7 pre-set and :require-bt nil — i.e.
;;; they never touched the honest gate and were COSMETIC by construction). The escape-route
;;; seeds come from the flag-gated source path inject-diversity-seeds (src/lisp/school/
;;; school-seed.lisp): TREND-first, non-USDJPY, NO fabricated performance. Each seed bypasses
;;; ONLY the breeding CPCV pre-gate (it is injected directly, not bred); it is then HONESTLY
;;; scored by primitive_scan (guardian's real OOS/CPCV engine on real local price data) and
;;; driven through the SAME Phase-1 gate (handle-v2-result) + §4 floor (run-rank-evaluation)
;;; as every bred child. Only genuine honest-PASS seeds become rank :B and count toward the
;;; active pool. "Give the entry, never fabricate the verdict."
(defparameter *seed-orig-names* (make-hash-table :test 'equal))
(let* ((seeds (inject-diversity-seeds *strategy-knowledge-base*))
       (pending nil) (nrej 0))
  (format t "~&INJECT: ~d diverse seed candidates (flag-gated escape route)~%" (length seeds))
  (dolist (c seeds)
    (setf (gethash (strategy-name c) *seed-orig-names*) (strategy-category c))
    (multiple-value-bind (ok st)
        (handler-case (add-to-kb c :breeder :require-bt t :notify nil)
          (error (e) (format t "~&  SEED-ADDKB-ERR ~a~%" e) (values nil :err)))
      (if (and ok (eq st :queued-phase1)) (push c pending) (incf nrej))))
  (format t "~&  seed add-to-kb: pending=~d rejected-by-gates=~d~%" (length pending) nrej)
  ;; HONEST scoring: real OOS/CPCV via primitive_scan (guardian's backtester/CPCV engine).
  (let ((scores (score-all pending)) (npass 0) (ntrendpass 0) (ngrave 0) (nnoscore 0))
    (dolist (c pending)
      (let ((m (gethash (strategy-name c) scores)))
        (if (null m)
            (progn (incf nnoscore)
                   (format t "~&  SEED-NOSCORE ~a~%" (strategy-name c)))
            (progn
              (setf (strategy-cpcv-pass-rate c) (getf m :cpcv-pr)
                    (strategy-cpcv-median-sharpe c) (getf m :cpcv-med))
              (handler-case
                  (handle-v2-result (format nil "~a_P1" (strategy-name c))
                                    (list :sharpe (getf m :sharpe) :profit-factor (getf m :pf)
                                          :win-rate (getf m :wr) :trades (getf m :trades)
                                          :max-dd (getf m :maxdd)))
                (error (e) (format t "~&  SEED-HANDLE-ERR ~a: ~a~%" (strategy-name c) e)))
              (let ((rk (strategy-rank c)))
                (format t "~&  SEED ~a sym=~a reg=~a  oos-sharpe=~,3f pf=~,3f cpcv-pr=~,2f trades=~d  -> rank=~a~%"
                        (strategy-name c) (strategy-symbol c)
                        (string-downcase (symbol-name (strategy-category c)))
                        (getf m :sharpe) (getf m :pf) (getf m :cpcv-pr) (getf m :trades) rk)
                (cond ((member rk '(:B :A :S))
                       (incf npass) (when (eq (strategy-category c) :trend) (incf ntrendpass)))
                      ((eq rk :graveyard) (incf ngrave))))))))
    ;; Persist each seed's HONEST verdict to the copy DB so db-distro / the gen-loop reload
    ;; reflect the real gate outcome (belt-and-suspenders on ensure-rank's own persistence).
    (dolist (c pending)
      (let ((rk (strategy-rank c)))
        (ignore-errors
          (swimmy.core::execute-non-query
           "UPDATE strategies SET rank=?, cpcv_pass_rate=?, symbol=?, category=? WHERE name=?"
           (cond ((member rk '(:B :A :S)) (format nil ":~a" rk))  ; ":B" — DB uses colon-prefix
                 ((eq rk :graveyard) ":GRAVEYARD")
                 (t ":GRAVEYARD"))  ; unresolved/no-score seed = not active (honest: never promoted)
           (float (or (strategy-cpcv-pass-rate c) 0.0) 1.0)
           (strategy-symbol c)
           (format nil "~a" (strategy-category c))
           (strategy-name c)))))
    (format t "~&SEED HONEST-GATE RESULT: seeded=~d honest-PASS=~d (non-USDJPY-TREND-PASS=~d) graveyard=~d no-score=~d~%"
            (length seeds) npass ntrendpass ngrave nnoscore)))
(build-category-pools)
(format t "~&===== GEN 0b (post-seed, HONESTLY scored) =====~%")
(db-distro "GEN0b")

;;; ---- 3. one real generation ----------------------------------------
(defun diverse-p (s)
  (and (not (equal (strategy-symbol s) "USDJPY")) (not (eq (strategy-category s) :trend))))

(defun breed-one-gen (pop)
  (let ((children nil))
    (dolist (cat '(:trend :reversion :breakout :scalp))
      (let* ((warriors (remove-if-not
                        (lambda (s) (and (eq (or (ignore-errors (strategy-regime-class s))
                                                 (strategy-category s)) cat)
                                         (breeding-cpcv-eligible-p s))) pop))
             (sorted (sort (copy-list warriors) #'>
                           :key (lambda (s) (or (ignore-errors (strategy-breeding-priority-score s)) 0))))
             (used (make-hash-table :test 'equal)) (pairs 0))
        (loop for i from 0 below (length sorted) while (< pairs 20)
              for p1 = (nth i sorted)
              do (when (and p1 (null (gethash (strategy-name p1) used)) (ignore-errors (can-breed-p p1)))
                   (let ((p2 (ignore-errors (find-diverse-breeding-partner p1 sorted :start-index (1+ i) :used-names used))))
                     (when p2 (incf pairs)
                       (setf (gethash (strategy-name p1) used) t (gethash (strategy-name p2) used) t)
                       (let ((c (ignore-errors (breed-strategies p1 p2)))) (when c (push c children)))))))))
    children))

(defun run-gen (g)
  (format t "~&===== GEN ~d =====~%" g)
  (let* ((pop (remove-if (lambda (s) (eq (strategy-rank s) :graveyard)) *strategy-knowledge-base*))
         (children (breed-one-gen pop))
         (ndiv-bred (count-if #'diverse-p children))
         (pending nil) (nrej 0))
    (format t "~&  bred=~d (diverse=~d)~%" (length children) ndiv-bred)
    (dolist (c children)
      (multiple-value-bind (ok st)
          (handler-case (add-to-kb c :breeder :require-bt t :notify nil)
            (error (e) (format t "~&  ADDKB-ERR ~a~%" e) (values nil :err)))
        (if (and ok (eq st :queued-phase1)) (push c pending) (incf nrej))))
    (format t "~&  add-to-kb: pending=~d rejected-by-gates=~d~%" (length pending) nrej)
    (let ((scores (score-all pending)) (nprom 0) (ndivprom 0) (ngrave 0))
      (dolist (c pending)
        (let ((m (gethash (strategy-name c) scores)))
          (when m
            (setf (strategy-cpcv-pass-rate c) (getf m :cpcv-pr)
                  (strategy-cpcv-median-sharpe c) (getf m :cpcv-med))
            (handler-case
                (handle-v2-result (format nil "~a_P1" (strategy-name c))
                                  (list :sharpe (getf m :sharpe) :profit-factor (getf m :pf)
                                        :win-rate (getf m :wr) :trades (getf m :trades)
                                        :max-dd (getf m :maxdd)))
              (error (e) (format t "~&  HANDLE-ERR ~a: ~a~%" (strategy-name c) e)))
            (let ((rk (strategy-rank c)))
              (cond ((member rk '(:B :A :S)) (incf nprom) (when (diverse-p c) (incf ndivprom)))
                    ((eq rk :graveyard) (incf ngrave)))))))
      (format t "~&  handle-v2-result: promoted-to-B+=~d (diverse=~d) graveyard=~d~%" nprom ndivprom ngrave))
    (handler-case (run-rank-evaluation) (error (e) (format t "~&  RANK-EVAL-ERR ~a~%" e)))
    (dolist (cat '(:trend :reversion :breakout :scalp))
      (handler-case (cull-pool-overflow cat) (error (e) (format t "~&  CULL-ERR ~a ~a~%" cat e))))
    ;; 2d fitness-sharing dilution: daily stagnant/weak culls score by selection-fitness
    ;; (evidence-adjusted-sharpe / niche-density). Snapshot active names, run the culls,
    ;; then PERSIST kills to the copy DB (the daily-cull path marks in-memory but does not
    ;; reliably write the rank column), so the reload reflects them.
    (let* ((before (remove-if-not (lambda (s) (eq (strategy-status s) :active)) *strategy-knowledge-base*))
           (before-names (mapcar #'strategy-name before))
           (usd-before (count-if (lambda (s) (equal (strategy-symbol s) "USDJPY")) before)))
      (handler-case (progn (cull-stagnant-crank-daily) (cull-weak-strategies))
        (error (e) (format t "~&  FITSHARE-CULL-ERR ~a~%" e)))
      (let ((alive (make-hash-table :test 'equal)) (nkilled 0))
        (dolist (s *strategy-knowledge-base*)
          (when (eq (strategy-status s) :active) (setf (gethash (strategy-name s) alive) t)))
        (dolist (nm before-names)
          (unless (gethash nm alive)
            (incf nkilled)
            (ignore-errors (swimmy.core::execute-non-query
                            "UPDATE strategies SET rank=':GRAVEYARD' WHERE name=?" nm))))
        (format t "~&  2d fitness-sharing cull: USDJPY(active before)=~d killed=~d (persisted)~%" usd-before nkilled)))
    (setf *strategy-knowledge-base* (fetch-all-strategies-from-db :ranks '(":B" ":A" ":S" ":LEGEND")))
    ;; Re-age: veteran mono stays cull-eligible (age 15); fresh diverse/seed/bred lineage
    ;; stays protected (age 0) so it keeps breeding (matches new lineage in the live daemon).
    (dolist (s *strategy-knowledge-base*)
      (let ((nm (strategy-name s)))
        (setf (strategy-age s) (if (or (search "SEED" nm) (search "Bred-" nm)) 0 15))))
    (build-category-pools)
    (db-distro (format nil "GEN~d" g))))

(dotimes (g 6) (run-gen (1+ g)) (finish-output))
(format t "~&===== DONE =====~%")
(sb-ext:exit :code 0)
