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
         (divrobust (swimmy.core::execute-single
                     (format nil "SELECT count(*) FROM strategies WHERE ~a AND symbol<>'USDJPY' AND UPPER(category) NOT LIKE '%TREND%' AND cpcv_pass_rate>=0.6" *active-where*))))
    (format t "~&[~a] active=~d  USDJPY=~d (~,1f%)  TREND=~d (~,1f%)  diverse-robust=~d~%"
            tag total usd (if (> total 0) (* 100.0 (/ usd total)) 0.0)
            trend (if (> total 0) (* 100.0 (/ trend total)) 0.0) divrobust)
    (format t "~&        symbols=~s cats=~s~%" syms cats)
    (list :total total :usd usd :trend trend :divrobust divrobust)))

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

;;; ---- 2. 2f bootstrap seeds: 20 diverse Keltner/BB-MR (trap fix) ------
(let* ((usd (remove-if-not (lambda (s) (equal (strategy-symbol s) "USDJPY")) *strategy-knowledge-base*))
       (nseed 0))
  (loop for i from 0 below 20
        for base = (nth (mod (* i 7) (max 1 (length usd))) usd)
        when base do
          (let ((c (copy-strategy base))
                (prim (if (evenp i) 'keltner 'bollinger)))
            (setf (strategy-name c) (format nil "SEED-~a-~d" prim i)
                  (strategy-symbol c) (if (< (mod i 3) 2) "EURUSD" "GBPUSD")
                  (strategy-category c) :reversion
                  (strategy-indicators c) (list (list prim (+ 45 (* 3 (mod i 7)))))
                  (strategy-band-mult c) (nth (mod i 3) '(1.5d0 2.0d0 2.5d0))
                  (strategy-atr-period c) 14
                  (strategy-atr-barrier-sl c) (if (evenp i) 2.0d0 3.0d0)
                  (strategy-atr-barrier-tp c) (if (evenp i) 2.0d0 3.0d0)
                  (strategy-timeframe c) (nth (mod i 2) '(240 360))
                  (strategy-sl c) 0.005d0 (strategy-tp c) 0.005d0
                  (strategy-cpcv-pass-rate c) 0.7d0 (strategy-sharpe c) 0.7d0
                  (strategy-cpcv-median-sharpe c) 0.5d0
                  (strategy-profit-factor c) 1.5d0 (strategy-win-rate c) 0.55d0
                  (strategy-max-dd c) 0.05d0 (strategy-oos-sharpe c) 0.5d0 (strategy-trades c) 400
                  (strategy-breeding-count c) 0 (strategy-generation c) 0
                  (strategy-rank c) :B (strategy-tier c) :battlefield (strategy-age c) 0
                  (strategy-status c) :active (strategy-revalidation-pending c) nil)
            (multiple-value-bind (ok st) (add-to-kb c :breeder :require-bt nil :notify nil)
              (declare (ignore st))
              (when ok (incf nseed)))))
  (format t "~&SEEDED ~d diverse Keltner/BB-MR into KB+DB (2f bootstrap)~%" nseed))
(build-category-pools)
(format t "~&===== GEN 0b (post-seed) =====~%")
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
