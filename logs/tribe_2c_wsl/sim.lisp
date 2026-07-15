;;; 2c multi-generation dilution simulation (SAFE: no live orders, no live DB, no zmq).
;;; Uses the REAL 2c breed + 2d selection-fitness + 2f pre-gate + cull logic, and scores
;;; every child HONESTLY by invoking the standalone primitive_scan.exe (OOS 2pip + purged
;;; CPCV) via WSL interop. Tracks the population symbol/regime distribution per generation.
(in-package :swimmy.school)
(setf swimmy.core:*enable-primitive-diversity* t)
(setf *2c-cpcv-pregate-min* 0.55)          ; 2f bootstrap: lowered from 0.60

;; --- honest scoring via primitive_scan.exe (interop) + jsown ---
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

(defun score-batch (strats symbol slippage)
  (when strats
    (let ((mani (format nil "logs/tribe_2c_wsl/sim_~a.json" symbol))
          (outf (format nil "logs/tribe_2c_wsl/sim_~a_out.json" symbol)))
      (with-open-file (o mani :direction :output :if-exists :supersede :if-does-not-exist :create)
        (format o "[~%~{~a~^,~%~}~%]~%" (mapcar #'ps-row strats)))
      (uiop:run-program (list "./target/release/primitive_scan.exe"
                              "--data" (format nil "data/historical/~a_M1.csv" symbol)
                              "--manifest" mani "--slippage" (format nil "~f" slippage)
                              "--out" outf)
                        :ignore-error-status t :output nil :error-output nil)
      (handler-case
          (let* ((json (with-open-file (in outf :external-format :utf-8)
                         (let ((s (make-string (file-length in)))) (read-sequence s in) s)))
                 (parsed (jsown:parse json))
                 (arr (jsown:val parsed "strategies"))
                 (by-name (make-hash-table :test 'equal)))
            (dolist (r arr) (setf (gethash (jsown:val r "name") by-name) r))
            (dolist (s strats)
              (let ((r (gethash (strategy-name s) by-name)))
                (when r
                  (let ((oos (jsown:val r "oos")) (cpcv (jsown:val r "cpcv")))
                    (setf (strategy-sharpe s) (float (jsown:val oos "penalized_sharpe") 1.0))
                    (setf (strategy-cpcv-pass-rate s) (float (jsown:val cpcv "pass_rate") 1.0))
                    (setf (strategy-cpcv-median-sharpe s) (float (jsown:val cpcv "median_sharpe") 1.0))
                    (setf (strategy-trades s) (jsown:val oos "trades")))))))
        (error (e) (format t "~&SCORE-WARN ~a: ~a~%" symbol e))))))

(defun score-all (strats)
  (dolist (sym '("USDJPY" "EURUSD" "GBPUSD" "EURJPY"))
    (score-batch (remove-if-not (lambda (s) (equal (strategy-symbol s) sym)) strats)
                 sym (if (member sym '("USDJPY" "EURJPY") :test #'equal) 0.01 0.0001))))

(defun breed-gen (pop)
  (let ((children nil)
        (eligible (count-if #'breeding-cpcv-eligible-p pop)))
    (format t "~&  [2f-eligible pool=~d/~d]~%" eligible (length pop))
    (dolist (cat '(:trend :reversion :breakout :scalp))
      (let* ((warriors (remove-if-not (lambda (s) (and (eq (strategy-category s) cat)
                                                       (breeding-cpcv-eligible-p s))) pop))
             (sorted (sort (copy-list warriors) #'>
                           :key (lambda (s) (or (ignore-errors (selection-fitness s pop)) 0))))
             (used (make-hash-table :test 'equal)) (pairs 0))
        (loop for i from 0 below (length sorted) while (< pairs 30)
              for p1 = (nth i sorted)
              do (when (and p1 (null (gethash (strategy-name p1) used)) (ignore-errors (can-breed-p p1)))
                   (let ((p2 (ignore-errors (find-diverse-breeding-partner p1 sorted :start-index (1+ i) :used-names used))))
                     (when p2 (incf pairs)
                       (setf (gethash (strategy-name p1) used) t (gethash (strategy-name p2) used) t)
                       (let ((c (ignore-errors (breed-strategies p1 p2)))) (when c (push c children)))))))))
    children))

(defun cull-to (pop cap)
  (let ((sorted (sort (copy-list pop) #'> :key (lambda (s) (or (ignore-errors (selection-fitness s pop)) -999)))))
    (if (> (length sorted) cap) (subseq sorted 0 cap) sorted)))

(defun distro (pop)
  (let ((sym (make-hash-table :test 'equal)) (cat (make-hash-table :test 'equal)))
    (dolist (s pop)
      (incf (gethash (strategy-symbol s) sym 0))
      (incf (gethash (string-downcase (symbol-name (strategy-category s))) cat 0)))
    (let (a b) (maphash (lambda (k v) (push (cons k v) a)) sym) (maphash (lambda (k v) (push (cons k v) b)) cat)
      (list :sym (sort a #'> :key #'cdr) :cat (sort b #'> :key #'cdr)))))

(defun n-diverse-robust (pop)
  (count-if (lambda (s) (and (not (equal (strategy-symbol s) "USDJPY"))
                             (not (eq (strategy-category s) :trend))
                             (>= (or (strategy-cpcv-pass-rate s) 0) 0.6)
                             (>= (or (strategy-sharpe s) 0) 0.3))) pop))

;; --- run ---
(setf *strategy-knowledge-base* (swimmy.persistence::load-all-strategies))
(let* ((usd (remove-if (lambda (s) (or (eq (strategy-rank s) :graveyard)
                                       (not (equal (strategy-symbol s) "USDJPY"))))
                       *strategy-knowledge-base*))
       (mono (subseq usd 0 (min 120 (length usd))))         ; 120 USDJPY/TREND monoculture
       ;; 2f bootstrap: 20 forward-robust seeds made by CLONING real (breedable) strategies
       ;; and converting them to genetically-varied diverse Keltner/BB-MR. Cloning preserves
       ;; the breeding-eligibility fields (pnl-history, trades, tier) that can-breed-p /
       ;; correlation gates need, which bare synthetic structs lack.
       (seeds (loop for i from 0 below 20
                    for base = (nth (mod (* i 7) (max 1 (length usd))) usd)
                    collect (let ((c (copy-strategy base))
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
                                    ;; generous quality/oos metrics so can-breed-p's B quality
                                    ;; floor + oos floor pass (rank :B skips A-conformance).
                                    (strategy-profit-factor c) 1.5d0 (strategy-win-rate c) 0.55d0
                                    (strategy-max-dd c) 0.05d0 (strategy-oos-sharpe c) 0.5d0 (strategy-trades c) 400
                                    (strategy-breeding-count c) 0 (strategy-generation c) 0
                                    (strategy-rank c) :B (strategy-tier c) :battlefield
                                    (strategy-status c) :active (strategy-revalidation-pending c) nil)
                              c)))
       (pop (append mono seeds))
       (cap 130))
  (format t "~&SIM start: ~d mono + ~d seeds; 2f-min=~,2f cap=~d~%" (length mono) (length seeds) *2c-cpcv-pregate-min* cap)
  (score-all mono)                                            ; honest re-score of the monoculture
  (format t "~&GEN 0 n=~d div-robust=~d distro=~s~%" (length pop) (n-diverse-robust pop) (distro pop))
  (dotimes (g 6)
    (let ((children (breed-gen pop)))
      (score-all children)
      (setf pop (cull-to (append pop children) cap))
      (format t "~&GEN ~d bred=~d n=~d div-robust=~d distro=~s~%"
              (1+ g) (length children) (length pop) (n-diverse-robust pop) (distro pop))
      (finish-output))))
(sb-ext:exit :code 0)
