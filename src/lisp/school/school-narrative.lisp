;;; school-narrative.lisp - Narrative Generation & Discord Messaging
;;; Part of the Swimmy School System
;;; Extracted from school.lisp to comply with SRP (Expert Panel 2026-01-13)

(in-package :swimmy.school)

;; Generate dynamic narrative with actual values
(defun generate-dynamic-narrative (strat-signal symbol price)
  "Generate natural language explanation"
  (let* ((name (getf strat-signal :strategy-name))
         (direction (getf strat-signal :direction))
         (category (getf strat-signal :category))
         (ind-vals (getf strat-signal :indicator-values))
         ;; V5.1: Default SL/TP when strategy has nil
         (sl (or (getf strat-signal :sl) 0.15))  ; Default 15 pips
         (tp (or (getf strat-signal :tp) 0.40))  ; Default 40 pips
         (category-label (get-category-display category)))
    (format nil "
═══════════════════════════════
📌 【~a】が戦場に立つ！
═══════════════════════════════

📊 発動戦略: ~a

~{~a~^~%~}

📍 ~a @ ~,3f (🕐 ~a)
~a

🎯 利確: +~d pips | 🛡️ 損切: -~d pips

💪 この条件で行く。
═══════════════════════════════~a"
            category-label
            name
            (mapcar (lambda (iv) (format nil "• ~a = ~,2f" (first iv) (second iv))) ind-vals)
            symbol price 
             (swimmy.core:get-jst-timestamp)
             (if (eq direction :buy) "🟢 BUY - 上昇を狙う" "🔴 SELL - 下落を狙う")
             (round (* 100 tp)) (round (* 100 sl))
             (get-category-positions-summary))))


(defun generate-trade-result-narrative (symbol direction pnl pnl-currency entry-price exit-price lot strategy duration-seconds category)
  "Generate natural language explanation for trade RESULT (Win/Loss)"
  (declare (ignore symbol direction))
  (let* (;; Actually user asked for "利益率" (Profit Rate).
         ;; Pips based? Or Money/Margin?
         ;; For simplicty and robustness, let's show Pips and Raw Amount first.
         ;; "利益率" usually means PnL / Margin. Since Margin is dynamic, let's use PnL/Capital risk or just show Pips as primary "Rate".
         ;; Let's try to calculate ROI if possible. Margin ~ Price * Lot * 100000 / Leverage(25).
         ;; Margin = (Entry * Lot * 100000) / 25
         (leverage 25)
         (margin (if (> entry-price 0) (/ (* entry-price lot 100000) leverage) 0)) ;; Return 0 if invalid
         (roi-percent (if (> margin 0) (* 100 (/ pnl-currency margin)) 0.0))
         (category-label (get-category-display category))
         (win-p (> pnl 0))
         (pips pnl))

    (format nil "
═══════════════════════════════
📌 【~a】 ~a
═══════════════════════════════
~a
📈 戦略: **~a** (~a)
🏷️ カテゴリ: ~a

💴 PnL: **~,0@f JPY** (~,1@f pips)
📊 ROI: **~,2@f%**

⏱️ Time:
  Entry: ~a (@ ~,3f)
  Exit : ~a (@ ~,3f)
  拘束: ~a

💪 ~a
═══════════════════════════════"
            category-label
            (if win-p "凱旋！(WIN)" "戦死... (LOSS)")
            (if win-p "🎉 勝鬨を上げよ！" "💀 屍を越えてゆけ...")
            strategy category
            category-label
            pnl-currency
            pips
            roi-percent
            (format-timestamp (- (get-universal-time) duration-seconds)) entry-price
            (format-timestamp (get-universal-time)) exit-price
            (format-duration duration-seconds)
            (if win-p "ナイス・トレード。" "次、取り返そう。"))))

(defun format-timestamp (u-time)
  "Format timestamp showing both JST (local) and UTC for MT5 cross-reference"
  (multiple-value-bind (s m h d mo y) (decode-universal-time u-time)
    (declare (ignore s y))
    ;; JST is UTC+9, so calculate UTC by subtracting 9 hours
    (multiple-value-bind (us um uh ud umo uy) (decode-universal-time u-time 0) ; 0 = UTC
      (declare (ignore us ud umo uy))
      (format nil "~2,'0d/~2,'0d ~2,'0d:~2,'0d JST / ~2,'0d:~2,'0d UTC" 
              mo d h m uh um))))

(defun format-duration (seconds)
  (let* ((days (floor seconds 86400))
         (hours (floor (mod seconds 86400) 3600))
         (mins (floor (mod seconds 3600) 60)))
    (cond
      ((> days 0) (format nil "~dd ~dh ~dm" days hours mins))
      ((> hours 0) (format nil "~dh ~dm" hours mins))
      (t (format nil "~dm" mins)))))

(defun get-category-positions-summary ()
  "Generate a compact summary of active positions for all categories"
  (if (hash-table-p *warrior-allocation*)
      (let ((trends nil) (breakouts nil) (scalps nil) (reversions nil))
        
        ;; Aggregate positions
        (maphash (lambda (k v)
                   (declare (ignore k))
                   (when v
                     (let ((sym (getf v :symbol))
                           (cat (getf v :category)))
                      (case cat
                        (:trend (pushnew sym trends :test #'string=))
                        (:breakout (pushnew sym breakouts :test #'string=))
                        (:scalp (pushnew sym scalps :test #'string=))
                        (:reversion (pushnew sym reversions :test #'string=)))))) 
                 *warrior-allocation*)
        
        ;; Format Text
        (format nil "
📊 **Active Categories**:
TREND     : ~a
BREAKOUT  : ~a
SCALP     : ~a
REVERSION : ~a"
                (if trends (format nil "~{~a~^, ~}" trends) "-")
                (if breakouts (format nil "~{~a~^, ~}" breakouts) "-")
                (if scalps (format nil "~{~a~^, ~}" scalps) "-")
                (if reversions (format nil "~{~a~^, ~}" reversions) "-")))
      ""))


(defun candidate-rank-label (strategy)
  "Generate a human-friendly rank status label for reports."
  (let* ((rank (strategy-rank strategy))
         (a-criteria (get-rank-criteria :A))
         (a-sharpe-min (getf a-criteria :sharpe-min 0.45))
         (s-eligible (check-rank-criteria strategy :S))
         (s-base (check-rank-criteria strategy :S :include-cpcv nil))
         (a-eligible (check-rank-criteria strategy :A))
         (a-base (check-rank-criteria strategy :A :include-oos nil))
         (sharpe (or (strategy-sharpe strategy) 0.0))
         (cpcv (or (strategy-cpcv-median-sharpe strategy) 0.0))
         (cpcv-pass (or (strategy-cpcv-pass-rate strategy) 0.0))
         (oos (or (strategy-oos-sharpe strategy) 0.0)))
    (cond
      ((eq rank :S) "S")
      (s-eligible (if (eq rank :A) "A: S-ELIGIBLE" "S: PROMOTION PENDING"))
      ((and (eq rank :A) s-base)
       (format nil "CPCV PENDING (median=~,2f pass=~,0f%%)" cpcv (* 100 cpcv-pass)))
      ((and a-eligible (eq rank :B)) "A: PROMOTION PENDING")
      ((and (eq rank :B) (>= sharpe a-sharpe-min) (not a-base)) "A: BASE METRICS FAIL")
      ((and a-eligible (eq rank :A)) "A")
      ((and (eq rank :B) a-base)
       (format nil "OOS PENDING (OOS=~,2f)" oos))
      (rank (symbol-name rank))
      (t "UNRANKED"))))

(defun %display-candidate-name (name &key (max-len 25))
  "Display-friendly strategy name.
   Keep both prefix and suffix when truncating to avoid same-prefix collisions."
  (let* ((safe (or name ""))
         (n (length safe)))
    (cond
      ((<= n max-len) safe)
      ((<= max-len 10) (subseq safe 0 max-len))
      (t
       (let* ((tail-len 3)
              (hash-suffix (format nil "~3,'0X" (mod (sxhash safe) #x1000)))
              (head-len (max 1 (- max-len (+ tail-len 2 (length hash-suffix)))))
              (head (subseq safe 0 head-len))
              (tail (subseq safe (- n tail-len))))
         (format nil "~a..~a~a" head tail hash-suffix))))))

(defun build-top-candidates-snippet (strategies)
  "Build top candidates snippet with fault isolation."
  (handler-case
      (let* ((sorted (sort (copy-list strategies) #'> :key (lambda (s) (or (strategy-sharpe s) -1.0))))
             (limit (min (length sorted) 5)))
        (with-output-to-string (s)
          (format s "~%🌟 **Top Candidates:**~%")
          (loop for i from 0 below limit
                for st = (nth i sorted)
                for label = (candidate-rank-label st)
                do (format s "- `~a` (S=~,2f, ~a)~%"
                           (%display-candidate-name (strategy-name st))
                           (or (strategy-sharpe st) 0.0)
                           label))))
    (error (e)
      (format nil "~%🌟 **Top Candidates:**~%  - error: ~a" e))))

(defun %format-db-rank-label (rank)
  "Normalize DB rank text into a human-facing label.
   DB stores ranks as strings like \":B\" or \"NIL\"; never display NIL."
  (labels ((normalize (s)
             (let* ((trimmed (string-upcase (string-trim '(#\Space #\Newline #\Tab) s)))
                    (no-colon (if (and (> (length trimmed) 0) (char= (char trimmed 0) #\:))
                                  (subseq trimmed 1)
                                  trimmed)))
               no-colon)))
    (let* ((raw (cond
                  ((null rank) nil)
                  ((stringp rank) rank)
                  ((symbolp rank) (symbol-name rank))
                  (t (format nil "~a" rank))))
           (r (and raw (normalize raw))))
      (cond
        ((or (null r) (string= r "") (string= r "NIL") (string= r "UNRANKED")) "INCUBATOR")
        (t r)))))

(defun build-top-candidates-snippet-from-db ()
  "Build top candidates snippet using DB as source of truth."
  (handler-case
      ;; Show only active candidates (exclude GRAVEYARD/RETIRED). Rank NIL is treated as INCUBATOR for display.
      (let* ((rows (execute-to-list
                    (concatenate 'string
                                 "SELECT name, sharpe, rank "
                                 "FROM strategies "
                                 "WHERE rank IS NULL OR (UPPER(rank) NOT IN (':GRAVEYARD','GRAVEYARD',':RETIRED','RETIRED')) "
                                 "ORDER BY sharpe DESC LIMIT 5")))
             (limit (length rows)))
        (with-output-to-string (s)
          (format s "~%🌟 **Top Candidates:**~%")
          (loop for i from 0 below limit
                for row = (nth i rows)
                do (destructuring-bind (name sharpe rank) row
                     (let* ((safe-name (or name ""))
                            (label (%format-db-rank-label rank)))
                       (format s "- `~a` (S=~,2f, ~a)~%"
                               (%display-candidate-name safe-name)
                               (float (or sharpe 0.0))
                               label))))))
    (error (e)
      (format nil "~%🌟 **Top Candidates:**~%  - error: ~a" e))))

(defun %a-stage1-failed-gates (strategy)
  "Return failing A Stage1 gate labels for STRATEGY."
  (let* ((criteria (get-rank-criteria :A))
         (sh-min (getf criteria :sharpe-min 0.45))
         (pf-min (getf criteria :pf-min 1.30))
         (wr-min (getf criteria :wr-min 0.43))
         (dd-max (getf criteria :maxdd-max 0.16))
         (sharpe (or (strategy-sharpe strategy) 0.0))
         (pf (or (strategy-profit-factor strategy) 0.0))
         (wr (or (strategy-win-rate strategy) 0.0))
         (maxdd (or (strategy-max-dd strategy) 1.0))
         (failed '()))
    (when (< sharpe sh-min) (push "SHARPE" failed))
    (when (< pf pf-min) (push "PF" failed))
    (when (< wr wr-min) (push "WR" failed))
    (when (>= maxdd dd-max) (push "MAXDD" failed))
    (nreverse failed)))

(defun %a-base-deficit-safe (strategy)
  "Best-effort A-base deficit score. Returns a large value when unavailable."
  (if (fboundp 'a-base-deficit-score)
      (float (or (a-base-deficit-score strategy) 0.0) 1.0)
      999.0))

(defun %dedupe-strategies-by-name (strategies)
  "Return STRATEGIES with duplicate strategy-name entries removed.
   Keep the first occurrence, so pre-sorted lists preserve their best candidate."
  (labels ((normalize (name)
             (string-upcase
              (string-trim '(#\Space #\Tab #\Newline #\Return)
                           (or name "")))))
    (let ((seen (make-hash-table :test 'equal))
          (unique '()))
      (dolist (st strategies (nreverse unique))
        (let ((name (normalize (and st (strategy-name st)))))
          (unless (gethash name seen)
            (setf (gethash name seen) t)
            (push st unique)))))))

(defun build-a-near-miss-snippet-from-db (&key (limit 5))
  "Build A near-miss snippet from B-rank candidates closest to A Stage1."
  (handler-case
      (let* ((b-candidates (or (ignore-errors (fetch-candidate-strategies :min-sharpe 0.0 :ranks '(":B")))
                               '()))
             (sorted (sort (copy-list b-candidates) #'< :key #'%a-base-deficit-safe))
             (unique (if sorted
                         (%dedupe-strategies-by-name sorted)
                         '()))
             (top (subseq unique 0 (min limit (length unique)))))
        (if (null top)
            "A Near-Miss Candidates (B): none"
            (with-output-to-string (s)
              (format s "A Near-Miss Candidates (B):~%")
              (dolist (st top)
                (let* ((name (or (strategy-name st) "UNKNOWN"))
                       (fails (%a-stage1-failed-gates st))
                       (fail-text (if fails
                                      (format nil "~{~a~^/~}" fails)
                                      "NONE"))
                       (deficit (%a-base-deficit-safe st))
                       (sharpe (float (or (strategy-sharpe st) 0.0) 1.0))
                       (pf (float (or (strategy-profit-factor st) 0.0) 1.0))
                       (wr (* 100.0 (float (or (strategy-win-rate st) 0.0) 1.0)))
                       (wr-pct (round wr))
                       (dd (* 100.0 (float (or (strategy-max-dd st) 0.0) 1.0))))
                  (format s "- `~a` deficit=~,3f fails=~a | S=~,2f PF=~,2f WR=~d% DD=~,1f%~%"
                          (%display-candidate-name name)
                          deficit fail-text sharpe pf wr-pct dd))))))
    (error (e)
      (format nil "A Near-Miss Candidates (B): error: ~a" e))))

(defun build-cpcv-status-snippet ()
  "Build CPCV status snippet for reports."
  ;; CPCV counters and start-time are split across daemons (dispatch vs result recv).
  ;; Use the shared status file as source-of-truth to avoid "sent=0/last start=N/A" confusion.
  (let* ((path (and (boundp 'swimmy.school::*cpcv-status-path*) swimmy.school::*cpcv-status-path*))
         (lines (when (and path (probe-file path))
                  (with-open-file (in path :direction :input)
                    (loop for line = (read-line in nil nil)
                          while line collect line))))
         (summary (or (and lines (first lines))
                      (cpcv-metrics-summary-line)))
         (last-start-unix
           (when lines
             (loop for l in lines
                   for prefix = "last_start_unix:"
                   when (and (>= (length l) (length prefix))
                             (string-equal prefix (subseq l 0 (length prefix))))
                     do (let* ((tail (string-trim '(#\Space #\Tab) (subseq l (length prefix))))
                               (n (ignore-errors (parse-integer tail))))
                          (when (and n (> n 0)) (return n))))))
         (updated-reason
           (when lines
             (loop for l in lines
                   for marker = "reason:"
                   for pos = (search marker l :test #'char-equal)
                   when pos
                     do (let ((tail (string-trim '(#\Space #\Tab)
                                                 (subseq l (+ pos (length marker))))))
                          (when (> (length tail) 0)
                            (return tail))))))
         (start-time (or last-start-unix (and (boundp 'swimmy.globals:*cpcv-start-time*)
                                              swimmy.globals:*cpcv-start-time*) 0))
         (start-text (if (and start-time (> start-time 0))
                         (format-timestamp start-time)
                         "N/A")))
    (format nil "🔬 CPCV Status~%~a | last start: ~a~@[ | reason: ~a~]"
            summary start-text updated-reason)))

(defparameter *evolution-report-path* "data/reports/evolution_factory_report.txt")
(defparameter *evolution-heartbeat-path* "data/heartbeat/school.tick")
(defparameter *evolution-report-interval* (* 60 60))
(defparameter *evolution-report-stale-threshold* (* 2 60 60))
(defparameter *evolution-report-alert-interval* (* 60 60))
(defparameter *last-evolution-report-alert-time* 0)
(defparameter *promotion-report-sync-interval* 120
  "Minimum seconds between promotion-triggered evolution report syncs.")
(defparameter *last-promotion-report-sync-time* 0)

(defun safe-file-write-date (path)
  (or (ignore-errors (file-write-date path)) 0))

(defun %systemd-service-state (service)
  "Return normalized systemd is-active state for SERVICE, or \"unknown\"."
  (let* ((raw (ignore-errors
                (uiop:run-program (list "systemctl" "is-active" service)
                                  :output :string
                                  :ignore-error-status t)))
         (trimmed (if raw
                      (string-downcase
                       (string-trim '(#\Space #\Tab #\Newline #\Return) raw))
                      "")))
    (if (> (length trimmed) 0) trimmed "unknown")))

(defun %evolution-heartbeat-age (&key (now (get-universal-time)))
  "Return age seconds for evolution heartbeat, or NIL when missing."
  (let ((last (safe-file-write-date *evolution-heartbeat-path*)))
    (when (> last 0)
      (max 0 (- now last)))))

(defun evolution-daemon-status-line (&key (now (get-universal-time)))
  "Human-readable evolution daemon status for reports."
  (let* ((state (%systemd-service-state "swimmy-evolution.service"))
         (heartbeat-age (%evolution-heartbeat-age :now now))
         (heartbeat-fresh-p (and heartbeat-age
                                 (<= heartbeat-age *evolution-report-stale-threshold*))))
    (cond
      ((string= state "active") "✅ Evolution Daemon Active")
      ((string= state "activating") "🟡 Evolution Daemon Activating")
      ((string= state "deactivating") "🟡 Evolution Daemon Deactivating")
      ((string= state "inactive")
       (if heartbeat-fresh-p
           (format nil "⚠️ Evolution Daemon Inactive (heartbeat ~ds ago)" heartbeat-age)
           "⚠️ Evolution Daemon Inactive"))
      ((string= state "failed")
       (if heartbeat-fresh-p
           (format nil "❌ Evolution Daemon Failed (heartbeat ~ds ago)" heartbeat-age)
           "❌ Evolution Daemon Failed"))
      (heartbeat-fresh-p
       (format nil "⚠️ Evolution Daemon Status Unknown (heartbeat ~ds ago)" heartbeat-age))
      (t "⚠️ Evolution Daemon Status Unknown"))))

(defun maybe-send-evolution-report (&key (now (get-universal-time)) last-write (reason "scheduled"))
  "Send evolution report if the last write exceeds the configured interval."
  (let* ((last (or last-write (safe-file-write-date *evolution-report-path*)))
         (age (- now last)))
    (when (> age *evolution-report-interval*)
      (format t "[REPORT] 📨 Sending Evolution Report (~a)...~%" reason)
      (notify-evolution-report)
      (when (fboundp 'write-oos-status-file)
        (ignore-errors (write-oos-status-file :reason reason)))
      t)))

(defun maybe-sync-evolution-report-on-promotion (&key rank reason (now (get-universal-time)))
  "Sync evolution report shortly after A/S promotions (throttled)."
  (declare (ignore reason))
  (when (and (member rank '(:A :S) :test #'eq)
             (> (- now *last-promotion-report-sync-time*)
                *promotion-report-sync-interval*))
    (setf *last-promotion-report-sync-time* now)
    (format t "[REPORT] 🔄 Promotion sync triggered (rank=~a)~%" rank)
    (notify-evolution-report)
    t))

(defun maybe-alert-evolution-report-staleness
    (&key (now (get-universal-time)) last-report last-heartbeat)
  "Alert if the report or heartbeat file is stale. Throttled by alert interval."
  (let* ((report-last (or last-report (safe-file-write-date *evolution-report-path*)))
         (heartbeat-last (or last-heartbeat (safe-file-write-date *evolution-heartbeat-path*)))
         (report-age (if (> report-last 0) (- now report-last) nil))
         (heartbeat-age (if (> heartbeat-last 0) (- now heartbeat-last) nil))
         (report-stale (or (null report-age) (> report-age *evolution-report-stale-threshold*)))
         (heartbeat-stale (or (null heartbeat-age) (> heartbeat-age *evolution-report-stale-threshold*)))
         (cooldown-ok (> (- now *last-evolution-report-alert-time*)
                         *evolution-report-alert-interval*)))
    (when (and cooldown-ok (or report-stale heartbeat-stale))
      (setf *last-evolution-report-alert-time* now)
      (let ((msg (format nil "⚠️ Evolution report/heartbeat stale. report_age=~a heartbeat_age=~a"
                         (or report-age "MISSING") (or heartbeat-age "MISSING"))))
        (swimmy.core:notify-discord-alert msg)
        (when (fboundp 'swimmy.core::emit-telemetry-event)
          (swimmy.core::emit-telemetry-event "evolution.report.stale"
                                             :service "school"
                                             :severity "warn"
                                             :data (list :report_age report-age
                                                         :heartbeat_age heartbeat-age)))))))

(defun generate-evolution-report ()
  "Generate the Evolution Factory Report (formerly Python).
   Answers User Q1: S-Rank = Battlefield (Veteran), A-Rank = Training."
  ;; V50.x Fix: Force sync metrics from DB to ensure Report is accurate (User Req)
  (refresh-strategy-metrics-from-db :force t)
  
  (let* ((all swimmy.globals:*strategy-knowledge-base*)
         (counts (get-db-rank-counts))
         (active-count (getf counts :active 0))
         (s-rank (getf counts :s 0))
         (a-rank (getf counts :a 0))
         (b-rank (getf counts :b 0)) ; Selection
         (graveyard (getf counts :graveyard 0))
         (retired (getf counts :retired 0))
         (lib-counts (ignore-errors (get-library-rank-counts)))
         (lib-graveyard (and lib-counts (getf lib-counts :graveyard 0)))
         (lib-retired (and lib-counts (getf lib-counts :retired 0)))
         (drift-warnings (ignore-errors (report-source-drift)))
         ;; New Recruits (24h) - using new creation-time slot (P13)
         (one-day-ago (- (get-universal-time) 86400))
         (new-recruits (count-if (lambda (s)
                                   (and (strategy-creation-time s)
                                        (> (strategy-creation-time s) one-day-ago)))
                                 all)))
    (let* ((top-snippet (build-top-candidates-snippet-from-db))
           (a-rank-db (or (ignore-errors (fetch-candidate-strategies :min-sharpe 0.0 :ranks '(":A")))
                          '()))
           (cpcv-gate-counts (cpcv-gate-failure-counts a-rank-db))
           (cpcv-gate-line (format nil "CPCV Gate Failures: sharpe<~,2f=~d pf<~,2f=~d wr<~,2f=~d maxdd>=~,2f=~d elite=~d total=~d"
                                   (getf cpcv-gate-counts :sharpe-min 0.75)
                                   (getf cpcv-gate-counts :sharpe 0)
                                   (getf cpcv-gate-counts :pf-min 1.70)
                                   (getf cpcv-gate-counts :pf 0)
                                   (getf cpcv-gate-counts :wr-min 0.50)
                                   (getf cpcv-gate-counts :wr 0)
                                   (getf cpcv-gate-counts :maxdd-max 0.10)
                                   (getf cpcv-gate-counts :maxdd 0)
                                   (getf cpcv-gate-counts :pass 0)
                                   (getf cpcv-gate-counts :total 0)))
           (cpcv-median-counts (cpcv-median-failure-counts a-rank-db))
           (cpcv-pass-min-pct (round (* 100 (getf cpcv-median-counts :pass-min 0.70))))
           (cpcv-median-line (format nil "CPCV Stage2 Failures: pass_rate<~d%=~d maxdd>=~,2f=~d total=~d"
                                     cpcv-pass-min-pct
                                     (getf cpcv-median-counts :pass-rate 0)
                                     (getf cpcv-median-counts :maxdd-max 0.12)
                                     (getf cpcv-median-counts :maxdd 0)
                                     (getf cpcv-median-counts :total 0)))
           (cpcv-snippet (format nil "~a~%~a~%~a"
                                 (build-cpcv-status-snippet)
                                 cpcv-gate-line
                                 cpcv-median-line))
           (oos-snippet (oos-metrics-summary-line))
           (validation-coverage-line (validation-coverage-summary-line))
           (a-stage1-counts
             (or (ignore-errors (a-stage1-failure-counts-from-db))
                 (ignore-errors (a-stage1-failure-counts all))))
           (a-stage1-snippet
             (if a-stage1-counts
                 (a-stage1-failure-summary-line a-stage1-counts :label "A Stage1 Failures (24h DB)")
                 "A Stage1 Failures (24h DB): unavailable"))
           (a-gate-pressure-line (a-gate-pressure-active-b-summary-line))
           (a-near-miss-snippet (build-a-near-miss-snippet-from-db :limit 5))
           (a-funnel-snippet (if (fboundp 'a-candidate-metrics-snippet)
                                 (a-candidate-metrics-snippet :limit 6)
                                 "A Candidate Funnel (latest): unavailable"))
           (daemon-status-line (evolution-daemon-status-line)))
      (let* ((graveyard-text (if lib-counts
                                 (format nil "~d (Library ~d)" graveyard lib-graveyard)
                                 (format nil "~d" graveyard)))
             (retired-text (if lib-counts
                               (format nil "~d (Library ~d)" retired lib-retired)
                               (format nil "~d" retired)))
             (s-min-trades (if (boundp '*s-rank-min-trade-evidence*)
                               *s-rank-min-trade-evidence*
                               0))
             (a-min-trades (if (boundp '*a-rank-min-trade-evidence*)
                               *a-rank-min-trade-evidence*
                               0))
             (a-mc-mode (if (and (boundp '*a-rank-require-mc*)
                                 *a-rank-require-mc*)
                            "req"
                            "opt"))
             (a-dryrun-mode (if (and (boundp '*a-rank-require-dryrun*)
                                     *a-rank-require-dryrun*)
                                "req"
                                "opt"))
             (drift-text (if (and drift-warnings (not (null drift-warnings)))
                             (with-output-to-string (s)
                               (format s "~%📎 **Source Drift:**~%")
                               (dolist (w drift-warnings)
                                 (format s " - ~a~%" w)))
                             "")))
    
        (format nil "
🏭 **Evolution Factory Report**
Current status of the autonomous strategy generation pipeline.

	🧠 Knowledge Base (Active)
	~d Strategies

	🏆 **S-Rank (Verified Elite)**
	~d (IS Sharpe≥0.75 PF≥1.70 WR≥50% MaxDD<10% + CPCV pass_rate≥70% & median MaxDD<12% + TradeEvidence≥~d + CommonStage2(MC req / DryRun req))

	🎖️ **A-Rank (Pro)**
	~d (Sharpe≥0.45 PF≥1.30 WR≥43% MaxDD<16% + OOS≥0.35 + Expectancy>0 + TradeEvidence≥~d + Stage2(MC ~a / DryRun ~a))

🪜 **B-Rank (Selection)**
~d (Sharpe≥0.15 PF≥1.05 WR≥35% MaxDD<25%)

👶 New Recruits (24h)
~d

👻 Graveyard
~a

🧊 Retired
~a
~a

	~a

	~a

	~a

	~a

	~a

	~a

	~a

	~a

	⚙️ System Status
	~a
	✅ Native Lisp Orchestration (V28)
~a"
	            active-count
	            s-rank
	            s-min-trades
	            a-rank
	            a-min-trades
	            a-mc-mode
	            a-dryrun-mode
	            b-rank
	            new-recruits
            graveyard-text
	            retired-text
	            drift-text
	            cpcv-snippet
	            oos-snippet
	            validation-coverage-line
	            a-stage1-snippet
	            a-gate-pressure-line
	            a-near-miss-snippet
	            a-funnel-snippet
	            top-snippet
              daemon-status-line
	            (format-timestamp (get-universal-time)))))))

(defun write-evolution-report-files (report)
  "Persist the Evolution Factory Report to local files."
  (let ((path (or *evolution-report-path* "data/reports/evolution_factory_report.txt")))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string report stream))))

(defun send-evolution-report (report &optional webhook)
  "Send the Evolution Factory Report to Discord."
  (let ((final-webhook (or webhook swimmy.core:*discord-daily-webhook* swimmy.globals:*discord-webhook-url*)))
    (if final-webhook
        (swimmy.core:queue-discord-notification 
         final-webhook
         report 
         :color 3447003 
         :title "🏭 Evolution Factory Report")
        (format t "[REPORT] ⚠️ Discord webhook missing; report saved locally only.~%"))))

(defun notify-evolution-report ()
  "Send the Evolution Factory Report to Discord AND save to file."
  (let ((report (generate-evolution-report)))
    (write-evolution-report-files report)
    (send-evolution-report report)
    (when (fboundp 'write-oos-status-file)
      (ignore-errors (write-oos-status-file :reason "report")))))

(defun validation-coverage-summary-line ()
  "Return DB cumulative validation coverage for OOS/CPCV."
  (handler-case
      (let* ((all-oos
               (or (execute-single
                    "SELECT count(*) FROM strategies
                      WHERE ABS(COALESCE(oos_sharpe, 0.0)) > 1e-6")
                   0))
             (all-cpcv
               (or (execute-single
                    "SELECT count(*) FROM strategies
                      WHERE ABS(COALESCE(cpcv_median, 0.0)) > 1e-6
                         OR ABS(COALESCE(cpcv_pass_rate, 0.0)) > 1e-6")
                   0))
             (active-oos
               (or (execute-single
                    "SELECT count(*) FROM strategies
                      WHERE (rank IS NULL OR UPPER(rank) NOT IN (':GRAVEYARD','GRAVEYARD',':RETIRED','RETIRED'))
                        AND ABS(COALESCE(oos_sharpe, 0.0)) > 1e-6")
                   0))
             (active-cpcv
               (or (execute-single
                    "SELECT count(*) FROM strategies
                      WHERE (rank IS NULL OR UPPER(rank) NOT IN (':GRAVEYARD','GRAVEYARD',':RETIRED','RETIRED'))
                        AND (ABS(COALESCE(cpcv_median, 0.0)) > 1e-6
                             OR ABS(COALESCE(cpcv_pass_rate, 0.0)) > 1e-6)")
                   0)))
        (format nil "Validation Coverage (DB): OOS done=~d CPCV done=~d | Active OOS=~d CPCV=~d"
                all-oos all-cpcv active-oos active-cpcv))
    (error (e)
      (format nil "Validation Coverage (DB): unavailable (~a)" e))))

(defun a-gate-pressure-active-b-summary-line ()
  "Return A Stage1 gate pressure summary for Active B candidates."
  (handler-case
      (let* ((criteria (get-rank-criteria :A))
             (sh-min (float (getf criteria :sharpe-min 0.45) 1.0))
             (pf-min (float (getf criteria :pf-min 1.30) 1.0))
             (wr-min (float (getf criteria :wr-min 0.43) 1.0))
             (dd-max (float (getf criteria :maxdd-max 0.16) 1.0))
             ;; "near" band highlights candidates close to PF gate.
             (pf-near-min (max 0.0 (- pf-min 0.06)))
             (b-rank-where "UPPER(rank) IN (':B','B')")
             (total
               (or (execute-single
                    (format nil "SELECT count(*) FROM strategies WHERE ~a" b-rank-where))
                   0))
             (pass-all
               (or (execute-single
                    (format nil
                            "SELECT count(*) FROM strategies
                              WHERE ~a
                                AND COALESCE(sharpe, 0.0) >= ?
                                AND COALESCE(profit_factor, 0.0) >= ?
                                AND COALESCE(win_rate, 0.0) >= ?
                                AND COALESCE(max_dd, 1.0) < ?"
                            b-rank-where)
                    sh-min pf-min wr-min dd-max)
                   0))
             (pass-sharpe
               (or (execute-single
                    (format nil
                            "SELECT count(*) FROM strategies
                              WHERE ~a
                                AND COALESCE(sharpe, 0.0) >= ?"
                            b-rank-where)
                    sh-min)
                   0))
             (pass-pf
               (or (execute-single
                    (format nil
                            "SELECT count(*) FROM strategies
                              WHERE ~a
                                AND COALESCE(profit_factor, 0.0) >= ?"
                            b-rank-where)
                    pf-min)
                   0))
             (pass-wr
               (or (execute-single
                    (format nil
                            "SELECT count(*) FROM strategies
                              WHERE ~a
                                AND COALESCE(win_rate, 0.0) >= ?"
                            b-rank-where)
                    wr-min)
                   0))
             (pass-maxdd
               (or (execute-single
                    (format nil
                            "SELECT count(*) FROM strategies
                              WHERE ~a
                                AND COALESCE(max_dd, 1.0) < ?"
                            b-rank-where)
                    dd-max)
                   0))
             (pf-near
               (or (execute-single
                    (format nil
                            "SELECT count(*) FROM strategies
                              WHERE ~a
                                AND COALESCE(sharpe, 0.0) >= ?
                                AND COALESCE(win_rate, 0.0) >= ?
                                AND COALESCE(max_dd, 1.0) < ?
                                AND COALESCE(profit_factor, 0.0) >= ?
                                AND COALESCE(profit_factor, 0.0) < ?"
                            b-rank-where)
                    sh-min wr-min dd-max pf-near-min pf-min)
                   0)))
        (format nil
                "A Gate Pressure (Active B): total=~d pass_all=~d | pass sharpe=~d pf=~d wr=~d maxdd=~d | pf_near[~,2f,~,2f)=~d"
                total pass-all pass-sharpe pass-pf pass-wr pass-maxdd
                pf-near-min pf-min pf-near))
    (error (e)
      (format nil "A Gate Pressure (Active B): unavailable (~a)" e))))

(defun oos-metrics-summary-line ()
  "Human-readable summary of OOS pipeline health for reports/Discord."
  (labels ((fmt-latency (value)
             (if (numberp value)
                 (format nil "~,2f" (float value))
                 "-")))
    (let* ((m (report-oos-db-metrics))
           (q (fetch-oos-queue-stats))
           (fail (if (fboundp 'report-oos-failure-stats)
                     (report-oos-failure-stats)
                     nil))
           (lat (if (fboundp 'report-oos-metrics)
                    (report-oos-metrics)
                    nil))
           (sent (getf m :sent 0))
           (retry (getf m :retry 0))
           (success (getf m :success 0))
           (failure (getf m :failure 0))
           (pending (getf q :pending 0))
           (age (getf q :oldest-age))
           (age-text (if age (format nil "~ds" age) "-"))
           (data-invalid (getf fail :data-invalid 0))
           (send-failure (getf fail :send-failure 0))
           (db-error (getf fail :db-error 0))
           (lat-avg (if (numberp (getf lat :latency-avg nil))
                        (format nil "~,2f" (float (getf lat :latency-avg 0.0)))
                        "0.00"))
           (lat-min (fmt-latency (getf lat :latency-min nil)))
           (lat-max (fmt-latency (getf lat :latency-max nil))))
      (format nil "OOS sent: ~d retry: ~d success: ~d failure: ~d pending: ~d oldest: ~a (data ~d send ~d db ~d) latency(avg/min/max): ~a/~a/~a sec"
              sent retry success failure pending age-text
              data-invalid send-failure db-error
              lat-avg lat-min lat-max))))
