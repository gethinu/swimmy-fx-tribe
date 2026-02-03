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
         (clan (get-clan category)))
    (format nil "
═══════════════════════════════
~a 【~a】が戦場に立つ！
═══════════════════════════════

📊 発動戦略: ~a

~{~a~^~%~}

📍 ~a @ ~,3f (🕐 ~a)
~a

🎯 利確: +~d pips | 🛡️ 損切: -~d pips

💪 この条件で行く。
═══════════════════════════════~a"
            (if clan (clan-emoji clan) "🏛️") 
            (if clan (clan-name clan) "Unknown")
            name
            (mapcar (lambda (iv) (format nil "• ~a = ~,2f" (first iv) (second iv))) ind-vals)
            symbol price 
             (swimmy.core:get-jst-timestamp)
             (if (eq direction :buy) "🟢 BUY - 上昇を狙う" "🔴 SELL - 下落を狙う")
             (round (* 100 tp)) (round (* 100 sl))
             (get-clan-positions-summary))))


(defun generate-trade-result-narrative (symbol direction pnl pnl-currency entry-price exit-price lot strategy duration-seconds category)
  "Generate natural language explanation for trade RESULT (Win/Loss)"
  (declare (ignore symbol direction))
  (let* ((clan (get-clan category))
         ;; Actually user asked for "利益率" (Profit Rate).
         ;; Pips based? Or Money/Margin?
         ;; For simplicty and robustness, let's show Pips and Raw Amount first.
         ;; "利益率" usually means PnL / Margin. Since Margin is dynamic, let's use PnL/Capital risk or just show Pips as primary "Rate".
         ;; Let's try to calculate ROI if possible. Margin ~ Price * Lot * 100000 / Leverage(25).
         ;; Margin = (Entry * Lot * 100000) / 25
         (leverage 25)
         (margin (if (> entry-price 0) (/ (* entry-price lot 100000) leverage) 0)) ;; Return 0 if invalid
         (roi-percent (if (> margin 0) (* 100 (/ pnl-currency margin)) 0.0))
         (clan-emoji (if clan (clan-emoji clan) "🏛️"))
         (clan-name (if clan (clan-name clan) "Unknown"))
         (win-p (> pnl 0))
         (pips pnl))

    (format nil "
═══════════════════════════════
~a 【~a】 ~a
═══════════════════════════════
~a
📈 戦略: **~a** (~a)
🏳️ 部族: ~a ~a

💴 PnL: **~,0@f JPY** (~,1@f pips)
📊 ROI: **~,2@f%**

⏱️ Time:
  Entry: ~a (@ ~,3f)
  Exit : ~a (@ ~,3f)
  拘束: ~a

💪 ~a
═══════════════════════════════"
            clan-emoji
            clan-name
            (if win-p "凱旋！(WIN)" "戦死... (LOSS)")
            (if win-p "🎉 勝鬨を上げよ！" "💀 屍を越えてゆけ...")
            strategy category
            clan-emoji clan-name
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

(defun get-clan-positions-summary ()
  "Generate a compact summary of active positions for all clans"
  (if (hash-table-p *warrior-allocation*)
      (let ((hunters nil) (breakers nil) (raiders nil) (shamans nil))
        
        ;; Aggregate positions
        (maphash (lambda (k v)
                   (declare (ignore k))
                   (when v
                     (let ((sym (getf v :symbol))
                           (cat (getf v :category)))
                       (case cat
                         (:trend (pushnew sym hunters :test #'string=))
                         (:breakout (pushnew sym breakers :test #'string=))
                         (:scalp (pushnew sym raiders :test #'string=))
                         (:reversion (pushnew sym shamans :test #'string=))
                         (:hunters (pushnew sym hunters :test #'string=))     ; Alias
                         (:breakers (pushnew sym breakers :test #'string=))   ; Alias
                         (:raiders (pushnew sym raiders :test #'string=))     ; Alias
                         (:shamans (pushnew sym shamans :test #'string=)))))) ; Alias
                 *warrior-allocation*)
        
        ;; Format Text
        (format nil "
🏰 **Active Battlefields**:
🏹 Hunters : ~a
⚔️ Breakers: ~a
🗡️ Raiders : ~a
🔮 Shamans : ~a"
                (if hunters (format nil "~{~a~^, ~}" hunters) "-")
                (if breakers (format nil "~{~a~^, ~}" breakers) "-")
                (if raiders (format nil "~{~a~^, ~}" raiders) "-")
                (if shamans (format nil "~{~a~^, ~}" shamans) "-")))
      ""))


(defun candidate-rank-label (strategy)
  "Generate a human-friendly rank status label for reports."
  (let* ((rank (strategy-rank strategy))
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
      ((and (eq rank :B) (>= sharpe 0.3) (not a-base)) "A: BASE METRICS FAIL")
      ((and a-eligible (eq rank :A)) "A")
      ((and (eq rank :B) a-base)
       (format nil "OOS PENDING (OOS=~,2f)" oos))
      (rank (symbol-name rank))
      (t "UNRANKED"))))

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
                           (subseq (strategy-name st) 0 (min 25 (length (strategy-name st))))
                           (or (strategy-sharpe st) 0.0)
                           label))))
    (error (e)
      (format nil "~%🌟 **Top Candidates:**~%  - error: ~a" e))))

(defun build-cpcv-status-snippet ()
  "Build CPCV status snippet for reports."
  (let* ((expected swimmy.globals:*expected-cpcv-count*)
         (received (length (or swimmy.globals:*cpcv-results-buffer* nil)))
         (start-time swimmy.globals:*cpcv-start-time*)
         (start-text (if (> start-time 0) (format-timestamp start-time) "N/A")))
    (format nil "🔬 CPCV Status~%~d queued | ~d received | last start: ~a"
            expected received start-text)))

(defun generate-evolution-report ()
  "Generate the Evolution Factory Report (formerly Python).
   Answers User Q1: S-Rank = Battlefield (Veteran), A-Rank = Training."
  ;; V50.x Fix: Force sync metrics from DB to ensure Report is accurate (User Req)
  (refresh-strategy-metrics-from-db :force t)
  
  (let* ((all swimmy.globals:*strategy-knowledge-base*)
         ;; Filter by Rank (V47.8: Updated to use Rank System instead of Tiers)
         (s-rank (count-if (lambda (s) (eq (strategy-rank s) :S)) all))
         (a-rank (count-if (lambda (s) (eq (strategy-rank s) :A)) all))
         (b-rank (count-if (lambda (s) (eq (strategy-rank s) :B)) all)) ; Selection
         (graveyard (length (directory (merge-pathnames "GRAVEYARD/*.lisp" swimmy.persistence:*library-path*))))
         ;; New Recruits (24h) - using new creation-time slot (P13)
         (one-day-ago (- (get-universal-time) 86400))
         (new-recruits (count-if (lambda (s) 
                                   (and (strategy-creation-time s)
                                        (> (strategy-creation-time s) one-day-ago))) 
                                 all)))
    (let ((top-snippet (build-top-candidates-snippet all))
          (cpcv-snippet (build-cpcv-status-snippet))
          (oos-snippet (oos-metrics-summary-line)))
    
    (format nil "
🏭 **Evolution Factory Report**
Current status of the autonomous strategy generation pipeline.

🧠 Knowledge Base (Active)
~d Strategies

🏆 **S-Rank (Verified Elite)**
~d (Sharpe≥0.5 PF≥1.5 WR≥45% MaxDD<15% + CPCV)

🎖️ **A-Rank (Pro)**
~d (Sharpe≥0.3 PF≥1.2 WR≥40% MaxDD<20% + OOS)

🪜 **B-Rank (Selection)**
~d (Sharpe≥0.1 PF≥1.0 WR≥30% MaxDD<30%)

👶 New Recruits (24h)
~d

👻 Graveyard
~d

~a

~a

~a

⚙️ System Status
✅ Evolution Daemon Active
✅ Native Lisp Orchestration (V28)
~a"
            (length all)
            s-rank
            a-rank
            b-rank
            new-recruits
            graveyard
            cpcv-snippet
            oos-snippet
            top-snippet
            (format-timestamp (get-universal-time)))))) 

(defun write-evolution-report-files (report)
  "Persist the Evolution Factory Report to local files."
  (let ((paths (list (list "data/reports/evolution_factory_report.txt" report))))
    (dolist (entry paths)
      (destructuring-bind (path content) entry
        (ensure-directories-exist path)
        (with-open-file (stream path :direction :output :if-exists :supersede :if-does-not-exist :create)
          (write-string content stream))))))

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
    (send-evolution-report report)))

(defun oos-metrics-summary-line ()
  "Human-readable summary of OOS pipeline health for reports/Discord."
  (let* ((m (report-oos-metrics))
         (f (report-oos-failure-stats))
         (avg (getf m :latency-avg 0.0))
         (mn (or (getf m :latency-min) "-"))
         (mx (or (getf m :latency-max) "-")))
    (format nil "OOS sent: ~d retry: ~d success: ~d failure: ~d (data ~d send ~d db ~d) latency(avg/min/max): ~,2f/~a/~a sec"
            (getf m :sent 0)
            (getf m :retry 0)
            (getf m :success 0)
            (getf m :failure 0)
            (getf f :data-invalid 0)
            (getf f :send-failure 0)
            (getf f :db-error 0)
            avg mn mx)))
