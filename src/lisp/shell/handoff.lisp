;;; ============================================================================
;;; shell/handoff.lisp - Opus Handoff & Weekly Summaries

(in-package :swimmy.shell)
;;; ============================================================================
;;; AI collaboration handoff documents
;;; Part of "The Efficient Gardener" refactoring
;;;               src/lisp/metrics.lisp (for report data)
;;; ============================================================================

;;; ==========================================
;;; STATE
;;; ==========================================

(defparameter *handoff-path* "/home/swimmy/swimmy/.opus/daily_handoff.md")
(defparameter *last-weekly-summary* 0)

;;; ==========================================
;;; UTILITY FUNCTIONS
;;; ==========================================

(defun get-date-string ()
  "Get current date as string."
  (multiple-value-bind (s m h day month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore s m h))
    (format nil "~d-~2,'0d-~2,'0d" year month day)))

(defun get-time-string ()
  "Get current time as string (UTC) HH:MM:SS"
  (multiple-value-bind (s m h)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~2,'0d:~2,'0d:~2,'0d" h m s)))

(defun get-jst-time-string ()
  "Get current time as string (JST) HH:MM:SS"
  (multiple-value-bind (s m h)
      (decode-universal-time (get-universal-time) -9)
    (format nil "~2,'0d:~2,'0d:~2,'0d" h m s)))

;;; ==========================================
;;; DAILY HANDOFF
;;; ==========================================

(defun generate-daily-handoff ()
  "Generate daily handoff markdown for Opus AI partner."
  (let* ((progress (get-goal-progress))
         (issues nil))
    
    ;; Detect issues
    (when (< (getf progress :pace-pct) 80)
      (push "ペースが目標の80%を下回っている" issues))
    (when (and (boundp '*danger-level*) (> *danger-level* 1))
      (push (format nil "危険レベルが高い: ~d" *danger-level*) issues))
    (when (and (fboundp 'has-resigned-p) (has-resigned-p))
      (push "本日のトレードを投了した" issues))
    (when (and (boundp '*consecutive-losses*) (>= *consecutive-losses* 3))
      (push (format nil "~d連敗中" *consecutive-losses*) issues))
    
    ;; Generate markdown
    (handler-case
        (progn
          (ensure-directories-exist *handoff-path*)
          (with-open-file (out *handoff-path* :direction :output :if-exists :supersede)
            (format out "# 🐟 Swimmy Daily Handoff~%")
            (format out "## Opus AI パートナーへの引き継ぎ~%~%")
            (format out "**日付**: ~a~%" (get-date-string))
            (format out "**更新時刻**: ~a~%" (get-time-string))
            (format out "**更新者**: Swimmy Bot (自動生成)~%~%")
            (format out "---~%~%")
            
            ;; Summary table
            (format out "## 📊 今日のサマリー~%~%")
            (format out "| 項目 | 値 |~%")
            (format out "|------|-----|~%")
            (format out "| 日次PnL | ¥~:d |~%" (round *daily-pnl*))
            (format out "| 累計PnL | ¥~:d |~%" (round (getf progress :actual-pnl)))
            (format out "| 目標進捗 | ~,1f% |~%" (getf progress :progress-pct))
            (format out "| ペース | ~,0f% |~%" (getf progress :pace-pct))
            (format out "| リーダー | ~a |~%"
                    (if (and (boundp '*current-leader*) *current-leader*)
                        (leader-info-strategy-name *current-leader*)
                        "未選出"))
            (format out "| レジーム | ~a |~%"
                    (if (boundp '*current-regime*) *current-regime* "不明"))
            (format out "| ボラティリティ | ~a |~%"
                    (if (boundp '*current-volatility-state*) *current-volatility-state* "不明"))
            (format out "| 危険レベル | ~d |~%"
                    (if (boundp '*danger-level*) *danger-level* 0))
            (format out "| 投了 | ~a |~%"
                    (if (and (fboundp 'has-resigned-p) (has-resigned-p)) "はい" "いいえ"))
            (format out "~%---~%~%")
            
            ;; Issues
            (format out "## 🔍 発生した問題~%~%")
            (if issues
                (dolist (issue issues)
                  (format out "- ~a~%" issue))
                (format out "- 特になし~%"))
            (format out "~%---~%~%")
            
            ;; Questions for Opus
            (format out "## ❓ Opusへの質問~%~%")
            (if (and (boundp '*improvement-requests*)
                     (fboundp 'improvement-request-status)
                     (fboundp 'improvement-request-category)
                     (fboundp 'improvement-request-description))
                (let ((pending (remove-if-not
                                (lambda (r) (eq (improvement-request-status r) :pending))
                                *improvement-requests*)))
                  (if pending
                      (dolist (req pending)
                        (format out "- [~a] ~a~%"
                                (improvement-request-category req)
                                (improvement-request-description req)))
                      (format out "- なし（順調）~%")))
                (format out "- なし（順調）~%"))
            (format out "~%---~%~%")
            
            ;; Previous recommendations
            (format out "## 💡 前回のOpusからの提案~%~%")
            (format out "<!-- Opusが分析後にここに追記 -->~%")
            (format out "- 次回セッションで更新~%~%")
            (format out "---~%~%")
            
            ;; Actions taken
            (format out "## ✅ 実施したアクション~%~%")
            (format out "<!-- 前回の提案に対して実施したこと -->~%")
            (format out "- 次回セッションで更新~%~%")
            (format out "---~%~%")
            
            ;; Notes
            (format out "## 📝 メモ~%~%")
            (format out "<!-- 自由記述欄 -->~%~%")
            (format out "---~%~%")
            (format out "*このファイルはSwimmyとOpusの継続的コラボレーション用です。*~%")
            (format out "*`/daily-review` でOpusが分析を開始します。*~%"))
          
          (format t "[SHELL] 📋 Daily handoff generated: ~a~%" *handoff-path*)
          *handoff-path*)
      (error (e)
        (format t "[SHELL] ⚠️ Failed to generate handoff: ~a~%" e)
        nil))))

;;; ==========================================
;;; WEEKLY SUMMARY
;;; ==========================================

(defun generate-weekly-summary ()
  "Generate weekly summary (call once per week)."
  (let* ((progress (get-goal-progress))
         (summary-path "/home/swimmy/swimmy/.opus/weekly_summary.md"))
    
    (handler-case
        (progn
          (ensure-directories-exist summary-path)
          (with-open-file (out summary-path :direction :output :if-exists :supersede)
            (format out "# 📊 Swimmy 週次サマリー~%~%")
            (format out "**生成日**: ~a~%~%" (get-date-string))
            
            (format out "## パフォーマンス~%~%")
            (format out "| 項目 | 値 |~%")
            (format out "|------|-----|~%")
            (format out "| 累計PnL | ¥~:d |~%" (round (getf progress :actual-pnl)))
            (format out "| 目標達成率 | ~,1f% |~%" (getf progress :progress-pct))
            (format out "| ペース | ~,0f% |~%" (getf progress :pace-pct))
            (format out "~%")
            
            (format out "## 学習したパターン~%~%")
            (if (and (boundp '*learned-patterns*) *learned-patterns*)
                (dolist (p (subseq *learned-patterns* 0 (min 5 (length *learned-patterns*))))
                  (when (and (fboundp 'learned-pattern-description)
                             (fboundp 'learned-pattern-confidence))
                    (format out "- ~a (確信度: ~,0f%)~%"
                            (learned-pattern-description p)
                            (* 100 (learned-pattern-confidence p)))))
                (format out "- 記録されたパターンなし~%"))
            (format out "~%")
            
            (format out "## Opusへの質問~%~%")
            (format out "- 今週のパフォーマンスをどう評価しますか？~%")
            (format out "- 戦略の調整は必要ですか？~%")
            (format out "- 次週の市場予測と対策は？~%")
            (format out "~%")
            
            (format out "*週次レビューを依頼: `/daily-review` で Opus と対話*~%"))
          
          ;; Send Discord notification
          (notify-discord
           (format nil "📊 **週次サマリー生成完了**~%~%累計: ¥~:d (~,1f%)~%~%詳細: .opus/weekly_summary.md"
                   (round (getf progress :actual-pnl))
                   (getf progress :progress-pct))
           :color 3447003)
          
          (setf *last-weekly-summary* (get-universal-time))
          summary-path)
      (error (e)
        (format t "[SHELL] ⚠️ Failed to generate weekly summary: ~a~%" e)
        nil))))

(defun send-periodic-status-report (symbol bid)
  "V48.5: Periodic status reports Moved to Shell layer.
   Throttled by *status-notification-interval*."
  (let* ((now (get-universal-time))
         (last-time (gethash symbol *last-status-notification-time* 0)))
    (when (> (- now last-time) *status-notification-interval*)
      (let ((tribe-dir (if (boundp '*tribe-direction*) (symbol-value '*tribe-direction*) "N/A"))
            (tribe-con (if (and (boundp '*tribe-consensus*) (symbol-value '*tribe-consensus*)) (symbol-value '*tribe-consensus*) 0.0))
            (swarm-con (if (and (boundp '*last-swarm-consensus*) (symbol-value '*last-swarm-consensus*)) (symbol-value '*last-swarm-consensus*) 0.0))
            (pred (if (boundp '*last-prediction*) (symbol-value '*last-prediction*) "N/A"))
            (conf (if (and (boundp '*last-confidence*) (symbol-value '*last-confidence*)) (symbol-value '*last-confidence*) 0.0))
            (danger (if (boundp '*danger-level*) (symbol-value '*danger-level*) 0))
            (active-warriors (if (boundp '*warrior-allocation*) 
                                 (hash-table-count (symbol-value '*warrior-allocation*)) 0)))
        (swimmy.core:notify-discord-symbol symbol 
          (format nil "🕒 STATUS REPORT~%Price: ~,3f~%~%🧠 AI: ~a (~,1f%)~%🏛️ Tribes: ~a (~,0f%)~%🐟 Swarm: ~,0f%~%~%⚔️ Warriors: ~d~%⚠️ Danger: Lv~d"
                  bid pred (* 100 conf) tribe-dir (* 100 tribe-con) (* 100 swarm-con) active-warriors danger)
          :color swimmy.core:+color-status+)
        (setf (gethash symbol *last-status-notification-time*) now)))))

(format t "[SHELL] handoff.lisp loaded~%")
