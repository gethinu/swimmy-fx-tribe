;;; ============================================================================
;;; shell/interface.lisp - Natural Language Interface

(in-package :swimmy.shell)
;;; ============================================================================
;;; User query routing and natural language processing
;;; Part of "The Efficient Gardener" refactoring
;;; ============================================================================


;;; ==========================================
;;; NATURAL LANGUAGE QUERY INTERFACE
;;; ==========================================

(defun swimmy-ask (query)
  "Natural language interface for Intent Trading - process user queries.
   Supported query types:
   - status/状況/どう - Current system status
   - goal/目標/いくら - Goal progress
   - performance/成績/結果 - Performance summary
   - market/相場/マーケット - Market conditions
   - help - Available commands"
  (let ((q (string-downcase query)))
    (cond
      ;; Status queries
      ((or (search "status" q) (search "状況" q) (search "どう" q))
       (swimmy-status))
      
      ;; Goal queries
      ((or (search "goal" q) (search "目標" q) (search "いくら" q))
       (let ((progress (get-goal-progress)))
         (format nil "今月の目標: ¥~:d~%現在: ¥~:d (~,1f%)~%残り: ¥~:d"
                 *monthly-goal*
                 (round (getf progress :actual-pnl))
                 (getf progress :progress-pct)
                 (round (getf progress :remaining)))))
      
      ;; Performance queries
      ((or (search "performance" q) (search "成績" q) (search "結果" q))
       (get-performance-summary))
      
      ;; Market queries
      ((or (search "market" q) (search "相場" q) (search "マーケット" q))
       (format nil "レジーム: ~a~%ボラティリティ: ~a~%リーダー: ~a"
               (if (boundp '*current-regime*) *current-regime* "不明")
               (if (boundp '*current-volatility-state*) *current-volatility-state* "不明")
               (if (and (boundp '*current-leader*) *current-leader*)
                   (leader-info-strategy-name *current-leader*)
                   "未選出")))
      
      ;; Mood/tribal queries
      ((or (search "mood" q) (search "雰囲気" q) (search "ムード" q))
       (let ((mood (get-tribal-mood)))
         (format nil "部族のムード: ~a"
                 (case mood
                   (:optimistic "楽観的 🌞")
                   (:pessimistic "悲観的 🌧️")
                   (t "中立 ⚖️")))))
      
      ;; Elder wisdom
      ((or (search "elder" q) (search "長老" q) (search "知恵" q))
       (if *hall-of-fame*
           (format nil "長老の数: ~d~%直近の知恵: ~a"
                   (length *hall-of-fame*)
                   (elder-wisdom (first *hall-of-fame*)))
           "まだ長老はいません"))
      
      ;; Help
      (t
       "使えるクエリ: status/状況, goal/目標, performance/成績, market/相場, mood/雰囲気, elder/長老"))))

(format t "[SHELL] interface.lisp loaded~%")
