;;; src/lisp/system/opus.lisp
;;; OPUS INTEGRATION: LOG ANALYSIS & KNOWLEDGE BASE
(in-package :swimmy.main)
;;; ==========================================
;;; Enables continuous AI partnership with automatic log analysis
;;; and knowledge base management.

(defparameter *log-analysis-history* nil)
(defparameter *knowledge-base-path* #P"/home/swimmy/swimmy/.opus/knowledge_base.lisp")
(defparameter *learned-patterns* nil)
(defparameter *improvement-requests* nil)

;;; ------------------------------------------------------------------
;;; Data Structures
;;; ------------------------------------------------------------------

(defstruct log-analysis
  timestamp
  trading-days-analyzed
  total-pnl
  win-rate
  best-strategy
  worst-strategy
  key-findings
  improvement-suggestions)

(defstruct improvement-request
  timestamp
  category      ; :strategy, :risk, :performance, :architecture
  description
  context
  status        ; :pending, :analyzed, :implemented
  response)

(defstruct learned-pattern
  timestamp
  pattern-type   ; :win-condition, :loss-condition, :regime-behavior
  description
  frequency
  confidence)

;;; ------------------------------------------------------------------
;;; Log Analysis Logic
;;; ------------------------------------------------------------------

(defun generate-log-analysis-report ()
  "Generate analysis report from recent performance data"
  (let* ((wins *consecutive-wins*)
         (losses *consecutive-losses*)
         (total (+ wins losses))
         (win-rate (if (> total 0) (* 100.0 (/ wins total)) 50.0))
         (progress (if (fboundp 'get-goal-progress) (get-goal-progress) (list :pace-pct 0 :days-elapsed 0 :actual-pnl 0)))
         (findings nil)
         (suggestions nil))
    
    ;; Analyze patterns
    (when (< (getf progress :pace-pct) 80)
      (push "ペースが目標を下回っている" findings)
      (push "より積極的な戦略、またはリスク調整を検討" suggestions))
    
    (when (> *danger-level* 1)
      (push "危険レベルが高い" findings)
      (push "戦略の見直し、または一時停止を検討" suggestions))
    
    (when (eq *current-volatility-state* :elevated)
      (push "ボラティリティが高い状態が続いている" findings)
      (push "ポジションサイズの縮小を継続" suggestions))
    
    (when (and *current-leader* (< (leader-info-pnl-as-leader *current-leader*) 0))
      (push "現在のリーダー戦略がマイナス" findings)
      (push "リーダー交代の検討" suggestions))
    
    ;; Create analysis
    (let ((analysis (make-log-analysis
                     :timestamp (get-universal-time)
                     :trading-days-analyzed (getf progress :days-elapsed)
                     :total-pnl (getf progress :actual-pnl)
                     :win-rate win-rate
                     :best-strategy (if *current-leader* 
                                        (leader-info-strategy-name *current-leader*) 
                                        "N/A")
                     :worst-strategy "分析中"
                     :key-findings (or findings (list "特に問題なし"))
                     :improvement-suggestions (or suggestions (list "現状維持")))))
      
      (push analysis *log-analysis-history*)
      
      ;; Output report
      (format t "~%[L] ═══════════════════════════════════════════════════~%")
      (format t "[L] 📊 OPUS LOG ANALYSIS REPORT~%")
      (format t "[L] ═══════════════════════════════════════════════════~%")
      (format t "[L] 📅 Days analyzed: ~d~%" (log-analysis-trading-days-analyzed analysis))
      (format t "[L] 💰 Total PnL: ¥~:d~%" (round (log-analysis-total-pnl analysis)))
      (format t "[L] 🎯 Win rate: ~,1f%~%" (log-analysis-win-rate analysis))
      (format t "[L] 👑 Best strategy: ~a~%" (log-analysis-best-strategy analysis))
      (format t "[L]~%")
      (format t "[L] 🔍 Key Findings:~%")
      (dolist (f (log-analysis-key-findings analysis))
        (format t "[L]    • ~a~%" f))
      (format t "[L]~%")
      (format t "[L] 💡 Improvement Suggestions:~%")
      (dolist (s (log-analysis-improvement-suggestions analysis))
        (format t "[L]    → ~a~%" s))
      (format t "[L] ═══════════════════════════════════════════════════~%~%")
      
      analysis)))

;;; ------------------------------------------------------------------
;;; Improvement Requests
;;; ------------------------------------------------------------------

(defun request-opus-analysis (category description)
  "Generate a request for Opus analysis (stored for next Gemini call)"
  (let ((request (make-improvement-request
                  :timestamp (get-universal-time)
                  :category category
                  :description description
                  :context (list :pnl *daily-pnl*
                                :regime *current-regime*
                                :volatility *current-volatility-state*
                                :danger *danger-level*)
                  :status :pending
                  :response nil)))
    (push request *improvement-requests*)
    (format t "[L] 🤖 SELF-IMPROVEMENT: Request queued - ~a: ~a~%" category description)
    request))

(defun auto-request-improvements ()
  "Automatically generate improvement requests based on current state"
  ;; Performance issues
  (when (< *daily-pnl* (- (or (if (fboundp 'get-daily-target) (get-daily-target) 1000) 1000)))
    (request-opus-analysis :performance 
                           "日次損失が目標の倍以上。戦略の問題か市場適合の問題か分析が必要"))
  
  ;; Risk issues
  (when (>= *consecutive-losses* 4)
    (request-opus-analysis :risk 
                           "4連敗以上。リスク管理の強化または戦略変更を検討"))
  
  ;; Strategy issues
  (when (and *current-leader* 
             (> (leader-info-trades-as-leader *current-leader*) 10)
             (< (leader-info-pnl-as-leader *current-leader*) 0))
    (request-opus-analysis :strategy 
                           "10トレード以上でリーダー戦略がマイナス。戦略の有効性を再評価")))

(defun generate-gemini-prompt-for-improvements ()
  "Generate a prompt for Gemini API with pending improvement requests"
  (let ((pending (remove-if-not (lambda (r) (eq (improvement-request-status r) :pending))
                                *improvement-requests*)))
    (when pending
      (format nil "あなたはSwimmyトレーディングシステムの分析者です。~%~%現在の状況:~%- 日次PnL: ¥~:d~%- レジーム: ~a~%- ボラティリティ: ~a~%- 危険レベル: ~d~%~%以下の改善要求を分析してください:~%~{~%~a. [~a] ~a~}~%~%各項目について具体的な改善提案をしてください。"
              (round *daily-pnl*)
              *current-regime*
              *current-volatility-state*
              *danger-level*
              (let ((i 0))
                (mapcar (lambda (r)
                          (incf i)
                          (list i (improvement-request-category r) (improvement-request-description r)))
                        pending))))))

;;; ------------------------------------------------------------------
;;; Knowledge Base
;;; ------------------------------------------------------------------

(defun record-learned-pattern (pattern-type description confidence)
  "Record a learned pattern to knowledge base"
  (let ((existing (find description *learned-patterns* 
                        :key #'learned-pattern-description :test #'string=)))
    (if existing
        ;; Update existing
        (progn
          (incf (learned-pattern-frequency existing))
          (setf (learned-pattern-confidence existing) 
                (* 0.9 confidence (+ 0.1 (learned-pattern-confidence existing)))))
        ;; Add new
        (push (make-learned-pattern
               :timestamp (get-universal-time)
               :pattern-type pattern-type
               :description description
               :frequency 1
               :confidence confidence)
              *learned-patterns*)))
  (format t "[L] 📚 KNOWLEDGE: Recorded pattern - ~a~%" description))

(defun save-knowledge-base ()
  "Save knowledge base to file"
  (handler-case
      (with-open-file (out *knowledge-base-path* :direction :output :if-exists :supersede)
        (write (list :patterns *learned-patterns*
                     :analyses *log-analysis-history*
                     :improvements *improvement-requests*)
               :stream out :pretty t))
    (error (e) (format t "[L] ⚠️ Could not save knowledge base: ~a~%" e))))

(defun load-knowledge-base ()
  "Load knowledge base from file"
  (handler-case
      (with-open-file (in *knowledge-base-path* :direction :input :if-does-not-exist nil)
        (when in
          (let ((data (read in nil nil)))
            (setf *learned-patterns* (getf data :patterns))
            (setf *log-analysis-history* (getf data :analyses))
            (setf *improvement-requests* (getf data :improvements))
            (format t "[L] 📚 Knowledge base loaded: ~d patterns, ~d analyses~%"
                    (length *learned-patterns*) (length *log-analysis-history*)))))
    (error (e) nil)))

(defun opus-daily-session ()
  "Run daily Opus integration session - call this at end of trading day"
  (format t "~%[L] 🤖 ═══════════════════════════════════════════════════~%")
  (format t "[L] 🤖 OPUS DAILY SESSION~%")
  (format t "[L] 🤖 ═══════════════════════════════════════════════════~%~%")
  
  ;; 1. Generate log analysis
  (generate-log-analysis-report)
  
  ;; 2. Auto-generate improvement requests
  (auto-request-improvements)
  
  ;; 3. Record any obvious patterns
  (when (> *consecutive-wins* 3)
    (record-learned-pattern :win-condition 
                            (format nil "~a レジームで ~a 連勝" *current-regime* *consecutive-wins*)
                            0.7))
  
  ;; 4. Save knowledge base
  (save-knowledge-base)
  
  ;; 5. Generate Gemini prompt if needed
  (let ((prompt (generate-gemini-prompt-for-improvements)))
    (when prompt
      (format t "~%[L] 📋 GEMINI PROMPT FOR NEXT SESSION:~%")
      (format t "[L] ~a~%~%" (subseq prompt 0 (min 500 (length prompt))))))
  
  (format t "[L] 🤖 Session complete. Ready for Opus review.~%")
  (format t "[L] 🤖 ═══════════════════════════════════════════════════~%~%"))
