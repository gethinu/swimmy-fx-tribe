;;; ============================================================================
;;; shell/narrative.lisp - Tribal Narrative System

(in-package :swimmy.shell)
;;; ============================================================================
;;; Reputation, gossip, tribal dialect - the "warm narrative" layer
;;; Part of "The Efficient Gardener" refactoring
;;;
;;; NOTE: Core elder logic is in engine/learning.lisp
;;;       This file handles the _presentation_ and ceremonies
;;; ============================================================================

;;; ==========================================
;;; REPUTATION SYSTEM
;;; ==========================================

(defparameter *reputation-scores* (make-hash-table :test 'equal))
(defparameter *gossip-log* nil)

(defstruct reputation
  strategy-name
  trust-score       ; 0.0 - 1.0
  profit-score      ; Based on actual profit
  reliability       ; How often it follows through
  recklessness      ; Did it push limits?
  peer-reviews)     ; What other strategies think

(defun get-reputation (strategy-name)
  "Get or create reputation for a strategy."
  (or (gethash strategy-name *reputation-scores*)
      (setf (gethash strategy-name *reputation-scores*)
            (make-reputation
             :strategy-name strategy-name
             :trust-score 0.5
             :profit-score 0
             :reliability 0.5
             :recklessness 0
             :peer-reviews nil))))

(defun update-reputation (strategy-name outcome &key (pnl 0) (pushed-limits nil))
  "Update reputation after a trade."
  (let ((rep (get-reputation strategy-name)))
    (case outcome
      (:win
       (setf (reputation-trust-score rep)
             (min 1.0 (+ (reputation-trust-score rep) 0.05)))
       (incf (reputation-profit-score rep) pnl)
       (incf (reputation-reliability rep) 0.02))
      (:loss
       (setf (reputation-trust-score rep)
             (max 0.0 (- (reputation-trust-score rep) 0.03)))
       (incf (reputation-profit-score rep) pnl)
       (decf (reputation-reliability rep) 0.01)))
    
    (when pushed-limits
      (incf (reputation-recklessness rep))
      (add-gossip strategy-name "pushed limits" -0.1))))

;;; ==========================================
;;; GOSSIP SYSTEM
;;; ==========================================

(defun add-gossip (about-strategy topic sentiment)
  "Add gossip about a strategy."
  (let ((gossip (list :about about-strategy
                      :topic topic
                      :sentiment sentiment
                      :time (get-universal-time))))
    (push gossip *gossip-log*)
    
    ;; Affect reputation
    (let ((rep (get-reputation about-strategy)))
      (setf (reputation-trust-score rep)
            (max 0.0 (min 1.0 (+ (reputation-trust-score rep) sentiment)))))
    
    (format t "[L] 💬 GOSSIP: 「~aが~aしたらしい...」~%"
            about-strategy topic)))

(defun get-tribal-mood ()
  "Analyze recent gossip to get tribal mood."
  (let* ((recent (subseq *gossip-log* 0 (min 10 (length *gossip-log*))))
         (total-sentiment (reduce #'+ recent
                                  :key (lambda (g) (or (getf g :sentiment) 0))
                                  :initial-value 0)))
    (cond
      ((> total-sentiment 0.5) :optimistic)
      ((< total-sentiment -0.5) :pessimistic)
      (t :neutral))))

;;; ==========================================
;;; TRIBAL DIALECT (Simplified)
;;; ==========================================
;;; Expert Panel Review (Priority 3):
;;; "Poetic" patterns removed to reduce noise.
;;; This section now only handles basic narrative initialization if needed.

(defun initialize-tribal-dialect ()
  "Initialize the tribal language (No-op in simplified version)."
  (format t "[L] 📚 Tribal Dialect initialized (Simplified)~%"))

(defun detect-patterns (history)
  "Detect active patterns (No-op in simplified version)."
  nil)

;;; ==========================================
;;; HALL OF FAME CEREMONY (Display)
;;; ==========================================

(defun display-elder-induction (elder)
  "Display the induction ceremony for a new elder.
   This is called after induct-to-hall-of-fame from engine/learning.lisp"
  (format t "~%[L] ═══════════════════════════════════════~%")
  (format t "[L] 🏛️ HALL OF FAME INDUCTION~%")
  (format t "[L] ═══════════════════════════════════════~%")
  (format t "[L] 👑 ~a joins the Elders!~%" (elder-name elder))
  (format t "[L] 💰 Peak PnL: ¥~:d~%" (round (elder-peak-pnl elder)))
  (format t "[L] 📖 Wisdom: \"~a\"~%" (elder-wisdom elder))
  (format t "[L] ═══════════════════════════════════════~%~%"))

(defun display-elder-vote-message (elder-name wisdom vote-type context-type)
  "Display an elder's vote message in tribal style."
  (format t "[L] 👴 Elder ~a: 「~a」~%"
          elder-name
          (case vote-type
            (:volatility-warning "ボラが高すぎる。わしの時代もそうだった。")
            (:patience-warning "待て。レンジでは焦るな。")
            (:learned-lesson-volatility "過去の失敗から学んだ。極端なボラは危険だ。")
            (:learned-lesson-ranging "レンジ相場での失敗を覚えている。")
            (t wisdom))))

(defun display-elder-memory-decay ()
  "Display the poetic message when elder lessons decay."
  (format t "[L] 👴 長老の記憶が薄れる...（減衰適用）~%"))

(format t "[SHELL] narrative.lisp loaded~%")
