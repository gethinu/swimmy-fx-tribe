;; school-voting.lisp - Swimmy School: Leader System
;; Swarm consensus/voting removed (2026-02-09)

(in-package :swimmy.school)

;;; ==========================================
;;; LEADER FISH SYSTEM (リーダーフィッシュ)
;;; ==========================================
;;; Inspired by: Ensemble Meta-Learning + Natural flocking behavior
;;; The best performing strategy leads the school

(defstruct leader-info
  strategy-name
  sharpe
  win-rate
  tenure-start
  trades-as-leader
  pnl-as-leader)

(defun elect-leader ()
  "Elect the best performing strategy as leader"
  (let* ((all-strategies (append *strategy-knowledge-base* *evolved-strategies*))
         (candidates (remove-if-not (lambda (s)
                                      (and (strategy-sharpe s)
                                           (> (strategy-sharpe s) 0)))
                                    all-strategies))
         (sorted (sort (copy-list candidates) #'> :key #'strategy-sharpe))
         (best (first sorted)))
    (when best
      (let ((new-leader-name (strategy-name best)))
        ;; Only change leader if tenure exceeded or no current leader
        (when (or (null *current-leader*)
                  (> *leader-tenure* *min-leader-tenure*))
          (unless (and *current-leader*
                       (string= new-leader-name
                                (leader-info-strategy-name *current-leader*)))
            ;; New leader elected!
            (when *current-leader*
              (push *current-leader* *leader-history*))
            (setf *current-leader*
                  (make-leader-info
                   :strategy-name new-leader-name
                   :sharpe (strategy-sharpe best)
                   :win-rate 0.0
                   :tenure-start (get-universal-time)
                   :trades-as-leader 0
                   :pnl-as-leader 0.0))
            (setf *leader-tenure* 0)
            (format t "[L] 👑 NEW LEADER: ~a (Sharpe: ~,2f)~%"
                    new-leader-name (strategy-sharpe best)))))))
  *current-leader*)

(defun get-leader-direction (history)
  "Get the leader's trading signal"
  (when *current-leader*
    (let* ((leader-name (leader-info-strategy-name *current-leader*))
           (leader-strat (or (find leader-name *strategy-knowledge-base*
                                   :key #'strategy-name :test #'string=)
                             (find leader-name *evolved-strategies*
                                   :key #'strategy-name :test #'string=))))
      (when leader-strat
        (evaluate-strategy-signal leader-strat history)))))

(defun update-leader-stats (pnl)
  "Update leader's performance statistics"
  (when *current-leader*
    (incf (leader-info-trades-as-leader *current-leader*))
    (incf (leader-info-pnl-as-leader *current-leader*) pnl)
    (incf *leader-tenure*)))
