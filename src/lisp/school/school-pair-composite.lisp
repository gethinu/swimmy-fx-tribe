;;; school-pair-composite.lisp - Pair-Composite utilities

(in-package :swimmy.school)

(defparameter *pair-strategy-enabled* nil
  "When true, apply pair-composite overlay logic.")

(defparameter *pair-active-defs* nil
  "Active pair definitions (plist with :pair-id, :a, :b, :weight-a, :weight-b).")

(defparameter *pair-max-lot* 0.05
  "Maximum lot allowed per pair overlay.")

(defun %fnv1a-64 (s)
  "Compute 64-bit FNV-1a hash for string S."
  (let ((hash #xCBF29CE484222325))
    (loop for ch across s
          do (setf hash (logxor hash (char-code ch))
                   hash (ldb (byte 64 0) (* hash #x100000001B3))))
    hash))

(defun pair-id (a b)
  "Return stable, order-independent ID for a pair of strategy names."
  (let* ((names (sort (list (string a) (string b)) #'string<))
         (raw (format nil "~a|~a" (first names) (second names))))
    (format nil "~16,'0x" (%fnv1a-64 raw))))

(defun %pair-def-matches-p (pair-def strategy-name)
  (let ((a (getf pair-def :a))
        (b (getf pair-def :b))
        (name (string strategy-name)))
    (or (and a (string= name (string a)))
        (and b (string= name (string b))))))

(defun apply-pair-overlay (strategy-name direction symbol base-lot)
  "Return (final-lot pair-id) after applying pair overlay (if enabled)."
  (declare (ignore direction symbol))
  (if (or (null strategy-name) (not *pair-strategy-enabled*))
      (list base-lot nil)
      (let* ((pair-def (find-if (lambda (def) (%pair-def-matches-p def strategy-name))
                                *pair-active-defs*))
             (pair-id (and pair-def (getf pair-def :pair-id)))
             (final-lot (if pair-def
                            (min base-lot *pair-max-lot*)
                            base-lot)))
        (list final-lot pair-id))))

(defparameter *pair-candidate-per-group* 50)
(defparameter *pair-eligible-ranks* '(:A :S :LEGEND :legend))

(defun %strategy-prop (strat key)
  (cond
    ((typep strat 'strategy)
     (ecase key
       (:name (strategy-name strat))
       (:symbol (strategy-symbol strat))
       (:timeframe (strategy-timeframe strat))
       (:sharpe (strategy-sharpe strat))
       (:rank (strategy-rank strat))))
    ((and (listp strat) (getf strat key)) (getf strat key))
    ((and (listp strat) (getf strat (intern (string-upcase (symbol-name key)) :keyword)))
     (getf strat (intern (string-upcase (symbol-name key)) :keyword)))
    (t nil)))

(defun %normalize-rank (rank)
  (cond
    ((keywordp rank) rank)
    ((symbolp rank) (intern (string-upcase (symbol-name rank)) :keyword))
    ((stringp rank) (intern (string-upcase rank) :keyword))
    (t nil)))

(defun %pair-eligible-rank-p (rank)
  (member (%normalize-rank rank) *pair-eligible-ranks*))

(defun %strategy-better-p (a b)
  (let ((sa (or (%strategy-prop a :sharpe) 0.0))
        (sb (or (%strategy-prop b :sharpe) 0.0))
        (na (string (%strategy-prop a :name)))
        (nb (string (%strategy-prop b :name))))
    (if (/= sa sb)
        (> sa sb)
        (string< na nb))))

(defun pair-candidate-pool (strategies &key (per-group *pair-candidate-per-group*))
  (let ((groups (make-hash-table :test 'equal)))
    (dolist (s strategies)
      (when (%pair-eligible-rank-p (%strategy-prop s :rank))
        (let* ((symbol (or (%strategy-prop s :symbol) ""))
               (tf (or (%strategy-prop s :timeframe) 0))
               (key (format nil "~a|~a" symbol tf)))
          (push s (gethash key groups)))))
    (let (out)
      (maphash (lambda (key group)
                 (declare (ignore key))
                 (let* ((sorted (sort (copy-list group) #'%strategy-better-p))
                        (top (subseq sorted 0 (min per-group (length sorted)))))
                   (setf out (nconc out top))))
               groups)
      out)))

