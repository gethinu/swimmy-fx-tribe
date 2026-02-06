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
