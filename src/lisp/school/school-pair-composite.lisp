;;; school-pair-composite.lisp - Pair-Composite utilities

(in-package :swimmy.school)

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
