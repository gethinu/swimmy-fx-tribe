;;; tools/run_lisp_wisdom.lisp
;;; ============================================================================
;;; WISDOM EXTRACTION (One-Shot)
;;; ============================================================================

(load "swimmy.asd")
(ql:quickload :swimmy)

(format t "~%[WISDOM] 🔮 Loading Knowledge Base for Extraction...~%")

;; Run Analysis
(handler-case
    (let ((count (swimmy.school:analyze-veterans)))
      (format t "~%[WISDOM] ✅ Extraction Complete. Extracted ~d Genes.~%" count))
  (error (e)
    (format t "~%[WISDOM] ❌ Extraction Failed: ~a~%" e)))

(sb-ext:exit)
