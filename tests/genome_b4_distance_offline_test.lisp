;;;; genome_b4_distance_offline_test.lisp
;;;; Offline, FFI-free regression for the B-4/R4 behavioural distance
;;;; (regen_engine_redesign_20260703.md §B-4; tribe rebuild §R4).
;;;;
;;;; Native Windows cannot `asdf:load-system :swimmy` (sqlite/pzmq FFI DLLs are absent —
;;;; see memory native-windows-sbcl-blockers), so this harness loads ONLY the pure
;;;; school-genome.lisp against a minimal strategy struct and exercises the real
;;;; calculate-genetic-distance / extract-genome. Same assertions also live in the full
;;;; suite as test-b4-* (src/lisp/tests/evolution-tests.lisp) for CI where the FFI is present.
;;;;
;;;; Run from the repo root:
;;;;   sbcl --script tests/genome_b4_distance_offline_test.lisp
;;;; Exits non-zero on any failure.

(defpackage :swimmy.core (:use :cl) (:export #:*enable-primitive-diversity*))
(in-package :swimmy.core)
(defparameter *enable-primitive-diversity* nil)

(defpackage :swimmy.school (:use :cl :swimmy.core))
(in-package :swimmy.school)

;; Minimal struct — only the slots the genome functions touch (defaults match dsl.lisp).
(defstruct strategy
  name indicators entry exit (sl 0.0) (tp 0.0) (volume 0.01)
  (sharpe 0.0) (trades 0) (category :trend) (pnl-history nil)
  (timeframe 1) (generation 0) (symbol "USDJPY"))

(load (merge-pathnames "src/lisp/school/school-genome.lisp" *default-pathname-defaults*))

(defvar *pass* 0)
(defvar *fail* 0)
(defun chk (label ok &optional detail)
  (if ok (progn (incf *pass*) (format t "  PASS  ~a~@[  ~a~]~%" label detail))
         (progn (incf *fail*) (format t "  FAIL  ~a~@[  ~a~]~%" label detail))))
(defun ~= (a b &optional (eps 1e-6)) (< (abs (- a b)) eps))
(defun g (&key (symbol "USDJPY") (category :trend) (timeframe 1440)
               (indicators '((sma 27) (sma 93))) (entry '(cross-above close sma-27))
               (sl 0.03) (tp 0.09) (behavior nil))
  (list :symbol symbol :category category :timeframe timeframe
        :indicators indicators :entry entry :sl sl :tp tp :behavior behavior))
(defmacro with-flag (val &body body) `(let ((*enable-primitive-diversity* ,val)) ,@body))

(format t "~%========== B-4 / R4 OFFLINE DISTANCE TEST ==========~%")

;; A. extract-genome carries symbol + behaviour (regen B-1)
(let* ((s (make-strategy :name "T" :symbol "EURUSD" :category :reversion :timeframe 240
                         :indicators '((rsi 14)) :sl 0.5 :tp 1.0 :pnl-history '(1.0 -2.0 3.0)))
       (gm (extract-genome s)))
  (chk "extract-genome carries :symbol" (equal (getf gm :symbol) "EURUSD"))
  (chk "extract-genome carries :behavior" (equal (getf gm :behavior) '(1.0 -2.0 3.0))))

;; B. Flag OFF = byte-identical legacy, blind to symbol
(let* ((a (g :symbol "USDJPY")) (b (g :symbol "EURUSD")))
  (with-flag nil
    (chk "flag OFF dispatches to legacy" (~= (calculate-genetic-distance a b)
                                             (legacy-genetic-distance a b)))
    (chk "flag OFF is blind to symbol" (~= (calculate-genetic-distance a b) 0.0))))

;; C. Flag ON collapses same-niche clones to near-zero
(let* ((c1 (g :indicators '((sma 27) (sma 93)) :sl 0.03 :tp 0.09))
       (c2 (g :indicators '((sma 36) (sma 144)) :entry '(cross-above close sma-36) :sl 0.04 :tp 0.12))
       (off (with-flag nil (calculate-genetic-distance c1 c2)))
       (on  (with-flag t   (calculate-genetic-distance c1 c2))))
  (chk "flag OFF scatters clones (> 0.02 floor) -- the bug" (> off 0.02)
       (format nil "legacy=~,4f" off))
  (chk "flag ON collapses clones (< 0.02 floor)" (< on 0.02)
       (format nil "B-4=~,4f (was ~,4f)" on off))
  (chk "flag ON clones rejected by sweet spot" (with-flag t (not (genetic-compatibility-p c1 c2)))))

;; D. Flag ON keeps non-USDJPY / cross-regime far
(with-flag t
  (let ((d-sym (calculate-genetic-distance (g) (g :symbol "EURUSD" :indicators '((vwapvr 50 220))
                                                  :entry '(vwapvr-signal) :sl 1.2 :tp 4.0)))
        (d-cat (calculate-genetic-distance (g) (g :category :reversion :timeframe 240
                                                  :indicators '((rsi 14)) :entry '(< rsi-14 30)
                                                  :sl 0.5 :tp 1.0))))
    (chk "flag ON non-USDJPY far (> 0.2)" (> d-sym 0.2) (format nil "dist=~,4f" d-sym))
    (chk "flag ON cross-regime far (> 0.2)" (> d-cat 0.2) (format nil "dist=~,4f" d-cat))))

;; E. Behaviour corr hook (regen B-4 PRIMARY; dormant when data absent)
(chk "behaviour identical -> ~0" (~= (genome-behavior-distance '(1.0 2.0 3.0 4.0) '(1.0 2.0 3.0 4.0)) 0.0 1e-4))
(chk "behaviour anti-corr -> ~0" (~= (genome-behavior-distance '(1.0 2.0 3.0 4.0) '(-1.0 -2.0 -3.0 -4.0)) 0.0 1e-4))
(chk "behaviour <3 points -> NIL (dormant)" (null (genome-behavior-distance '(1.0 2.0) '(1.0 2.0))))
(chk "behaviour absent -> NIL (never fabricates)" (null (genome-behavior-distance nil nil)))

(format t "========== RESULT: ~d passed, ~d failed ==========~%" *pass* *fail*)
(when (> *fail* 0) (sb-ext:exit :code 1))
