(in-package :swimmy.school)

;;; ============================================================================
;;; SCHOOL MACRO: GLOBAL MACRO MATRIX & DYNAMIC CORRELATION (Simons Logic)
;;; ============================================================================
;;; Phase 23: "Look at the stars, not just the telescope."
;;; Implements:
;;; 1. Macro Data Loading (Yahoo Finance CSVs)
;;; 2. Dynamic Correlation Analysis (USDJPY vs US10Y)
;;; 3. Regime Override Logic (Broken Arrow)
;;; ============================================================================

(defparameter *macro-correlation-window* 100 "Window for correlation (hours)")
(defparameter *macro-data-dir* (merge-pathnames "data/macro/" (swimmy.school::resolve-swimmy-home)))

(defun parse-macro-csv (driver-name)
  "Parse CSV for driver. Returns list of (timestamp . close)."
  (let ((path (merge-pathnames (format nil "~a.csv" driver-name) *macro-data-dir*))
        (history nil))
    (when (probe-file path)
      (with-open-file (stream path :direction :input)
        (read-line stream nil nil) ; Skip header
        (loop for line = (read-line stream nil nil)
              while line do
              (let* ((parts (uiop:split-string line :separator ","))
                     (date-str (first parts))
                     (close-str (nth 4 parts))) ; Open, High, Low, Close
                (when (and date-str close-str)
                  ;; Parse Date (YYYY-MM-DD HH:MM:SS...) - Simplifying: using index/string for now or Universal Time if possible.
                  ;; For correlation, we assume aligned indices mostly.
                  (let ((val (swimmy.core:safe-parse-number close-str)))
                    (when (numberp val)
                      (push (cons date-str val) history))))))))
    (nreverse history)))

(defun load-macro-data ()
  "Load all 14 drivers into *macro-state*."
  (ensure-directories-exist *macro-data-dir*)
  (format t "[MACRO] 🌍 Loading Global Macro Matrix...~%")
  (dolist (driver *macro-drivers*)
    (let ((data (parse-macro-csv driver)))
      (if data
          (setf (gethash driver *macro-state*) data)
          (format t "[MACRO] ⚠️ No data for ~a~%" driver))))
  (setf *last-macro-load-time* (get-universal-time)))

(defun get-macro-history (driver)
  (let ((data (gethash driver *macro-state*)))
    (if data (mapcar #'cdr data) nil)))

(defun get-macro-latest (driver)
  (let ((hist (gethash driver *macro-state*)))
    (if hist (cdr (car (last hist))) 0.0)))

(defun get-macro-change (driver n)
  "Get % change over last N periods."
  (let ((hist (gethash driver *macro-state*)))
    (if (and hist (> (length hist) n))
        (let* ((current (cdr (car (last hist))))
               (prev (cdr (nth (- (length hist) n 1) hist))))
          (if (and current prev (> prev 0))
              (/ (- current prev) prev)
              0.0))
        0.0)))

;;; ============================================================================
;;; DYNAMIC CORRELATION ENGINE (Simons)
;;; ============================================================================

(defun calculate-correlation (seq1 seq2)
  "Calculate Pearson Correlation between two sequences."
  (let ((l1 (length seq1))
        (l2 (length seq2)))
    (when (or (< l1 2) (< l2 2))
      (return-from calculate-correlation 0.0))
    (let* ((n (min l1 l2))
           (s1 (subseq seq1 (- l1 n)))
           (s2 (subseq seq2 (- l2 n)))
           (mean1 (/ (reduce #'+ s1) n))
           (mean2 (/ (reduce #'+ s2) n)))
    (let ((numerator (loop for x in s1 for y in s2 sum (* (- x mean1) (- y mean2))))
          (denom1 (loop for x in s1 sum (expt (- x mean1) 2)))
          (denom2 (loop for y in s2 sum (expt (- y mean2) 2))))
      (if (and (> denom1 0) (> denom2 0))
          (/ numerator (sqrt (* denom1 denom2)))
          0.0)))))

(defun analyze-macro-correlations ()
  "Analyze key correlations for Regime Detection."
  (let* ((usdjpy (get-macro-history "USDJPY")) ; Note: Need USDJPY in macro fetcher or use internal data
         (us10y (get-macro-history "US10Y"))
         (spx (get-macro-history "SPX"))
         (dxy (get-macro-history "DXY"))
         (vix (get-macro-history "VIX")))
    (declare (ignore usdjpy vix))
    
    ;; WARNING: If USDJPY is not in *macro-drivers* fetched by Python, this fails.
    ;; Phase 19 Python script had: DXY, US10Y, SPX...
    ;; It did NOT have USDJPY.
    ;; But School has internal USDJPY data.
    ;; Ideally we use the internal `*candle-history*` vs external Macro.
    ;; But mixing them is tricky due to alignment.
    ;; For Phase 23, let's assume we use what we have in *macro-state*.
    ;; If USDJPY is missing, we skip that check.
    ;; Wait, Simons explicitly said "Corr(USDJPY, US10Y)".
    ;; I should update the Python fetcher to include USDJPY logic OR use internal history.
    ;; Internal History is in `*candle-histories*` (hash of symbol -> candles).
    ;; `get-macro-correlation` should handle this.
    
    (list 
     :us10y-usdjpy (calculate-correlation (get-internal-close-history "USDJPY") us10y)
     :spx-risk (calculate-correlation spx (get-internal-close-history "USDJPY")) ;; USDJPY often correlates with SPX (Risk On)
     :dxy-euro (calculate-correlation dxy (get-internal-close-history "EURUSD")))))

(defun get-internal-close-history (symbol)
  "Get close prices from internal Swimmy history."
  (let ((hist (gethash symbol swimmy.school::*candle-histories*)))
    (if hist 
        (mapcar #'candle-close hist)
        nil)))

(defun detect-broken-arrow ()
  "Detect Regime Shift via Correlation Breakdown."
  ;; Simons: "When USDJPY ignores US10Y, the regime has changed."
  (let* ((usdjpy-hist (get-internal-close-history "USDJPY"))
         (us10y-hist (get-macro-history "US10Y"))
         (corr (calculate-correlation usdjpy-hist us10y-hist)))
    
    (cond
      ((< corr 0.2) 
       (format t "[MACRO] 🏹 BROKEN ARROW: US10Y/USDJPY Decoupled (Corr: ~,2f)~%" corr)
       :decoupled)
      (t :aligned))))

(format t "[L] 🌠 school-macro.lisp loaded - Dynamic Correlation Matrix (Simons)~%")
