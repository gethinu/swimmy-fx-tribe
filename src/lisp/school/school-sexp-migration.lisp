;;; school-sexp-migration.lisp

(in-package :swimmy.school)

(defun %legacy-plist-p (obj)
  (and (listp obj) (keywordp (first obj))))

(defun %timeframe->minutes (tf)
  "Normalize timeframe input to internal minutes(int)."
  (labels ((all-digits-p (s)
             (and (stringp s)
                  (> (length s) 0)
                  (loop for ch across s always (digit-char-p ch))))
           (parse-int (s default)
             (handler-case (parse-integer s) (error () default))))
    (cond
      ((numberp tf) (max 1 (round tf)))
      ((stringp tf)
       (let ((up (string-upcase (string-trim '(#\Space #\Tab #\Newline #\Return) tf))))
         (cond
           ((or (string= up "MN") (string= up "MN1")) 43200)
           ((and (>= (length up) 2) (char= (char up 0) #\M)
                 (all-digits-p (subseq up 1)))
            (max 1 (parse-int (subseq up 1) 1)))
           ((and (>= (length up) 2) (char= (char up 0) #\H)
                 (all-digits-p (subseq up 1)))
            (* 60 (max 1 (parse-int (subseq up 1) 1))))
           ((and (>= (length up) 2) (char= (char up 0) #\D)
                 (all-digits-p (subseq up 1)))
            (* 1440 (max 1 (parse-int (subseq up 1) 1))))
           ((and (>= (length up) 2) (char= (char up 0) #\W)
                 (all-digits-p (subseq up 1)))
            (* 10080 (max 1 (parse-int (subseq up 1) 1))))
           (t 1))))
      (t 1))))

(defun %normalize-strategy-timeframe! (strat)
  "Canonicalize strategy timeframe slot to minutes(int)."
  (when (strategy-p strat)
    (setf (strategy-timeframe strat)
          (%timeframe->minutes (strategy-timeframe strat))))
  strat)

(defun %parse-maybe-form (form-str)
  (when (and form-str (stringp form-str) (> (length form-str) 0))
    (swimmy.core:safe-read-sexp form-str :package :swimmy.school)))

(defun %plist->strategy (plist &key indicators entry exit)
  (let ((name (or (getf plist :name) (getf plist :NAME))))
    (when name
      (make-strategy
       :name name
       :indicators (or indicators '())
       :entry entry
       :exit exit
       :sl (or (getf plist :sl) (getf plist :SL) 0.0)
       :tp (or (getf plist :tp) (getf plist :TP) 0.0)
       :sharpe (or (getf plist :sharpe) (getf plist :SHARPE) 0.0)
       :profit-factor (or (getf plist :profit-factor) (getf plist :PROFIT-FACTOR) 0.0)
       :win-rate (or (getf plist :win-rate) (getf plist :WIN-RATE) 0.0)
       :max-dd (or (getf plist :max-dd) (getf plist :MAX-DD) 0.0)
       :timeframe (%timeframe->minutes (or (getf plist :timeframe) (getf plist :TIMEFRAME) "M1"))
       :direction (or (getf plist :direction) (getf plist :DIRECTION) :BOTH)
       :symbol (or (getf plist :symbol) (getf plist :SYMBOL) "USDJPY")))))

(defun normalize-strategy-sexp (sexp-str &key indicators entry exit)
  "Return plist (:status :ok/:quarantine :strategy strat :sexp serialized :reason reason)."
  (let* ((obj (swimmy.core:safe-read-sexp sexp-str :package :swimmy.school))
         (strat (cond
                  ((strategy-p obj) (%normalize-strategy-timeframe! obj))
                  ((%legacy-plist-p obj) (%plist->strategy obj :indicators indicators :entry entry :exit exit))
                  (t nil))))
    (if strat
        (list :status :ok :strategy strat :sexp (format nil "~s" strat))
        (list :status :quarantine :strategy nil :sexp nil :reason :invalid))))
