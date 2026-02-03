;;; school-sexp-migration.lisp

(in-package :swimmy.school)

(defun %legacy-plist-p (obj)
  (and (listp obj) (keywordp (first obj))))

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
       :timeframe (or (getf plist :timeframe) (getf plist :TIMEFRAME) "M1")
       :direction (or (getf plist :direction) (getf plist :DIRECTION) :BOTH)
       :symbol (or (getf plist :symbol) (getf plist :SYMBOL) "USDJPY")))))

(defun normalize-strategy-sexp (sexp-str &key indicators entry exit)
  "Return plist (:status :ok/:quarantine :strategy strat :sexp serialized :reason reason)."
  (let* ((obj (swimmy.core:safe-read-sexp sexp-str :package :swimmy.school))
         (strat (cond
                  ((strategy-p obj) obj)
                  ((%legacy-plist-p obj) (%plist->strategy obj :indicators indicators :entry entry :exit exit))
                  (t nil))))
    (if strat
        (list :status :ok :strategy strat :sexp (format nil "~s" strat))
        (list :status :quarantine :strategy nil :sexp nil :reason :invalid))))
