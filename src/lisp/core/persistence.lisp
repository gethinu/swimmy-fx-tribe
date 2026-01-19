
(defpackage :swimmy.persistence
  (:use :cl :sb-ext)
  (:export :*library-path*
           :init-library
           :save-strategy
           :load-strategy
           :load-all-strategies
           :delete-strategy
           :move-strategy
           :strategy-exists-p))

(in-package :swimmy.persistence)


(defparameter *library-path* (merge-pathnames "data/library/" (user-homedir-pathname)))

;; Or specifically: /home/swimmy/swimmy/data/library/
(defparameter *library-path* #P"/home/swimmy/swimmy/data/library/")

(defun ensure-directory (path)
  (ensure-directories-exist path))

(defun sanitize-filename (name)
  (substitute #\_ #\/ (substitute #\_ #\\ name)))

(defun get-strategy-path (name tier)
  (let ((tier-str (if (keywordp tier) (symbol-name tier) (string-upcase tier)))
        (name-str (sanitize-filename name)))
    (merge-pathnames (format nil "~a/~a.lisp" tier-str name-str) *library-path*)))

(defun init-library ()
  "Initialize the data library directories"
  (ensure-directory *library-path*)
  (dolist (tier '("GRAVEYARD" "INCUBATOR" "TRAINING" "BATTLEFIELD" "VETERAN" "LEGEND"))
    (ensure-directory (merge-pathnames (format nil "~a/" tier) *library-path*)))
  (format t "[LIB] 📚 Library initialized at ~a~%" *library-path*))

(defun save-strategy (strategy-obj)
  "Save a strategy object to a file"
  ;; We rely on the strategy object having accessible slots (using accessors from swimmy.school)
  ;; However, to avoid circular dependencies, we might need to be passed the slot values or a property list.
  ;; Better: We assume this package is used by swimmy.school, so we can't depend on swimmy.school here easily.
  ;; Strategy 2: Pass a PLIST or STRUCTURE-OBJECT generic print.
  
  (let* ((name (slot-value strategy-obj 'swimmy.school::name))
         (tier (slot-value strategy-obj 'swimmy.school::tier))
         (path (get-strategy-path name tier)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede)
      (with-standard-io-syntax
        (let ((*print-readably* t)
              (*print-pretty* t))
          (write strategy-obj :stream out))))
    (format t "[LIB] 💾 Saved ~a to ~a~%" name path)))

(defun load-strategy (path)
  (with-open-file (in path)
    (with-standard-io-syntax
      (read in))))

(defun load-all-strategies ()
  "Load all strategies from the library"
  (let ((strategies nil)
        (count 0))
    (dolist (tier '("GRAVEYARD" "INCUBATOR" "TRAINING" "BATTLEFIELD" "VETERAN" "LEGEND"))
      (let ((wildcard (merge-pathnames (format nil "~a/*.lisp" tier) *library-path*)))
        (dolist (file (directory wildcard))
          (handler-case
              (let ((strat (load-strategy file)))
                (push strat strategies)
                (incf count))
            (error (e)
              (format t "[LIB] ⚠️ Failed to load ~a: ~a~%" file e))))))
    (format t "[LIB] 📖 Loaded ~d strategies from Library~%" count)
    strategies))

(defun delete-strategy (strategy-obj)
  "Delete the file associated with the strategy."
  (let* ((name (slot-value strategy-obj 'swimmy.school::name))
         (tier (slot-value strategy-obj 'swimmy.school::tier))
         (path (get-strategy-path name tier)))
    (if (probe-file path)
        (progn
          (delete-file path)
          (format t "[LIB] 🗑️ Deleted ~a from ~a~%" name path))
        (format t "[LIB] ⚠️ File not found for deletion: ~a~%" path))))

(defun move-strategy (strategy-obj new-tier)
  "Move strategy to a new tier (delete old file, update slot, save new file)."
  ;; 1. Delete old file using CURRENT tier
  (delete-strategy strategy-obj)
  
  ;; 2. Update tier slot
  (setf (slot-value strategy-obj 'swimmy.school::tier) new-tier)
  
  ;; 3. Save to new location
  (save-strategy strategy-obj)
  (format t "[LIB] 🚚 Moved ~a to ~a~%" (slot-value strategy-obj 'swimmy.school::name) new-tier))

(defun strategy-exists-p (name tier)
  (probe-file (get-strategy-path name tier)))
