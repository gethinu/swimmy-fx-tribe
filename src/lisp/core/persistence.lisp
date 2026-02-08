
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


(defun resolve-library-root ()
  "Resolve the project root for persistence (env override supported)."
  (let ((env (ignore-errors (uiop:getenv "SWIMMY_HOME"))))
    (cond
      ((and env (> (length env) 0))
       (uiop:ensure-directory-pathname env))
      (t
       (uiop:ensure-directory-pathname (uiop:getcwd))))))

(defparameter *library-path* (merge-pathnames "data/library/" (resolve-library-root)))

(defparameter *library-rank-dirs* '("GRAVEYARD" "RETIRED" "INCUBATOR" "B" "A" "S" "LEGEND" "LEGEND-ARCHIVE")
  "Rank-based directories used for sharded file persistence.")

(defparameter *legacy-tier-dirs* '("INCUBATOR" "SELECTION" "TRAINING" "BATTLEFIELD" "VETERAN" "LEGEND" "GRAVEYARD")
  "Legacy tier directories retained only for migration/cleanup.")

(defun ensure-directory (path)
  (ensure-directories-exist path))

(defun sanitize-filename (name)
  (substitute #\_ #\/ (substitute #\_ #\\ name)))

(defun normalize-rank-dir (rank)
  "Normalize rank into a directory name."
  (let ((r (cond
             ((null rank) "INCUBATOR")
             ((keywordp rank) (symbol-name rank))
             ((symbolp rank) (symbol-name rank))
             ((stringp rank) (string-upcase rank))
             (t "INCUBATOR"))))
    (string-upcase r)))

(defun strategy-storage-rank (strategy-obj)
  "Resolve the storage rank for a strategy (Rank-first, Incubator fallback)."
  (let ((rank (slot-value strategy-obj 'swimmy.school::rank)))
    (cond
      ((null rank) :INCUBATOR)
      ((member rank '(:B :A :S :LEGEND :LEGEND-ARCHIVE :GRAVEYARD :RETIRED :INCUBATOR) :test #'eq) rank)
      (t :INCUBATOR))))

(defun get-strategy-path (name rank)
  (let ((rank-str (normalize-rank-dir rank))
        (name-str (sanitize-filename name)))
    (merge-pathnames (format nil "~a/~a.lisp" rank-str name-str) *library-path*)))

(defun init-library ()
  "Initialize the data library directories"
  (ensure-directory *library-path*)
  (dolist (dir *library-rank-dirs*)
    (ensure-directory (merge-pathnames (format nil "~a/" dir) *library-path*)))
  (format t "[LIB] 📚 Library initialized at ~a~%" *library-path*))

(defun save-strategy (strategy-obj)
  "Save a strategy object to a file ATOMICALLY (Expert Panel 2).
   Writes to .tmp first, then renames to prevent corruption."
  
  (let* ((name (slot-value strategy-obj 'swimmy.school::name))
         (rank (strategy-storage-rank strategy-obj))
         (path (get-strategy-path name rank))
         (temp-path (merge-pathnames (format nil "~a.tmp" (pathname-name path)) path)))
    
    (ensure-directories-exist path)

    ;; Preserve creation-time on re-save to avoid git noise and keep "new recruit"
    ;; semantics stable (creation-time should mean "born", not "last saved").
    (when (probe-file path)
      (let ((existing (ignore-errors (load-strategy path))))
        (when existing
          (let ((existing-creation-time (ignore-errors (slot-value existing 'swimmy.school::creation-time))))
            (when existing-creation-time
              (setf (slot-value strategy-obj 'swimmy.school::creation-time)
                    existing-creation-time))))))
    
    ;; 1. Write to Temp File
    (with-open-file (out temp-path :direction :output :if-exists :supersede)
      (with-standard-io-syntax
        (let ((*print-readably* t)
              (*print-pretty* t))
          (write strategy-obj :stream out))))
    
    ;; 2. Atomic Rename
    (rename-file temp-path path)
    
    (format t "[LIB] 💾 Saved ~a to ~a (Atomic)~%" name path)))

(defun %rewrite-struct-sexp (content)
  "Convert \"#S(STRATEGY ...)\" to \"(STRATEGY ...)\" so we can parse safely."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) content)))
    (when (and (>= (length trimmed) 3)
               (char-equal (char trimmed 0) #\#)
               (char-equal (char trimmed 1) #\S)
               (char= (char trimmed 2) #\()))
      (let ((after (subseq trimmed 3)))
        (concatenate 'string "(" after))))

(defun %read-strategy-from-string (content)
  (when (and content (stringp content))
    (let ((*read-eval* nil)
          (*package* (find-package :swimmy.school)))
      (handler-case
          (multiple-value-bind (obj _) (read-from-string content nil nil)
            (declare (ignore _))
            (cond
              ((and obj (fboundp 'swimmy.school::strategy-p)
                    (swimmy.school::strategy-p obj))
               obj)
              ((and (listp obj)
                    (symbolp (first obj))
                    (string-equal (symbol-name (first obj)) "STRATEGY"))
               (apply #'swimmy.school:make-strategy (rest obj)))
              (t nil)))
        (error () nil)))))

(defun load-strategy (path)
  "Load a strategy with package protection (Expert Panel 2026-01-30)."
  (with-open-file (in path)
    (with-standard-io-syntax
      (let ((*package* (find-package :swimmy.school)))
        (handler-case
            (read in)
          (error (e)
            (format t "[PERSISTENCE] ⚠️ Failed to read strategy from ~a: ~a~%" path e)
            (let* ((content (ignore-errors (uiop:read-file-string path)))
                   (rewritten (and content (%rewrite-struct-sexp content)))
                   (fallback (or (%read-strategy-from-string content)
                                 (%read-strategy-from-string rewritten))))
              (when fallback
                (format t "[PERSISTENCE] ✅ Recovered strategy from ~a via fallback parser~%" path))
              fallback)))))))

(defun load-all-strategies ()
  "Load all strategies from the library"
  (let ((strategies nil)
        (count 0))
    (dolist (dir '("INCUBATOR" "B" "A" "S" "LEGEND" "LEGEND-ARCHIVE"))
      (let ((wildcard (merge-pathnames (format nil "~a/*.lisp" dir) *library-path*)))
        (dolist (file (directory wildcard))
          (handler-case
              (let ((strat (load-strategy file)))
                ;; Rank is authoritative. Directory is storage only.
                (when strat
                  (push strat strategies)
                  (incf count)))
            (error (e)
              (format t "[LIB] ⚠️ Failed to load ~a: ~a~%" file e))))))
    (format t "[LIB] 📖 Loaded ~d strategies from Library~%" count)
    strategies))

(defun delete-strategy (strategy-obj &key rank)
  "Delete the file associated with the strategy."
  (let* ((name (slot-value strategy-obj 'swimmy.school::name))
         (resolved-rank (or rank (strategy-storage-rank strategy-obj)))
         (path (get-strategy-path name resolved-rank)))
    (cond
      ((probe-file path)
       (delete-file path)
       (format t "[LIB] 🗑️ Deleted ~a from ~a~%" name path))
      ;; Legacy tier fallback (pre-migration files)
      ((and (null rank)
            (slot-exists-p strategy-obj 'swimmy.school::tier)
            (let* ((tier (slot-value strategy-obj 'swimmy.school::tier))
                   (legacy-path (get-strategy-path name tier)))
              (when (probe-file legacy-path)
                (delete-file legacy-path)
                (format t "[LIB] 🗑️ Deleted ~a from legacy ~a~%" name legacy-path)
                t))))
      (t
       (format t "[LIB] ⚠️ File not found for deletion: ~a~%" path)))))

(defun move-strategy (strategy-obj new-rank &key (force nil) (from-rank nil))
  "Move strategy to a new rank (delete old file, update slot, save new file).
   V49.3: Fortress Mode - Blocks moving A/S/Legend to Graveyard without :force t."
  
  ;; Fortress Safety Check
  (let ((rank (slot-value strategy-obj 'swimmy.school::rank))
        (name (slot-value strategy-obj 'swimmy.school::name)))
    (when (and (member rank '(:A :S :legend))
               (not force)
               (or (eq new-rank :graveyard) 
                   (string-equal (string new-rank) "GRAVEYARD")))
       (error "[PERSISTENCE] 🛡️ FORTRESS BLOCK: Attempted to move protected ~a strategy '~a' to Graveyard without FORCE!" 
              rank name)))

  ;; 1. Delete old file using CURRENT rank (legacy tier fallback supported)
  (if from-rank
      (delete-strategy strategy-obj :rank from-rank)
      (delete-strategy strategy-obj))
  
  ;; 2. Update rank slot
  (setf (slot-value strategy-obj 'swimmy.school::rank) new-rank)
  
  ;; 3. Save to new location
  (save-strategy strategy-obj)
  (format t "[LIB] 🚚 Moved ~a to ~a~%" (slot-value strategy-obj 'swimmy.school::name) new-rank))

(defun strategy-exists-p (name rank)
  (probe-file (get-strategy-path name rank)))
