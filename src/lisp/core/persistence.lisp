
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
  "Save a strategy object to a file ATOMICALLY (Expert Panel 2).
   Writes to .tmp first, then renames to prevent corruption."
  
  (let* ((name (slot-value strategy-obj 'swimmy.school::name))
         (tier (slot-value strategy-obj 'swimmy.school::tier))
         (path (get-strategy-path name tier))
         (temp-path (merge-pathnames (format nil "~a.tmp" (pathname-name path)) path)))
    
    (ensure-directories-exist path)
    
    ;; 1. Write to Temp File
    (with-open-file (out temp-path :direction :output :if-exists :supersede)
      (with-standard-io-syntax
        (let ((*print-readably* t)
              (*print-pretty* t))
          (write strategy-obj :stream out))))
    
    ;; 2. Atomic Rename
    (rename-file temp-path path)
    
    (format t "[LIB] 💾 Saved ~a to ~a (Atomic)~%" name path)))

(defun load-strategy (path)
  "Load a strategy with package protection (Expert Panel 2026-01-30)."
  (with-open-file (in path)
    (with-standard-io-syntax
      (let ((*package* (find-package :swimmy.school)))
        (handler-case
            (read in)
          (error (e)
            (format t "[PERSISTENCE] ⚠️ Failed to read strategy from ~a: ~a~%" path e)
            nil))))))

(defun load-all-strategies ()
  "Load all strategies from the library"
  (let ((strategies nil)
        (count 0))
    (dolist (tier '("INCUBATOR" "TRAINING" "BATTLEFIELD" "VETERAN" "LEGEND"))
      (let ((wildcard (merge-pathnames (format nil "~a/*.lisp" tier) *library-path*)))
        (dolist (file (directory wildcard))
          (handler-case
              (let ((strat (load-strategy file)))
                ;; V28.1: Sync Rank with Tier on Load (Fixes Execution Block)
                (when strat
                  (let ((tier-kw (intern (string-upcase tier) :keyword)))
                    ;; Force Tier to match Directory (Truth)
                    (setf (slot-value strat 'swimmy.school::tier) tier-kw)
                    ;; Force Rank logic
                    (when (eq tier-kw :battlefield)
                      (setf (slot-value strat 'swimmy.school::rank) :veteran)
                      ;; (format t "[LIB] ⚔️ Restoring VETERAN rank for ~a~%" (slot-value strat 'swimmy.school::name))
                      )))
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

(defun move-strategy (strategy-obj new-tier &key (force nil))
  "Move strategy to a new tier (delete old file, update slot, save new file).
   V49.3: Fortress Mode - Blocks moving A/S/Legend to Graveyard without :force t."
  
  ;; Fortress Safety Check
  (let ((rank (slot-value strategy-obj 'swimmy.school::rank))
        (name (slot-value strategy-obj 'swimmy.school::name)))
    (when (and (member rank '(:A :S :legend))
               (not force)
               (or (eq new-tier :graveyard) 
                   (string-equal (string new-tier) "GRAVEYARD")))
       (error "[PERSISTENCE] 🛡️ FORTRESS BLOCK: Attempted to move protected ~a strategy '~a' to Graveyard without FORCE!" 
              rank name)))

  ;; 1. Delete old file using CURRENT tier
  (delete-strategy strategy-obj)
  
  ;; 2. Update tier slot
  (setf (slot-value strategy-obj 'swimmy.school::tier) new-tier)
  
  ;; 3. Save to new location
  (save-strategy strategy-obj)
  (format t "[LIB] 🚚 Moved ~a to ~a~%" (slot-value strategy-obj 'swimmy.school::name) new-tier))

(defun strategy-exists-p (name tier)
  (probe-file (get-strategy-path name tier)))
