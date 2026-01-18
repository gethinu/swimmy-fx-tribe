;;; src/lisp/school/school-breeder.lisp
;;; ============================================================================
;;; STRATEGY BREEDING LAB (Evolution Engine)
;;; ============================================================================
;;; Implements "Breeding" as requested by Naval/Musk.
;;; Takes fittest strategies and generates offspring for the Incubator.
;;; ============================================================================

(in-package :swimmy.school)

(defun random-range (min max)
  "Return random number between min and max."
  (+ min (random (- max min))))

(defun mutate-value (val factor)
  "Mutate a numeric value by +/- factor."
  (if (numberp val)
      (let ((delta (* val factor (random-range -1.0 1.0))))
        (+ val delta))
      val))

(defun crossover-indicators (p1 p2)
  "Combine indicators from parent 1 and parent 2."
  (let ((i1 (strategy-indicators p1))
        (i2 (strategy-indicators p2)))
    ;; Simple logic: Take half from each, randomly
    (remove-duplicates (append (subseq i1 0 (ceiling (/ (length i1) 2)))
                               (subseq i2 0 (ceiling (/ (length i2) 2))))
                       :test #'equal)))

(defun breed-strategies (parent1 parent2)
  "Create a child strategy from two parents."
  (let* ((child-name (format nil "Bred-~a-~a-Gen~d" 
                             (subseq (strategy-name parent1) 0 (min 5 (length (strategy-name parent1))))
                             (random 1000)
                             (1+ (max (strategy-generation parent1) (strategy-generation parent2)))))
         (child-sl (mutate-value (/ (+ (strategy-sl parent1) (strategy-sl parent2)) 2.0) 0.1))
         (child-tp (mutate-value (/ (+ (strategy-tp parent1) (strategy-tp parent2)) 2.0) 0.1))
         (child-is (crossover-indicators parent1 parent2)))
    
    (make-strategy
      :name child-name
      :category (strategy-category parent1) ;; Inherit from P1
      :timeframe (strategy-timeframe parent1)
      :generation (1+ (max (strategy-generation parent1) (strategy-generation parent2)))
      :sl child-sl
      :tp child-tp
      :volume 0.01
      :indicators child-is
      ;; For now, inherit P1 entry/exit logic directly, but mutation should happen here too eventually
      :entry (strategy-entry parent1)
      :exit (strategy-exit parent1)
      :tier :incubator ;; Born in the Incubator
      :status :active)))

(defun run-breeding-cycle ()
  "Breed the Top 2 Battlefield strategies in each category."
  (format t "[BREEDER] 🧬 Starting Breeding Cycle...~%")
  (let ((categories '(:trend :reversion :breakout :scalp)))
    (dolist (cat categories)
      (let* ((warriors (get-strategies-by-tier :battlefield cat))
             (sorted (sort (copy-list warriors) #'> :key (lambda (s) (or (strategy-sharpe s) 0)))))
        (when (>= (length sorted) 2)
          (let ((p1 (first sorted))
                (p2 (second sorted)))
            (format t "[BREEDER] 💕 Breeding ~a + ~a~%" (strategy-name p1) (strategy-name p2))
            (let ((child (breed-strategies p1 p2)))
              (push child *strategy-knowledge-base*)
              (format t "[BREEDER] 👶 Born: ~a (Tier: Incubator)~%" (strategy-name child)))))))))

;;; ----------------------------------------------------------------------------
;;; Phase 6b: Persistence Implementation
;;; ----------------------------------------------------------------------------

(defun save-recruit-to-lisp (strat)
  "Append the strategy definition to strategies-dynamic.lisp (Code-as-Data)."
  (let ((filepath "src/lisp/strategies/strategies-dynamic.lisp"))
    (with-open-file (stream filepath
                            :direction :output
                            :if-exists :append
                            :if-does-not-exist :create)
      (format stream "~%~%(push ~%" )
      (format stream "  (make-strategy~%")
      (format stream "    :name ~s~%" (strategy-name strat))
      (format stream "    :category ~s~%" (strategy-category strat))
      (format stream "    :timeframe ~s~%" (strategy-timeframe strat))
      (format stream "    :generation ~d~%" (strategy-generation strat))
      (format stream "    :sl ~f~%" (strategy-sl strat))
      (format stream "    :tp ~f~%" (strategy-tp strat))
      (format stream "    :volume ~f~%" (strategy-volume strat))
      (format stream "    :indicators '~s~%" (strategy-indicators strat)) ;; Quote the list!
      (format stream "    :entry ~s~%" (strategy-entry strat))
      (format stream "    :exit ~s~%" (strategy-exit strat))
      (format stream "    :tier ~s~%" (strategy-tier strat))
      (format stream "    :status ~s)~%" (strategy-status strat))
      (format stream "  *strategy-knowledge-base*)~%"))
    (format t "[PERSIST] 💾 Saved code for ~a to ~a~%" (strategy-name strat) filepath)))

;;; ----------------------------------------------------------------------------
;;; Phase 3.7: The Entrance Exam (Recruitment Filter)
;;; ----------------------------------------------------------------------------

(defun recruit-elite-strategy ()
  "Recruit a new strategy by forcing it to pass the Entrance Exam (historical test).
   Calls tools/recruit_elite.py."
  (format t "[RECRUIT] ⚔️ Starting Entrance Exam process...~%")
  (handler-case
      (let ((output (uiop:run-program '("tools/recruit_elite.py") 
                                      :output :string 
                                      :ignore-error-status t)))
        (if (and output (> (length output) 0))
            (let* ((json (jsown:parse output))
                   (name (jsown:val json "name"))
                   (tf (jsown:val json "timeframe"))
                   (short (jsown:val json "sma_short"))
                   (long-p (jsown:val json "sma_long"))
                   (sl (jsown:val json "sl"))
                   (tp (jsown:val json "tp")))
              
              (format t "[RECRUIT] 🎉 PASSED! Recruit: ~a (TF:~a SMA:~a/~a)~%" name tf short long-p)
              
              ;; Create and Register
              (let ((recruit (make-strategy
                               :name name
                               :category :trend ;; Default category for now
                               :timeframe tf
                               :generation 0
                               :sl sl
                               :tp tp
                               :volume 0.01
                               :indicators (list (format nil "SMA-~a" short) (format nil "SMA-~a" long-p))
                               :entry (format nil "CROSS SMA ~a ~a" short long-p)
                               :exit (format nil "TP/SL")
                               :tier :incubator
                               :status :active)))
                (push recruit *strategy-knowledge-base*)
                ;; Phase 6b: Persist Immediately (Code-as-Data)
                (save-recruit-to-lisp recruit)
                recruit))
            (format t "[RECRUIT] ❌ All candidates failed the exam.~%")))
    (error (e)
      (format t "[RECRUIT] 💥 Error during recruitment: ~a~%" e))))
