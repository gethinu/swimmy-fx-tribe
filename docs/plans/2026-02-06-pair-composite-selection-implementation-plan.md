# Pair-Composite Selection Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Pair-Compositeのペア選定ロジックをDB非依存の純粋関数として実装し、相関・スコア評価をテストで担保する。

**Architecture:** A/S/Legendの候補戦略をシンボル×TFごとに上位N=50で絞り、trade_listから生成したPnL系列でPearson相関とスコア(0.7*Sharpe+0.3*PF)を計算し上位5ペアを返す。相関は|corr|≤0.2、候補不足時のみ救済|corr|≤0.3。評価窓は直近100トレード、N不足は除外。純粋関数にし、外部依存は注入可能にする。

**Tech Stack:** Common Lisp (SBCL), S-expression (alist), SQLiteは呼ばない

### Task 0: 設計とSTATEの更新をコミット

**Files:**
- Modify: `docs/plans/2026-02-06-pair-composite-strategy-design.md`
- Modify: `docs/llm/STATE.md`

**Step 1: Update docs with candidate pool rule**

`docs/plans/2026-02-06-pair-composite-strategy-design.md` に「シンボル×TFごとの上位N=50」を追記し、`docs/llm/STATE.md` に同様の方針を明記する。

**Step 2: Commit**

```bash
git add docs/plans/2026-02-06-pair-composite-strategy-design.md docs/llm/STATE.md
git commit -m "docs: define pair candidate pool per symbol/tf"
```

### Task 1: 候補母集団の抽出 (シンボル×TFで上位N)

**Files:**
- Modify: `src/lisp/school/school-pair-composite.lisp`
- Modify: `src/lisp/tests/pair-composite-tests.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing test**

```lisp
(deftest test-pair-candidate-pool-top-n
  "pair candidate pool should select top N per symbol/timeframe"
  (let* ((s1 (swimmy.school::make-strategy :name "S1" :symbol "USDJPY" :timeframe 1 :sharpe 0.1 :rank :A))
         (s2 (swimmy.school::make-strategy :name "S2" :symbol "USDJPY" :timeframe 1 :sharpe 0.2 :rank :A))
         (s3 (swimmy.school::make-strategy :name "S3" :symbol "USDJPY" :timeframe 1 :sharpe 0.3 :rank :A))
         (s4 (swimmy.school::make-strategy :name "E1" :symbol "EURUSD" :timeframe 1 :sharpe 0.5 :rank :A))
         (s5 (swimmy.school::make-strategy :name "B1" :symbol "USDJPY" :timeframe 1 :sharpe 0.9 :rank :B))
         (pool (swimmy.school::pair-candidate-pool (list s1 s2 s3 s4 s5) :per-group 2)))
    (let ((names (mapcar #'swimmy.school::strategy-name pool)))
      (assert-true (= (length pool) 3) "Expected top 2 from USDJPY + 1 from EURUSD")
      (assert-true (and (member "S3" names :test #'string=)
                        (member "S2" names :test #'string=)
                        (member "E1" names :test #'string=))
                   "Expected S3,S2,E1 in pool")
      (assert-false (member "B1" names :test #'string=) "Rank B should be excluded"))))
```

Add this test to `src/lisp/tests.lisp` runner list.

**Step 2: Run test to verify it fails**

Run:
```bash
sbcl --non-interactive --eval '(progn (asdf:load-system :swimmy) (swimmy.tests:test-pair-candidate-pool-top-n) (sb-ext:quit))'
```
Expected: FAIL (function not implemented).

**Step 3: Write minimal implementation**

```lisp
(defparameter *pair-candidate-per-group* 50)
(defparameter *pair-eligible-ranks* '(:A :S :LEGEND :legend))

(defun %strategy-prop (strat key)
  (cond
    ((typep strat 'strategy)
     (ecase key
       (:name (strategy-name strat))
       (:symbol (strategy-symbol strat))
       (:timeframe (strategy-timeframe strat))
       (:sharpe (strategy-sharpe strat))
       (:rank (strategy-rank strat))))
    ((and (listp strat) (getf strat key)) (getf strat key))
    ((and (listp strat) (getf strat (intern (string-upcase (symbol-name key)) :keyword)))
     (getf strat (intern (string-upcase (symbol-name key)) :keyword)))
    (t nil)))

(defun %normalize-rank (rank)
  (cond
    ((keywordp rank) rank)
    ((symbolp rank) (intern (string-upcase (symbol-name rank)) :keyword))
    ((stringp rank) (intern (string-upcase rank) :keyword))
    (t nil)))

(defun %pair-eligible-rank-p (rank)
  (member (%normalize-rank rank) *pair-eligible-ranks*))

(defun %strategy-better-p (a b)
  (let ((sa (or (%strategy-prop a :sharpe) 0.0))
        (sb (or (%strategy-prop b :sharpe) 0.0))
        (na (string (%strategy-prop a :name)))
        (nb (string (%strategy-prop b :name))))
    (if (/= sa sb)
        (> sa sb)
        (string< na nb))))

(defun pair-candidate-pool (strategies &key (per-group *pair-candidate-per-group*))
  (let ((groups (make-hash-table :test 'equal)))
    (dolist (s strategies)
      (when (%pair-eligible-rank-p (%strategy-prop s :rank))
        (let* ((symbol (or (%strategy-prop s :symbol) ""))
               (tf (or (%strategy-prop s :timeframe) 0))
               (key (format nil "~a|~a" symbol tf)))
          (push s (gethash key groups)))))
    (let (out)
      (maphash (lambda (key group)
                 (declare (ignore key))
                 (let* ((sorted (sort (copy-list group) #'%strategy-better-p))
                        (top (subseq sorted 0 (min per-group (length sorted)))))
                   (setf out (nconc out top))))
               groups)
      out)))
```

**Step 4: Run test to verify it passes**

Run:
```bash
sbcl --non-interactive --eval '(progn (asdf:load-system :swimmy) (swimmy.tests:test-pair-candidate-pool-top-n) (sb-ext:quit))'
```
Expected: PASS.

**Step 5: Commit**

```bash
git add src/lisp/school/school-pair-composite.lisp src/lisp/tests/pair-composite-tests.lisp src/lisp/tests.lisp
git commit -m "feat: add pair candidate pool selection"
```

### Task 2: trade_list 正規化と0埋めマージ

**Files:**
- Modify: `src/lisp/school/school-pair-composite.lisp`
- Modify: `src/lisp/tests/pair-composite-tests.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing test**

```lisp
(deftest test-pair-align-pnl-series-zero-fill
  "align pnl series should zero-fill missing timestamps"
  (let* ((a '(((timestamp . 1) (pnl . 1.0))
              ((timestamp . 3) (pnl . -1.0))))
         (b '(((timestamp . 2) (pnl . 2.0))
              ((timestamp . 3) (pnl . 1.0))))
         (series-a (swimmy.school::trade-list->series a :max-trades 10))
         (series-b (swimmy.school::trade-list->series b :max-trades 10)))
    (multiple-value-bind (xs ys)
        (swimmy.school::align-pnl-series series-a series-b)
      (assert-equal '(1.0 0.0 -1.0) xs "Expected zero-fill for A")
      (assert-equal '(0.0 2.0 1.0) ys "Expected zero-fill for B"))))
```

Add this test to `src/lisp/tests.lisp` runner list.

**Step 2: Run test to verify it fails**

Run:
```bash
sbcl --non-interactive --eval '(progn (asdf:load-system :swimmy) (swimmy.tests:test-pair-align-pnl-series-zero-fill) (sb-ext:quit))'
```
Expected: FAIL (functions not implemented).

**Step 3: Write minimal implementation**

```lisp
(defparameter *pair-eval-window-trades* 100)
(defparameter *pair-min-trades* 100)

(defun %trade-entry-val (entry keys)
  (cond
    ((and (listp entry) (keywordp (first entry)))
     (loop for k in keys
           for kw = (cond ((keywordp k) k)
                          ((symbolp k) (intern (string-upcase (symbol-name k)) :keyword))
                          (t nil))
           for val = (and kw (getf entry kw :missing))
           unless (eq val :missing) do (return val)))
    (t
     (loop for k in keys
           for cell = (assoc k entry)
           when cell do (return (cdr cell))))))

(defun trade-list->series (trade-list &key (max-trades *pair-eval-window-trades*))
  (let ((pairs nil))
    (dolist (entry trade-list)
      (let ((ts (%trade-entry-val entry '(timestamp t)))
            (pnl (%trade-entry-val entry '(pnl))))
        (when (and ts pnl)
          (push (cons ts (float pnl 0.0)) pairs))))
    (setf pairs (sort pairs #'< :key #'car))
    (when (> (length pairs) max-trades)
      (setf pairs (subseq pairs (- (length pairs) max-trades))))
    ;; Collapse duplicates by summing
    (let ((acc (make-hash-table :test 'eql))
          (keys nil))
      (dolist (pair pairs)
        (let ((ts (car pair)) (pnl (cdr pair)))
          (unless (gethash ts acc) (push ts keys))
          (incf (gethash ts acc 0.0) pnl)))
      (setf keys (sort keys #'<))
      (mapcar (lambda (ts) (cons ts (gethash ts acc 0.0))) keys))))

(defun align-pnl-series (series-a series-b)
  (let ((map-a (make-hash-table :test 'eql))
        (map-b (make-hash-table :test 'eql))
        (ts nil))
    (dolist (pair series-a)
      (setf (gethash (car pair) map-a) (cdr pair))
      (pushnew (car pair) ts))
    (dolist (pair series-b)
      (setf (gethash (car pair) map-b) (cdr pair))
      (pushnew (car pair) ts))
    (setf ts (sort ts #'<))
    (values (mapcar (lambda (t) (gethash t map-a 0.0)) ts)
            (mapcar (lambda (t) (gethash t map-b 0.0)) ts))))
```

**Step 4: Run test to verify it passes**

Run:
```bash
sbcl --non-interactive --eval '(progn (asdf:load-system :swimmy) (swimmy.tests:test-pair-align-pnl-series-zero-fill) (sb-ext:quit))'
```
Expected: PASS.

**Step 5: Commit**

```bash
git add src/lisp/school/school-pair-composite.lisp src/lisp/tests/pair-composite-tests.lisp src/lisp/tests.lisp
git commit -m "feat: add trade list normalization for pair selection"
```

### Task 3: Pearson相関の実装

**Files:**
- Modify: `src/lisp/school/school-pair-composite.lisp`
- Modify: `src/lisp/tests/pair-composite-tests.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing test**

```lisp
(defun approx= (a b &optional (eps 1e-6))
  (< (abs (- a b)) eps))

(deftest test-pair-pearson-correlation
  "pearson correlation should be +/-1 for identical/inverse series"
  (let* ((xs '(1.0 2.0 3.0))
         (ys '(1.0 2.0 3.0))
         (zs '(3.0 2.0 1.0)))
    (assert-true (approx= 1.0 (swimmy.school::pearson-correlation xs ys)) "Expected corr=1")
    (assert-true (approx= -1.0 (swimmy.school::pearson-correlation xs zs)) "Expected corr=-1")))
```

Add this test to `src/lisp/tests.lisp` runner list.

**Step 2: Run test to verify it fails**

Run:
```bash
sbcl --non-interactive --eval '(progn (asdf:load-system :swimmy) (swimmy.tests:test-pair-pearson-correlation) (sb-ext:quit))'
```
Expected: FAIL (function not implemented).

**Step 3: Write minimal implementation**

```lisp
(defun pearson-correlation (xs ys)
  (let* ((n (length xs)))
    (if (or (zerop n) (/= n (length ys)))
        0.0
        (let* ((mean-x (/ (reduce #'+ xs) n))
               (mean-y (/ (reduce #'+ ys) n))
               (num 0.0)
               (den-x 0.0)
               (den-y 0.0))
          (dotimes (i n)
            (let ((dx (- (nth i xs) mean-x))
                  (dy (- (nth i ys) mean-y)))
              (incf num (* dx dy))
              (incf den-x (* dx dx))
              (incf den-y (* dy dy))))
          (if (or (zerop den-x) (zerop den-y))
              0.0
              (/ num (sqrt (* den-x den-y))))))))
```

**Step 4: Run test to verify it passes**

Run:
```bash
sbcl --non-interactive --eval '(progn (asdf:load-system :swimmy) (swimmy.tests:test-pair-pearson-correlation) (sb-ext:quit))'
```
Expected: PASS.

**Step 5: Commit**

```bash
git add src/lisp/school/school-pair-composite.lisp src/lisp/tests/pair-composite-tests.lisp src/lisp/tests.lisp
git commit -m "feat: add pearson correlation for pair selection"
```

### Task 4: 逆ボラ重みとスコア計算

**Files:**
- Modify: `src/lisp/school/school-pair-composite.lisp`
- Modify: `src/lisp/tests/pair-composite-tests.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing test**

```lisp
(defun approx= (a b &optional (eps 1e-6))
  (< (abs (- a b)) eps))

(deftest test-pair-score-from-pnls
  "score should use sharpe/pf on composite pnls"
  (let* ((pnls '(1.0 -0.5 1.0 -0.5))
         (result (swimmy.school::pair-score-from-pnls pnls))
         (sharpe (getf result :sharpe))
         (pf (getf result :pf))
         (score (getf result :score)))
    (assert-true (approx= 0.333333 sharpe 1e-4) "Expected sharpe ~0.3333")
    (assert-true (approx= 2.0 pf 1e-6) "Expected PF 2.0")
    (assert-true (approx= 0.833333 score 1e-4) "Expected score ~0.8333")))

(deftest test-pair-inverse-vol-weights
  "inverse vol weights should favor lower volatility"
  (multiple-value-bind (w1 w2)
      (swimmy.school::inverse-vol-weights '(1.0 -1.0) '(2.0 -2.0))
    (assert-true (approx= 0.666666 w1 1e-4) "Expected w1 ~0.6667")
    (assert-true (approx= 0.333333 w2 1e-4) "Expected w2 ~0.3333")))
```

Add these tests to `src/lisp/tests.lisp` runner list.

**Step 2: Run test to verify it fails**

Run:
```bash
sbcl --non-interactive --eval '(progn (asdf:load-system :swimmy) (swimmy.tests:test-pair-score-from-pnls) (swimmy.tests:test-pair-inverse-vol-weights) (sb-ext:quit))'
```
Expected: FAIL (functions not implemented).

**Step 3: Write minimal implementation**

```lisp
(defparameter *pair-score-sharpe-weight* 0.7)
(defparameter *pair-score-pf-weight* 0.3)

(defun %stddev (xs)
  (let* ((n (length xs)))
    (if (zerop n)
        0.0
        (let* ((mean (/ (reduce #'+ xs) n))
               (sq (reduce #'+ (mapcar (lambda (x) (expt (- x mean) 2)) xs)))
               (var (/ sq n)))
          (sqrt var)))))

(defun inverse-vol-weights (xs ys)
  (let* ((sx (%stddev xs))
         (sy (%stddev ys))
         (w1 (if (zerop sx) 0.0 (/ 1.0 sx)))
         (w2 (if (zerop sy) 0.0 (/ 1.0 sy)))
         (sum (+ w1 w2)))
    (cond
      ((> sum 0) (values (/ w1 sum) (/ w2 sum)))
      (t (values 0.5 0.5)))))

(defun composite-pnls (xs ys w1 w2)
  (mapcar (lambda (x y) (+ (* w1 x) (* w2 y))) xs ys))

(defun profit-factor (pnls)
  (let ((gains 0.0) (losses 0.0))
    (dolist (p pnls)
      (if (> p 0) (incf gains p) (incf losses (abs p))))
    (if (zerop losses) 0.0 (/ gains losses))))

(defun sharpe-from-pnls (pnls)
  (let* ((n (length pnls)))
    (if (zerop n)
        0.0
        (let* ((mean (/ (reduce #'+ pnls) n))
               (sd (%stddev pnls)))
          (if (zerop sd) 0.0 (/ mean sd))))))

(defun pair-score-from-pnls (pnls)
  (let* ((sharpe (sharpe-from-pnls pnls))
         (pf (profit-factor pnls))
         (score (+ (* *pair-score-sharpe-weight* sharpe)
                   (* *pair-score-pf-weight* pf))))
    (list :sharpe sharpe :pf pf :score score)))
```

**Step 4: Run test to verify it passes**

Run:
```bash
sbcl --non-interactive --eval '(progn (asdf:load-system :swimmy) (swimmy.tests:test-pair-score-from-pnls) (swimmy.tests:test-pair-inverse-vol-weights) (sb-ext:quit))'
```
Expected: PASS.

**Step 5: Commit**

```bash
git add src/lisp/school/school-pair-composite.lisp src/lisp/tests/pair-composite-tests.lisp src/lisp/tests.lisp
git commit -m "feat: add pair scoring helpers"
```

### Task 5: ペア選定パイプラインと救済モード

**Files:**
- Modify: `src/lisp/school/school-pair-composite.lisp`
- Modify: `src/lisp/tests/pair-composite-tests.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: Write the failing test**

```lisp
(deftest test-pair-selection-rescue-mode
  "rescue mode should allow |corr|<=0.3 when < max pairs"
  (let* ((s1 (swimmy.school::make-strategy :name "A" :symbol "USDJPY" :timeframe 1 :sharpe 0.5 :rank :A))
         (s2 (swimmy.school::make-strategy :name "B" :symbol "USDJPY" :timeframe 1 :sharpe 0.4 :rank :A))
         (trade-map (list (cons "A" '(((timestamp . 1) (pnl . 1.0))
                                       ((timestamp . 2) (pnl . -1.0))
                                       ((timestamp . 3) (pnl . 1.0))))
                           (cons "B" '(((timestamp . 1) (pnl . 1.0))
                                       ((timestamp . 2) (pnl . -1.0))
                                       ((timestamp . 3) (pnl . 1.0)))))))
    (multiple-value-bind (pairs diag)
        (swimmy.school::select-pair-candidates
         (list s1 s2)
         trade-map
         :per-group 2
         :min-trades 3
         :max-pairs 5
         :corr-threshold 0.2
         :rescue-threshold 0.3
         :corr-fn (lambda (xs ys) 0.25))
      (declare (ignore diag))
      (assert-true (= (length pairs) 1) "Expected rescue to include pair"))))

(deftest test-pair-selection-excludes-short-trades
  "strategies with < min trades should be excluded"
  (let* ((s1 (swimmy.school::make-strategy :name "A" :symbol "USDJPY" :timeframe 1 :sharpe 0.5 :rank :A))
         (s2 (swimmy.school::make-strategy :name "B" :symbol "USDJPY" :timeframe 1 :sharpe 0.4 :rank :A))
         (trade-map (list (cons "A" '(((timestamp . 1) (pnl . 1.0))))
                           (cons "B" '(((timestamp . 1) (pnl . 1.0))
                                       ((timestamp . 2) (pnl . -1.0))
                                       ((timestamp . 3) (pnl . 1.0)))))))
    (multiple-value-bind (pairs diag)
        (swimmy.school::select-pair-candidates
         (list s1 s2)
         trade-map
         :per-group 2
         :min-trades 3
         :max-pairs 5)
      (assert-true (null pairs) "Expected no pairs when A is short")
      (assert-true (> (getf diag :excluded) 0) "Expected excluded count"))))
```

Add these tests to `src/lisp/tests.lisp` runner list.

**Step 2: Run test to verify it fails**

Run:
```bash
sbcl --non-interactive --eval '(progn (asdf:load-system :swimmy) (swimmy.tests:test-pair-selection-rescue-mode) (swimmy.tests:test-pair-selection-excludes-short-trades) (sb-ext:quit))'
```
Expected: FAIL (pipeline not implemented).

**Step 3: Write minimal implementation**

```lisp
(defparameter *pair-corr-threshold* 0.2)
(defparameter *pair-rescue-corr-threshold* 0.3)
(defparameter *pair-max-pairs* 5)

(defun %trade-list-from-map (trade-map name)
  (cond
    ((hash-table-p trade-map) (gethash name trade-map))
    ((listp trade-map) (cdr (assoc name trade-map :test #'string=)))
    (t nil)))

(defun %pair-eval (name-a name-b series-a series-b)
  (multiple-value-bind (w1 w2)
      (inverse-vol-weights series-a series-b)
    (let* ((pnls (composite-pnls series-a series-b w1 w2))
           (score (pair-score-from-pnls pnls)))
      (append (list :pair-id (pair-id name-a name-b)
                    :a name-a :b name-b
                    :weight-a w1 :weight-b w2)
              score))))

(defun select-pair-candidates (strategies trade-map
                               &key
                                 (per-group *pair-candidate-per-group*)
                                 (min-trades *pair-min-trades*)
                                 (max-pairs *pair-max-pairs*)
                                 (corr-threshold *pair-corr-threshold*)
                                 (rescue-threshold *pair-rescue-corr-threshold*)
                                 (corr-fn #'pearson-correlation))
  (let* ((pool (pair-candidate-pool strategies :per-group per-group))
         (eligible nil)
         (excluded 0)
         (pairs nil))
    (dolist (s pool)
      (let* ((name (string (%strategy-prop s :name)))
             (trades (%trade-list-from-map trade-map name)))
        (if (and trades (>= (length trades) min-trades))
            (push (list :name name :series (trade-list->series trades)) eligible)
            (incf excluded))))
    (setf eligible (nreverse eligible))
    (labels ((build (threshold)
               (dolist (i eligible)
                 (dolist (j eligible)
                   (when (string< (getf i :name) (getf j :name))
                     (multiple-value-bind (xs ys)
                         (align-pnl-series (getf i :series) (getf j :series))
                       (let ((corr (funcall corr-fn xs ys)))
                         (when (<= (abs corr) threshold)
                           (let ((entry (%pair-eval (getf i :name) (getf j :name) xs ys)))
                             (setf pairs (cons (append entry (list :corr corr)) pairs)))))))))))
      (build corr-threshold)
      (when (and (< (length pairs) max-pairs) rescue-threshold)
        (build rescue-threshold)))
    (setf pairs (sort pairs #'> :key (lambda (p) (getf p :score))))
    (setf pairs (subseq pairs 0 (min max-pairs (length pairs))))
    (values pairs (list :total (length pool)
                        :eligible (length eligible)
                        :excluded excluded))))
```

**Step 4: Run test to verify it passes**

Run:
```bash
sbcl --non-interactive --eval '(progn (asdf:load-system :swimmy) (swimmy.tests:test-pair-selection-rescue-mode) (swimmy.tests:test-pair-selection-excludes-short-trades) (sb-ext:quit))'
```
Expected: PASS.

**Step 5: Commit**

```bash
git add src/lisp/school/school-pair-composite.lisp src/lisp/tests/pair-composite-tests.lisp src/lisp/tests.lisp
git commit -m "feat: add pair selection pipeline"
```

### Final Verification

Run:
```bash
sbcl --non-interactive --eval '(progn (asdf:load-system :swimmy) (swimmy.tests:run-all-tests) (sb-ext:quit))'
```
Expected: PASS (all tests).
