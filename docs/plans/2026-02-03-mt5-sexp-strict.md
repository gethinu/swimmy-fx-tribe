# MT5 S-expression Strict Protocol Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 内部ZMQをS式のみで統一し、ORDER_OPENを`action/symbol/lot`に固定、MT5↔Guardian/Lispを仕様準拠で動かす。

**Architecture:** 内部ZMQのメッセージはすべてalist形式のS-expressionに統一。Lisp側はS式で生成・受信し、MT5側はS式パーサ/シリアライザを実装。JSON受理は内部ZMQから除外する。

**Tech Stack:** Common Lisp (SBCL), MQL5, ZeroMQ, existing test_runner.lisp

---

### Task 1: 正本ドキュメント更新（S式厳格化＋ORDER_OPENキー）

**Files:**
- Modify: `docs/llm/INTERFACES.md`
- Modify: `docs/llm/STATE.md`
- (Optional) Modify: `docs/llm/SPEC.md`, `docs/llm/ARCHITECTURE.md`

**Step 1: 更新内容を記述**
- INTERFACESに「内部ZMQはS式のみ、JSONは受理しない」を明記。
- ORDER_OPENのキーを`action/symbol/lot`に固定と明記。
- STATEの変更履歴に「内部ZMQをS式厳格化、ORDER_OPENキー固定」を追加。

**Step 2: 目視確認**
- `rg -n "Encoding|S-expression|JSON" docs/llm/INTERFACES.md docs/llm/STATE.md`

**Step 3: Commit**
```bash
git add docs/llm/INTERFACES.md docs/llm/STATE.md
git commit -m "docs: enforce sexp-only internal ZMQ and order_open keys"
```

---

### Task 2: TDD - ORDER_OPEN S式出力のテスト追加

**Files:**
- Modify: `src/lisp/tests.lisp`

**Step 1: 失敗テストを書く**
```lisp
(deftest test-order-open-sexp-keys
  "ORDER_OPEN uses action/symbol/lot and emits S-expression"
  (let* ((msg (swimmy.core:make-order-message "UT" "USDJPY" "BUY" 0.1 0.0 1.0 2.0
                                              :magic 123 :comment "C"))
         (payload (if (stringp msg) msg (with-output-to-string (s) (write msg :stream s)))))
    (assert-true (search "(action . \"BUY\")" payload))
    (assert-true (search "(symbol . \"USDJPY\")" payload))
    (assert-true (search "(lot . 0.1" payload))
    (assert-false (search "side" payload))
    (assert-false (search "instrument" payload))))
```

**Step 2: テスト実行（失敗確認）**
Run: `sbcl --script test_runner.lisp`
Expected: FAIL（ORDER_OPENがJSON/side/instrumentのまま）

---

### Task 3: Lisp側 ORDER_OPEN生成をS式に統一

**Files:**
- Modify: `src/lisp/core/execution-protocol.lisp`
- Modify: `src/lisp/risk-manager.lisp`

**Step 1: 最小実装**
- `make-protocol-message` をS式alistで返すように変更。
- `make-order-message` のキーを`action/symbol/lot`に変更。
- S式をZMQ送信用に文字列化するヘルパを追加（例: `sexp->string`）。

**Example Implementation Snippet**
```lisp
(defun sexp->string (form)
  (with-standard-io-syntax
    (let ((*print-case* :downcase)
          (*print-pretty* nil)
          (*print-readably* t))
      (write-to-string form))))

(defun make-protocol-message (type payload-alist)
  (let ((base `((type . ,type)
                (id . ,(generate-uuid))
                (timestamp . ,(local-time:format-timestring nil (local-time:now)
                                                           :format '(:year "-" (:month 2) "-" (:day 2) "T" (:hour 2) ":" (:min 2) ":" (:sec 2) "Z")))
                (version . ,+protocol-version+))))
    (append base payload-alist)))

(defun make-order-message (...)
  (let ((action (if (keywordp side) (string-upcase (symbol-name side)) (string-upcase side))))
    (make-protocol-message +MSG-ORDER-OPEN+
      `((strategy_id . ,strategy-id)
        (action . ,action)
        (symbol . ,symbol)
        (lot . ,(read-from-string (format nil "~,2f" lot)))
        (price . ,(float price))
        (sl . ,(float sl))
        (tp . ,(float tp))
        (magic . ,(if (numberp magic) magic (parse-integer (format nil "~a" magic) :junk-allowed t)))
        (comment . ,comment)))))
```

- `risk-manager.lisp` では `jsown:val` / `jsown:to-json` をやめ、
  `make-order-message`→`sexp->string` の出力をZMQ送信。

**Step 2: テスト実行（成功確認）**
Run: `sbcl --script test_runner.lisp`
Expected: PASS

**Step 3: Commit**
```bash
git add src/lisp/core/execution-protocol.lisp src/lisp/risk-manager.lisp src/lisp/tests.lisp
git commit -m "feat: emit order_open as sexp with action/symbol/lot"
```

---

### Task 4: Lisp側コマンド送信をS式化（typeキー統一）

**Files:**
- Modify: `src/lisp/school/school-danger.lisp`
- Modify: `src/lisp/school/school-allocation.lisp`
- Modify: `src/lisp/school/school-execution.lisp`
- Modify: `src/lisp/system/runner.lisp`
- Modify: `src/lisp/engine/positions.lisp`
- Modify: `src/lisp/core/data-client.lisp`
- Modify: `src/lisp/core/executor.lisp` (TRAINコマンド)

**Step 1: 失敗テスト追加（REQ_HISTORYがtypeでS式になること）**
```lisp
(deftest test-req-history-uses-type-sexp
  "REQ_HISTORY should be emitted as S-expression with type key"
  (let ((content (uiop:read-file-string "src/lisp/system/runner.lisp")))
    (assert-true (search "(type . \"REQ_HISTORY\")" content))
    (assert-false (search "\"action\" \"REQ_HISTORY\"" content))))
```

**Step 2: 実装**
- `jsown:to-json` を廃止し、S式文字列を直接送信。
- すべてのコマンドは `(type . "CLOSE")` 等に統一。

**Step 3: テスト実行**
Run: `sbcl --script test_runner.lisp`
Expected: PASS

**Step 4: Commit**
```bash
git add src/lisp/school/school-danger.lisp src/lisp/school/school-allocation.lisp \
  src/lisp/school/school-execution.lisp src/lisp/system/runner.lisp \
  src/lisp/engine/positions.lisp src/lisp/core/data-client.lisp src/lisp/core/executor.lisp src/lisp/tests.lisp
git commit -m "feat: emit mt5 commands as sexp with type key"
```

---

### Task 5: Lisp受信側をS式で処理（JSON拒否）

**Files:**
- Modify: `src/lisp/core/message-dispatcher.lisp`
- Modify: `src/lisp/core/executor.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: 失敗テスト追加（TICK S式を処理できる）**
```lisp
(deftest test-internal-process-msg-tick-sexp
  "internal-process-msg should process TICK S-expression"
  (let* ((fn (find-symbol "INTERNAL-PROCESS-MSG" :swimmy.main))
         (orig (symbol-function 'swimmy.main:update-candle))
         (called nil))
    (unwind-protect
        (progn
          (setf (symbol-function 'swimmy.main:update-candle)
                (lambda (bid symbol) (setf called (list bid symbol))))
          (funcall fn "((type . \"TICK\") (symbol . \"USDJPY\") (bid . 1.23) (ask . 1.24))")
          (assert-equal '(1.23 "USDJPY") called))
      (setf (symbol-function 'swimmy.main:update-candle) orig))))
```

**Step 2: 実装**
- S式分岐に `TICK/ACCOUNT_INFO/SWAP_DATA/HEARTBEAT/ORDER_ACK/TRADE_CLOSED/HISTORY` を追加。
- JSON分岐は「警告ログ + 無視」に変更。
- `executor.lisp` の `process-trade-closed` は alist 対応に変更。

**Step 3: テスト実行**
Run: `sbcl --script test_runner.lisp`
Expected: PASS

**Step 4: Commit**
```bash
git add src/lisp/core/message-dispatcher.lisp src/lisp/core/executor.lisp src/lisp/tests.lisp
git commit -m "feat: process internal ZMQ as sexp only"
```

---

### Task 6: MT5側 S式パーサ/シリアライザ導入

**Files:**
- Modify: `src/mt5/SwimmyBridge.mq5`

**Step 1: S式パース関数追加**
- `GetValueFromSexp`, `GetStringFromSexp`, `GetBoolFromSexp` を追加。
- 例: `(key . "VALUE")` / `(key . 1.23)` を抽出。

**Step 2: 受信コマンド処理の変更**
- `type` をS式から取得。
- `action/symbol/lot` に統一、文字列部分一致削除。

**Step 3: 送信データをS式に変更**
- `TICK/ACCOUNT_INFO/HISTORY/POSITIONS/SWAP_DATA/ORDER_ACK/TRADE_CLOSED` をS式で送信。
- 文字列整形は alist形式 `( (type . "TICK") (symbol . "USDJPY") ... )` へ変更。

**Step 4: 手動確認**
- MT5コンパイルでエラーがないことを確認。

**Step 5: Commit**
```bash
git add src/mt5/SwimmyBridge.mq5
git commit -m "feat(mt5): emit/parse sexp only and align order_open keys"
```

---

### Task 7: 付随ツールのS式化

**Files:**
- Modify: `tools/request_history_zmq.py`
- Modify: `tools/update_history_smart.py`

**Step 1: JSON送信をS式へ置換**
- `(type . "REQ_HISTORY") (symbol . ...) (count . N) (tf . "M1")` に変更。

**Step 2: 簡易動作確認**
- 実行ログにS式が送られていることを確認。

**Step 3: Commit**
```bash
git add tools/request_history_zmq.py tools/update_history_smart.py
git commit -m "chore: send req_history as sexp"
```

---

### Task 8: 全体テスト

**Step 1: フルテスト**
Run: `sbcl --script test_runner.lisp`
Expected: 67 passed, 0 failed

**Step 2: 変更確認**
Run: `git status -sb`
Expected: clean

**Step 3: Commit（残りがあれば）**
```bash
git add -A
git commit -m "chore: finalize sexp-only mt5 protocol"
```
