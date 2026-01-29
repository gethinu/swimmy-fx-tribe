;;; src/lisp/school/school-ast.lisp
;;; ============================================================================
;;; NATIVE AST PROTOCOL BRIDGE (Phase 23/24)
;;; ============================================================================
;;; Bridge between Lisp (Evolution) and Rust (Validation).
;;; Converts Lisp S-expressions into JSON AST nodes defined in strategy_ast.rs.
;;; 
;;; Author: Antigravity (Simons/Pg Directive)
;;; Date: 2026-01-28
;;; ============================================================================

(in-package :swimmy.school)

;;; ----------------------------------------------------------------------------
;;; AST NODE DEFS (Must match strategy_ast.rs)
;;; ----------------------------------------------------------------------------

(defun make-ast-node (type &rest args)
  "Create a hash-table representing an AST node for JSON serialization."
  (let ((node (make-hash-table :test 'equal)))
    (setf (gethash type node) args)
    node))

(defun sexp-to-ast (sexp)
  "Recursively convert a Lisp S-expression into a Rust-compatible AST JSON structure.
   
   Supported Operators:
   - Logic: AND, OR, NOT
   - Comparison: GT, LT, CROSS, CROSS-UP, CROSS-DOWN
   - Values: PRICE-CLOSE, PRICE-OPEN, PRICE-HIGH, PRICE-LOW, VOLUME, CONSTANT(n)
   - Indicators: SMA, EMA, RSI, ATR, MACD, BOLLINGER-UPPER, BOLLINGER-LOWER"
  
  (cond
    ;; 1. Constants (Numbers)
    ((numberp sexp)
     (make-ast-node "Constant" sexp))
    
    ;; 2. Atoms (Price/Volume)
    ((symbolp sexp)
     (let ((sym (string-upcase (symbol-name sexp))))
       (cond 
         ((string= sym "CLOSE") (make-ast-node "PriceClose"))
         ((string= sym "OPEN") (make-ast-node "PriceOpen"))
         ((string= sym "HIGH") (make-ast-node "PriceHigh"))
         ((string= sym "LOW") (make-ast-node "PriceLow"))
         ((string= sym "VOLUME") (make-ast-node "Volume"))
         (t (error "Unknown AST atom: ~a" sym)))))
    
    ;; 3. Compound Expressions (Lists)
    ((listp sexp)
     (let* ((op (string-upcase (symbol-name (car sexp))))
           (args (cdr sexp)))
       (cond
         ;; --- LOIGC ---
         ((string= op "AND")
          (make-ast-node "And" (sexp-to-ast (first args)) (sexp-to-ast (second args))))
         ((string= op "OR")
          (make-ast-node "Or" (sexp-to-ast (first args)) (sexp-to-ast (second args))))
         ((string= op "NOT")
          (make-ast-node "Not" (sexp-to-ast (first args))))
         
         ;; --- COMPARISON ---
         ((string= op "GT") ; >
          (make-ast-node "Gt" (sexp-to-ast (first args)) (sexp-to-ast (second args))))
         ((string= op ">")
          (make-ast-node "Gt" (sexp-to-ast (first args)) (sexp-to-ast (second args))))
         
         ((string= op "LT") ; <
          (make-ast-node "Lt" (sexp-to-ast (first args)) (sexp-to-ast (second args))))
         ((string= op "<")
          (make-ast-node "Lt" (sexp-to-ast (first args)) (sexp-to-ast (second args))))
         
         ((string= op "CROSS")
          (make-ast-node "Cross" (sexp-to-ast (first args)) (sexp-to-ast (second args))))
         ((string= op "CROSS-UP")
          (make-ast-node "CrossUp" (sexp-to-ast (first args)) (sexp-to-ast (second args))))
         ((string= op "CROSS-DOWN")
          (make-ast-node "CrossDown" (sexp-to-ast (first args)) (sexp-to-ast (second args))))
         
         ;; --- INDICATORS (Parameterized) ---
         ((string= op "SMA")
          (make-ast-node "Sma" :|period| (first args)))
         ((string= op "EMA")
          (make-ast-node "Ema" :|period| (first args)))
         ((string= op "RSI")
          (make-ast-node "Rsi" :|period| (first args)))
         ((string= op "ATR")
          (make-ast-node "Atr" :|period| (first args)))
         
         ((string= op "MACD")
          (make-ast-node "Macd" 
                         :|fast| (first args) 
                         :|slow| (second args) 
                         :|signal| (third args)))
         
         ((string= op "BOLLINGER-UPPER")
          (make-ast-node "BollingerUpper" 
                         :|period| (first args) 
                         :|std_dev| (second args)))
         ((string= op "BOLLINGER-LOWER")
          (make-ast-node "BollingerLower" 
                         :|period| (first args) 
                         :|std_dev| (second args)))
         
         (t (error "Unknown AST operator: ~a" op)))))
    
    (t (error "Invalid AST node type: ~a" sexp))))

;;; ----------------------------------------------------------------------------
;;; AST GENERATION UTILS
;;; ----------------------------------------------------------------------------

(defun generate-ast-for-strategy (strategy)
  "Generate AST maps for entry/exit conditions of a strategy.
   Returns a hash-table suitable for JSON serialization."
  (let ((ast-map (make-hash-table :test 'equal)))
    (when (strategy-entry strategy)
      (setf (gethash "entry_long" ast-map) (sexp-to-ast (strategy-entry strategy))))
    (when (strategy-exit strategy)
      (setf (gethash "exit_long" ast-map) (sexp-to-ast (strategy-exit strategy))))
    ;; Short logic support (if added later)
    ast-map))

(format t "[L] 🌳 school-ast.lisp loaded - Native AST Protocol Bridge~%")
