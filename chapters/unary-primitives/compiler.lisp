(defun primcall-operand-1 (expr)
  "Returns first operand of the expression"
  (nth 1 expr))

(defun primcall-p (expr)
  "Returns true if the expression `expr' is one of the
  primitives"
  (case (car expr)
    ((add1) t)
    ((sub1) t)))

(defun primcall-op (expr)
  "Returns the operation from sexp"
  (car expr))

(defun emit-expr (x)
  "Currently supports
   1. Immediate representations of integers
   2. Unary expressions"
  (cond
   ((integer-p x) (emit "movz x0, #~a" (immediate-rep x)))
   ((primcall-p x)
     (case (primcall-op x)
       (add1 (emit-expr (primcall-operand-1 x))
             (emit "add x0, x0, #~a" (immediate-rep 1))
       )))))

(defun unary-primitives--main ()
  (emit ".global _scheme_entry")
  (emit "_scheme_entry:")
  (emit-expr '(add1 1))
  (emit "ret"))
