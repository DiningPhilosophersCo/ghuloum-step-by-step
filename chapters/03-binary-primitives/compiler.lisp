(defun emit-expr (x si)
  "Currently supports
   1. Immediate representations of integers
   2. Unary expressions"
  (cond
   ((integer-p x) (emit "movz x0, #~a" (immediate-rep x)))
   ((primcall-p x)
     (case (primcall-op x)
       (add1 (emit-expr (primcall-operand-1 x) si)
             (emit "add x0, x0, #~a" (immediate-rep 1))
       )
       ((+) (emit-expr (primcall-operand-2 x) si)
            (emit "str x0, [sp, #~a]" si) ; Load the value from the stack into x1
            (emit-expr (primcall-operand-1 x) (- si wordsize))
            (emit "ldr x1, [sp, #~a]" si)
            (emit "add x0, x0, x1")))))) ; Add the value in x1 to x0

(defun binary-primitives--main ()
  (emit ".global _scheme_entry")
  (emit "_scheme_entry:")
  (emit-expr '(+ 1 2) 0)
  (emit "ret"))
