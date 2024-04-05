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
       (cons 
             (emit "mov x2, x27")
             (emit-expr (primcall-operand-1 x) si)
             (emit "str x0, [x27]")
             (emit "add x27, x27, #~a" wordsize)
             (emit-expr (primcall-operand-2 x) si) ;; We dont store the second argument in the stack like we did for the binary + operator. So we dont have to decrement the stack pointer.
             (emit "str x0, [x27]")
             (emit "add x27, x27, #~a" wordsize)
             (emit "add x0, x2, #1")
       )
       ((+) (emit-expr (primcall-operand-2 x) si)
            (emit "str x0, [sp, #~a]" si) ; Load the value from the stack into x1
            (emit-expr (primcall-operand-1 x) (- si wordsize))
            (emit "ldr x1, [sp, #~a]" si)
            (emit "add x0, x0, x1")))))) ; Add the value in x1 to x0

(defun heap-allocation--main ()
  (emit ".global _scheme_entry")
  (emit "_scheme_entry:")
  (emit "mov x27, x0") ;; x27 is our allocation pointer
  (emit-expr '(cons 1 66) 0)
  (emit "ret"))
