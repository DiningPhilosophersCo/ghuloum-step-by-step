(defun emit-expr (x si)
  "Currently supports
   1. Immediate representations of integers
   2. Unary expressions"
  (cond
   ((integer-p x) (emit "movz x0, #~a" (immediate-rep x)))
   ((primcall-p x)
     (case (primcall-op x)
       (if (emit-if (primcall-operand-1 x)
                    (primcall-operand-2 x)
                    (primcall-operand-3 x)
                   si))
       (add1 (emit-expr (primcall-operand-1 x) si)
             (emit "add x0, x0, #~a" (immediate-rep 1))
       )
       ((+) (emit-expr (primcall-operand-2 x) si)
            (emit "str x0, [sp, #~a]" si) ; Load the value from the stack into x1
            (emit-expr (primcall-operand-1 x) (- si wordsize))
            (emit "ldr x1, [sp, #~a]" si)
            (emit "add x0, x0, x1")))))) ; Add the value in x1 to x0

(defun emit-if (test conseq altern si)
  (let ((L0 (gen-label))
        (L1 (gen-label)))
    (emit-expr test si)
    (emit "cmp x0, #0")
    (emit "b.eq ~a" L0)
    (emit-expr conseq si)
    (emit "b ~a" L1)
    (emit "~a:" L0)
    (emit-expr altern si)
    (emit "~a:" L1)
  ))

(defun gen-label () 
  (let ((label (gensym "label")))
    (setf (get label 'label-name) label)
    label))

(defun conditional-expressions--main ()
  (emit ".global _scheme_entry")
  (emit "_scheme_entry:")
  (emit-expr '(if 0 3 4) 0)
  
  (emit "ret"))
