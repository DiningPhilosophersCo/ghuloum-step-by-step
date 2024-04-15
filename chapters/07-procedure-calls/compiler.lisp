;;  This section introduces and environment
(ql:quickload "trivia")

(defun emit-let (bindings body si env)
  "Emits the let expression"
  (emit "// begin let-expression")
  (if (null bindings)
      (progn
         (emit "// begin let-body")
         (emit-expr body si env))
      (let ((bindings-head (car bindings))
            (bindings-tail (cdr bindings)))
        (let ((new-env (env-extend (car bindings-head) env)))
          (emit "// begin let-binding")
          (emit-expr (nth 1 bindings-head) si env)
          (emit "str x0, [sp, #~a]" si)
          (emit-let bindings-tail body (- si wordsize) new-env)))))

(defun emit-expr (x si env)
  "Currently supports
   1. Immediate representations of integers
   2. Unary expressions
   3. Binary expressions
   4. Conditional Expressions
   5. Heap Allocation
   6. Local variables (let bindings)"
  (cond
   ((integer-p x)
         (emit "// begin emit-expr immediate")
         (emit "movz x0, #~a" (immediate-rep x)))
   ((variable-p x)
         (emit "// begin emit-expr variable")
         (emit "ldr x0, [sp, #~a]" (- (* wordsize (env-lookup x env)))))
   ((let-p x)
          (emit "// begin emit-expr let")
          (emit-let (let-bindings x) (let-body x) si env))
   ((primcall-p x)
     (case (primcall-op x)
       (add1
             (emit "// begin emit-expr primcall: add1")
             (emit-expr (primcall-operand-1 x) si env)
             (emit "add x0, x0, #~a" (immediate-rep 1))
       )
       (cons
             (emit "// begin emit-expr primcall: cons")
             (emit "mov x2, x27")
             (emit-expr (primcall-operand-1 x) si env)
             (emit "str x0, [x27]")
             (emit "add x27, x27, #~a" wordsize)
             (emit-expr (primcall-operand-2 x) si env) ;; We dont store the second argument in the stack like we did for the binary + operator. So we dont have to decrement the stack pointer.
             (emit "str x0, [x27]")
             (emit "add x27, x27, #~a" wordsize)
             (emit "orr x0, x2, #1")
       )
       ((+) 
            (emit "// begin emit-expr primcall (+)")
            (emit-expr (primcall-operand-2 x) si env)
            (emit "str x0, [sp, #~a]" si) ; Load the value from the stack into x1
            (emit-expr (primcall-operand-1 x) (- si wordsize) env)
            (emit "ldr x1, [sp, #~a]" si)
            (emit "add x0, x0, x1")))))) ; Add the value in x1 to x0

(defun convert-label-name-to-asm (label) 
  (format nil "_~a" (substitute #\_ #\- label)))

(defun emit-program (ast)
  (trivia:match ast
    ((list 'label-definitions definitions global-expression)
     (assert (typep definitions 'list))
     (emit "// defun")
     (dolist (definition definitions) ;; Use dolist for iteration
       (trivia:match definition 
         ((list label-name (list 'code variable-list code-expression))
	  (emit "~a:" (convert-label-name-to-asm (symbol-name label-name))))
         (t (error "Invalid syntax around definition"))))
     (emit "mov x0, #0"))
    (t
     (emit "// invalid ast")
     (error "AST does not match the expected form"))))

(defun procedure-calls--main ()
  (emit-program '(label-definitions ((foo-fn (code (a b) (+ a b)))) (+ 3 4)))
  (emit ".global _scheme_entry")
  (emit "_scheme_entry:")
  (emit "mov x27, x0") ;; x27 is our allocation pointer
  (emit "ret"))


(procedure-calls--main)


