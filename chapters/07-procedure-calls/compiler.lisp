;;  This section introduces and environment
(ql:quickload "trivia")

;; env utils
;; At the moment, all variables are in one global table
;; environment is an assoc list
(defun env-find (var env)
  "Looks up the variable in the environment"
  (alist--find 0 var env))

;; Helpers for env utils above
(defun alist--find (position key alist)
  "Helper: returns the position where key was found. Else errors"
  (trivia:match alist
    ((cons head tail)
     (trivia:match head
       ((list k v) (declare (ignore v)) (if (eq k key) position (alist--find (+ position 1) key tail)))
       ((list k) (if (eq k key) position (alist--find (+ position 1) key tail)))
       ((list k nil) (if (eq k key) position (alist--find (+ position 1) key tail)))
       (_ (error "Should have been a pair. alists are lists of pairs"))))
    (nil (error (format nil "Could not find key ~a in alist" key)))))
     
(defun env-add (variable-name variable-value env)
  "Adds a new var to the environment."
  (cons `(,variable-name . ,variable-value) env))

(defun emit-let (bindings body si env)
  "Emits the let expression"
  (emit "// begin let-expression")
  (if (null bindings)
      (progn
         (emit "// begin let-body")
         (emit-expr body si env))


      (trivia:match bindings
	((cons bindings-head bindings-tail)


	 (trivia:match bindings-head
	   ((list binding-name binding-value)

	    (let* ((new-env
		     (env-add binding-name binding-value env)))
              (emit "// begin let-binding")
              (emit-expr binding-value si env)
              (emit "str x0, [sp, #~a]" si)
	      (emit-let bindings-tail body (- si wordsize) new-env)))

	   (_ (error "Empty binding received"))))


	(_ (error "wrong bindings format")))))


(defun emit-expr (x si env)
  "Currently supports
   1. Immediate representations of integers
   2. Unary expressions
   3. Binary expressions
   4. Conditional Expressions
   5. Heap Allocation
   6. Local variables (let bindings)"
  (cond
   ((null x) (error "null value received"))
   ((integer-p x)
         (emit "// begin emit-expr immediate")
         (emit "movz x0, #~a" (immediate-rep x)))
   ((variable-p x)
    (let ((index (* (* -1 wordsize) (env-find x env))))
      (emit "// begin emit-expr variablesss")
      (emit "ldr x0, [sp, #~a]" index)
      (emit "// end emit-expr variable")))
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


(defun global-environment--add (binding global-environment)
  "Adds a tuple/pair to `global-environment' assoc list"
  (cons binding global-environment))

(defun local-environment--create (variable-list) ; list of strings
  "Creates a new local environment for a function declaration given it's parameter list"
  (assert (not (null variable-list)))
  (reduce
   #'(lambda (acc variable) (env-add variable nil acc))
   variable-list
   :initial-value nil))

;; (trivia:match (cons 1  2)
;;   ((cons a b) (print a))  ; This pattern matches a pair (a . b)
;;   (_ 'not-matched))

(defun emit-program (global-environment stack-index ast)
  (trivia:match ast
    ((list 'label-definitions definitions global-expression)
     (assert (typep definitions 'list))
     (emit "// defun")
     (reduce
      #'(lambda (global-environment definition) 
       (trivia:match definition 
         ((list label-name (list 'code variable-list code-expression))
	  (emit "~a:" (convert-label-name-to-asm (symbol-name label-name)))

	  ;; We would not try to extend the environment like we did
	  ;; before, because that would also add name duplication to
	  ;; the list of our problems, we would instead just look up
	  ;; the variable on the stack based on its index in the
	  ;; parameter list.

	  ;; so we assume the variables have been pushed onto the
	  ;; stack in the order of the parameter list, which means the
	  ;; topmost variable in the stack is the last one. ie
	  ;; (sp register - wordsize) points to the last argument
	  ;; sp - (8 * n-params) is the first one
	  ;; sp points to return address


	  ;; the paper suggests a solution to address the name
	  ;; conflicts between variables of different functions: it
	  ;; says that there should be a mapping between each function
	  ;; declaration and its parameter list. This way when if two
	  ;; different functions have the same parameter names, they
	  ;; would be still be local because the mapping would be
	  ;; different.


	  ;; We skip the part where labels are mapped to unique forms in following
	  ;; > For the labels form, a new set of unique labels are
          ;; > created and the initial environment is constructed to map
          ;; > each of the lvars to its corresponding label. 
	  ;; We assume the labels are already unique as provided by
	  ;; the user.
	  ;; So, the initial environment is constructed mapping the
	  ;; variables list to the lvars.

	  (assert (not (null variable-list)))

	  (let*
	      ((function-def `(,label-name `(,variable-list ,code-expression)))
	       (new-global-environment
		 (global-environment--add function-def global-environment))
	       (local-environment (local-environment--create variable-list))) ;; 
	    (emit-expr code-expression stack-index local-environment)
	    new-global-environment)
	  (emit "// TODO global expression ~a" global-expression)
	  )
         (t (error "Invalid syntax around definition"))))
      definitions
      :initial-value global-environment))
    (t
     (emit "// invalid ast"))))

(defun procedure-calls--main ()
  (let ((global-environment nil)
	(stack-index 0))
    (emit-program
     global-environment
     stack-index '(label-definitions
			((foo-fn (code (a b) (+ a b)))
			 (bar-fn (code (a b) (- a b))))
			(+ (foo-fn 3 4) (bar-fn 5 6))))
    (emit ".global _scheme_entry")
    (emit "_scheme_entry:")
    (emit "mov x27, x0") ;; x27 is our allocation pointer
    (emit "mov x0, #0")
    (emit "ret")))

