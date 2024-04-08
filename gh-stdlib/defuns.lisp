;; Refer the paper for these values. Take care to keep it in sync with the runtime written in C
(defconstant fixnum-shift 2)
(defconstant wordsize 8)

(defun integer-p (value)
  "Checks if a value is an integer.
  Args:
    value: The value to check.
  Returns:
    T if the value is an integer, nil otherwise."
  (and (numberp value) (equal (floor value) value)))

(defun immediate-rep (x)
  "Converts our toy scheme representation of integers to machine representation"
  (ash x fixnum-shift))

(defun emit (asm &rest args)
  "Assembly printer. Currently only to stdout"
  (apply #'format (cons t (cons asm args)))
  (format t "~%"))

(defun primcall-operand-1 (expr)
  "Returns first operand of the expression"
  (nth 1 expr))

(defun primcall-operand-2 (expr)
  "Returns second operand of the expression"
  (nth 2 expr))

(defun primcall-operand-3 (expr)
  "Returns third operand of the expression"
  (nth 3 expr))

(defun primcall-p (expr)
  "Returns true if the expression `expr' is one of the
  primitives"
  (case (car expr)
    ((add1) t)
    ((+) t)
    ((cons) t)
    ((if) t)))

(defun variable-p (expr)
  "Returns if the expressions is a lisp variable"
  (symbolp expr))

;; env utils
;; At the moment, all variables are in one global table
;; environment is an assoc list
(defun env-lookup (var env)
  "Looks up the variable in the environment"
  (position var env))

(defun env-extend (var env)
  "Adds a new var to the environment."
  (cons var env))

;; let expression utils.
;; 1. get bindings
;; 2. get body
(defun let-p (expr)
  "Returns if s-expression is an let-expression"
  (eq (car expr) 'let))

(defun let-bindings (expr)
  "Returns the bindings in the let-expression"
  (nth 1 expr))

(defun let-body (expr)
  "Returns the body of the let expression"
  (nth 2 expr))

(defun primcall-op (expr)
  "Returns the operation from sexp"
  (car expr))
