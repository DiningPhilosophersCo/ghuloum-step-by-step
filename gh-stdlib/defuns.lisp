;; Refer the paper for these values. Take care to keep it in sync with the runtime written in C
(defconstant fixnum-shift 2)
(defconstant wordsize 4)

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
    ((if) t)))

(defun primcall-op (expr)
  "Returns the operation from sexp"
  (car expr))