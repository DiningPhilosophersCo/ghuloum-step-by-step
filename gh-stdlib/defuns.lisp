;; Refer the paper for these values. Take care to keep it in sync with the runtime written in C
(defconstant fixnum-shift 2)

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
