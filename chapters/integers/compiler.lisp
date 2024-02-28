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
  (cond
    ((integer-p x) (ash x fixnum-shift))))

(defun compile-program (x)
  "Currently only support immediate representations of integers"
  (emit "movz x0, #~a" (immediate-rep x))
  (emit "ret"))

(defun integers--main() 
  (emit ".global _scheme_entry")
  (emit "_scheme_entry:")
  (compile-program 1024))
