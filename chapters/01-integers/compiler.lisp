(defun compile-program (x)
  "Currently only support immediate representations of integers"
  (emit "movz x0, #~a" (immediate-rep x))
  (emit "ret"))

(defun integers--main() 
  (emit ".global _scheme_entry")
  (emit "_scheme_entry:")
  (compile-program 1024))
