(defun emit (asm &rest args)
    (apply #'format (cons t (cons asm args)))
    (format t "~%"))

(defun compile-program (x)
  (emit "movl $~a, %eax" x)
  (emit "ret"))

(defun main() (compile-program  42))