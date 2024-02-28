(defun emit (asm &rest args)
    (apply #'format (cons t (cons asm args)))
    (format t "~%"))
