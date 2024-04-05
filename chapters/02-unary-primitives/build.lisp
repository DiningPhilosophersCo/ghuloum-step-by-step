(require "asdf")
(asdf:load-asd (merge-pathnames "unary-primitives.asd" (uiop:getcwd)))
(asdf:load-system :unary-primitives)
(sb-ext:save-lisp-and-die "unary-primitives" :toplevel #'unary-primitives--main :executable t)
