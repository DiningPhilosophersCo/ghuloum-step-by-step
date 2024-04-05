(require "asdf")
(asdf:load-asd (merge-pathnames "binary-primitives.asd" (uiop:getcwd)))
(asdf:load-system :binary-primitives)
(sb-ext:save-lisp-and-die "binary-primitives" :toplevel #'binary-primitives--main :executable t)
