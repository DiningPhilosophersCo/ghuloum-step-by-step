(require "asdf")
(asdf:load-asd (merge-pathnames "integers.asd" (uiop:getcwd)))
(asdf:load-system :integers)
(sb-ext:save-lisp-and-die "integers" :toplevel #'integers--main :executable t)
