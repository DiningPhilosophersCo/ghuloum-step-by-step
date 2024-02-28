(require "asdf")
(asdf:load-asd (merge-pathnames "gh-integers.asd" (uiop:getcwd)))
(asdf:load-system :gh-integers)
(sb-ext:save-lisp-and-die "gh-integers" :toplevel #'integers--main :executable t)
