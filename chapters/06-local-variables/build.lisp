(require "asdf")
(asdf:load-asd (merge-pathnames "local-variables.asd" (uiop:getcwd)))
(asdf:load-system :local-variables)
(sb-ext:save-lisp-and-die "local-variables" :toplevel #'local-variables--main :executable t)
