(require "asdf")
(asdf:load-asd (merge-pathnames "build.asd" (uiop:getcwd)))
(asdf:load-system :build)
(sb-ext:save-lisp-and-die "gh-compiler" :toplevel #'main :executable t)
