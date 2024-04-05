(require "asdf")
(asdf:load-asd (merge-pathnames "heap-allocation.asd" (uiop:getcwd)))
(asdf:load-system :heap-allocation)
(sb-ext:save-lisp-and-die "heap-allocation" :toplevel #'heap-allocation--main :executable t)
