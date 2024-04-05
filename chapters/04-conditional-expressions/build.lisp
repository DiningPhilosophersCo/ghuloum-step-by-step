(require "asdf")
(asdf:load-asd (merge-pathnames "conditional-expressions.asd" (uiop:getcwd)))
(asdf:load-system :conditional-expressions)
(sb-ext:save-lisp-and-die "conditional-expressions" :toplevel #'conditional-expressions--main :executable t)
