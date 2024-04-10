(load "../../build-common.lisp")
(load-chapter "local-variables")
(sb-ext:save-lisp-and-die "local-variables" :toplevel #'local-variables--main :executable t)
