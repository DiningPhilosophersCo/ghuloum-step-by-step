(load "../../build-common.lisp")
(load-chapter "integers")
(sb-ext:save-lisp-and-die "integers" :toplevel #'integers--main :executable t)
