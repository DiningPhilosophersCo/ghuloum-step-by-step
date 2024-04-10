(load "../../build-common.lisp")
(load-chapter "unary-primitives")
(sb-ext:save-lisp-and-die "unary-primitives" :toplevel #'unary-primitives--main :executable t)
