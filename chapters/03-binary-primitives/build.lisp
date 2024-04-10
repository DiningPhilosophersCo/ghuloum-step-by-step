(load "../../build-common.lisp")
(load-chapter "build-primitives")
(sb-ext:save-lisp-and-die "build-primitives" :toplevel #'build-primitives--main :executable t)
