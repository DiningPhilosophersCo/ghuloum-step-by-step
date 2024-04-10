(load "../../build-common.lisp")
(load-chapter "heap-allocation")
(sb-ext:save-lisp-and-die "heap-allocation" :toplevel #'heap-allocation--main :executable t)
