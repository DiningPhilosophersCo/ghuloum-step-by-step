(load "../../build-common.lisp")
(load-chapter "procedure-calls")
(sb-ext:save-lisp-and-die "procedure-calls" :toplevel #'procedure-calls--main :executable t)
