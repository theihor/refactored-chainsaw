(defpackage :src/main
  (:nicknames :main)
  (:use :common-lisp :anaphora
        :src/execution))

(in-package :src/main)

(defun main () (format t "Hello, world!"))

