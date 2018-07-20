(uiop:define-package :src/test/test
    (:use :common-lisp
          :lisp-unit))

(in-package :src/test/test)

(define-test test0
  (assert-equal t t))

