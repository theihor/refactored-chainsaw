(defpackage :src/coordinates
  (:use :common-lisp :anaphora)
  (:export #:pos-diff
           #:mlen
           #:clen
           #:diff-kind
           #:diff-near?))

;; well, it does not look necessary at the moment
;; let's use arrays for now
;; (defstruct (coordinate (:conc-name pos-)) x y z)

(defun pos-diff (c1 c2)
  "A coordinate difference d specifies the relative position of one coordinate to another and is written <dx, dy, dz>, where dx, dy, and dz are (positive or negative) integers. Adding distance d = <dx, dy, dz> to coordinate c = <x, y, z>, written c + d, yields the coordinate <x + dx, y + dy, z + dz>."
  (aops:each #'- c1 c2))

(defun mlen (diff)
  "The Manhattan length (or L1 norm) of a coordinate difference d = <dx, dy, dz> is written mlen(d) and defined as |dx| + |dy| + |dz| (the sum of the absolute values of dx, dy, and dz). The Manhattan length of a coordinate difference is always a non-negative integer."
  (reduce #'+ (aops:each #'abs diff)))

(defun clen (diff)
  "The Chessboard length (or Chebyshev distance or L∞ norm) of a coordinate difference d = <dx, dy, dz> is written clen(d) and defined as max(|dx|, |dy|, |dz|) (the maximum of the absolute values of dx, dy, and dz). The Chessboard length of a coordinate difference is always a non-negative integer."
  (reduce #'max (aops:each #'abs diff)))

(defun diff-linear? (diff)
  (let ((linear nil))
    (aops:each (lambda (x)
                 (when (/= x 0)
                   (if linear
                       (return-from diff-linear? nil)
                       (setf linear t))))
               diff)
    linear))

(defun diff-kind (diff)
  "Returns kind of coordinate difference
   :linear - for linear
   :short  - for short linear
   :long   - for long linear
   :arbitrary - for others"
  (if (diff-linear? diff)
      (let ((m (mlen diff)))
        (cond ((<= m 5) :short)
              ((<= m 15) :long)
              (t :linear)))
      :arbitrary))

(defun diff-near? (diff)
  (and (<= (mlen diff) 2)
       (= (clen diff) 1)))

