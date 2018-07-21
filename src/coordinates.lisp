(defpackage :src/coordinates
  (:use :common-lisp :anaphora)
  (:export #:point
           #:make-point
           #:make-region
           #:pos-add
           #:pos-diff
           #:pos-eq
           #:mlen
           #:clen
           #:diff-kind
           #:diff-near?
           #:in-region
           #:region-dimension
           #:region-points
           #:with-coordinates
           #:inside-field?
           #:mapc-adjacent
           #:+dimensions+))

(in-package :src/coordinates)

;; well, it does not look necessary at the moment
;; let's use arrays for now
;; (defstruct (coordinate (:conc-name pos-)) x y z)

;; coordinate is an array of three elements #(x y z)

(defconstant +dimensions+ 3)
(deftype point () `(simple-array fixnum (,+dimensions+)))

(defun make-point (c1 c2 c3)
  (make-array 3 :element-type 'fixnum :initial-contents (list c1 c2 c3)))

;;(deftype region () ???)

(defun make-region (p1 p2)
  (cons p1 p2))

(defun pos-add (c1 c2)
  "Adds two coordinates"
  (aops:each #'+ c1 c2))

(defun copy-point (p)
  (make-array 3 :element-type 'fixnum :initial-contents p))

(defun pos-diff (c1 c2)
  "A coordinate difference d specifies the relative position of one coordinate
to another and is written <dx, dy, dz>, where dx, dy, and dz are (positive or
negative) integers. Adding distance d = <dx, dy, dz> to coordinate c = <x, y,
z>, written c + d, yields the coordinate <x + dx, y + dy, z + dz>."
  (aops:each #'- c1 c2))

(defun pos-eq (c1 c2)
  (equalp c1 c2))

(defun mlen (diff)
  "The Manhattan length (or L1 norm) of a coordinate difference d = <dx, dy, dz>
is written mlen(d) and defined as |dx| + |dy| + |dz| (the sum of the absolute
values of dx, dy, and dz). The Manhattan length of a coordinate difference is
always a non-negative integer."
  (reduce #'+ (aops:each #'abs diff)))

(defun clen (diff)
  "The Chessboard length (or Chebyshev distance or L∞ norm) of a coordinate
difference d = <dx, dy, dz> is written clen(d) and defined as max(|dx|, |dy|,
|dz|) (the maximum of the absolute values of dx, dy, and dz). The Chessboard
length of a coordinate difference is always a non-negative integer."
  (reduce #'max (aops:each #'abs diff)))

(defun adjacent? (c1 c2)
  (let ((d (pos-diff c1 c2)))
    (= (mlen d) 1)))

(defun mapc-adjacent (c r func)
  (loop :for component :below 3
     :do (loop :for d :in '(-1 1)
            :do (let ((c1 (copy-point c)))
                  (setf (aref c1 component)
                        (+ d (aref c1 component)))
                  (when (inside-field? c1 r)
                    (funcall func c1))))))

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

;; region is a list of two coordinates (c1 c2)

(defun in-region (c r)
  "Checks wether coordinate `c' is a member of a region `r'"
  (destructuring-bind (c1 c2) r
    (labels ((%check (i)
               (and (>= (aref c i)
                        (min (aref c1 i)
                             (aref c2 i)))
                    (<= (aref c i)
                        (max (aref c1 i)
                             (aref c2 i))))))
      (loop :for i :from 0 :to 2 :do
           (unless (%check i)
             (return-from in-region nil)))
      t)))

(defun inside-field? (c r)
  "Checks wether coordinate all components of `c' 0 <= c < r"
  (labels ((%check (i)
             (and (>= (aref c i)
                      0)
                  (< (aref c i)
                     r))))
    (loop :for i :from 0 :to 2 :do
       (unless (%check i)
         (return-from inside-field? nil)))
    t))

(defmacro with-coordinates ((x y z) c-expr &body body)
  (alexandria:with-gensyms (c)
    `(let* ((,c ,c-expr)
            (,x (aref ,c 0))
            (,y (aref ,c 1))
            (,z (aref ,c 2)))
       ,@body)))

(defun region-dimension (r)
  "The dimension of a region r = [(x1, y1, z1), (x2, y2, z2)] is written dim(r)
and is defined as (x1 = x2 ? 0 : 1) + (y1 = y2 ? 0 : 1) + (z1 = z2 ? 0 :
1). That is, the dimension of a region counts the number of components that
differ. A region with dimension 0 is a “point”; a region with dimension 1 is a
“line”; a region with dimension 2 is a “plane”; and a region with dimension 3 is
a “box”."
  (destructuring-bind (c1 c2) r
    (with-coordinates (x1 y1 z1) c1
      (with-coordinates (x2 y2 z2) c2
        (+ (if (= x1 x2) 1 0)
           (if (= y1 y2) 1 0)
           (if (= z1 z2) 1 0))))))

(defun region-points (r)
  (let ((point-list nil))
    (destructuring-bind (c1 c2) r
      (with-coordinates (x1 y1 z1) c1
        (with-coordinates (x2 y2 z2) c2
          (loop :for i :from (min x1 x2) :to (max x1 x2) :do
               (loop :for j :from (min y1 y2) :to (max y1 y2) :do
                    (loop :for k :from (min z1 z2) :to (max z1 z2) :do
                         (push (make-point i j k) point-list)))))))
    point-list))


