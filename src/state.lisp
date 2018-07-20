(defpackage :src/state
  (:use :common-lisp :anaphora
        :src/coordinates))

(in-package :src/state)

;; matrix M is an array of bits
;; 1 <=> Full
;; 0 <=> Void

(defun voxel-state (c m r)
  "Returns a state of the voxel at coordinate `c' in matrix `m' as Full (1) or Void (0).
   `r' is the resolution of the matrix"
  (with-coordinates (x y z) c
    (let ((i (+ x (* r y) (* r r z))))
      (aref m i))))

