(uiop:define-package :src/trivial
    (:use :common-lisp)
  (:shadow #:fill)
  (:use
   :src/coordinates
   :src/state
   :src/commands
   :src/model)
  )

(in-package :src/trivial)

(defun compute-model-bounding-box (model)
  (with-slots (coordinates) model
    (let ((x1 (apply #'min (mapcar (lambda (c) (aref c 0)) coordinates)))
          (x2 (apply #'max (mapcar (lambda (c) (aref c 0)) coordinates)))
          (y1 (apply #'min (mapcar (lambda (c) (aref c 1)) coordinates)))
          (y2 (apply #'max (mapcar (lambda (c) (aref c 1)) coordinates)))
          (z1 (apply #'min (mapcar (lambda (c) (aref c 2)) coordinates)))
          (z2 (apply #'max (mapcar (lambda (c) (aref c 2)) coordinates))))
      (make-region (make-point x1 y1 z1)
                   (make-point x2 y2 z2)))))

(defmethod generate-trace ((tracer (eql :trivial)) model)
  (let ((state (src/model:make-pseudo-state-from-model model))
        (commands nil))

    (when (eq (state-harmonics state) :low)
      (push (make-instance 'flip) commands))

    (labels ((%move-to-and-fill (x y z)
               (let ((bot-pos (make-point x (1+ y) z))
                     (c (make-point x y z)))
                 (when (voxel-void? state c)
                   (gen-moves-to bot-pos)
                   (push (make-instance 'fill)))))))
    (destructuring-bind (c1 . c2) (compute-model-bounding-box model)
      (with-coordinates (x1 y1 z1) c1
        (with-coordinates (x2 y2 z2) c2
          (loop :for y :from y1 :to y2 :do
               (loop :for x :from x1 :to x2 :do
                    (if (oddp x)
                        (loop :for z :from z1 :to z2 :do
                             (%move-to-and-fill x y z))
                        (loop :for z :from z1 :to z2 :do
                             (%move-to-and-fill x y z))))))))
    
    commands))
