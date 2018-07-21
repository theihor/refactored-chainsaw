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

(defun compute-mode-bounding-box (model)
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

    commands))
