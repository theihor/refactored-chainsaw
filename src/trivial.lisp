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
          (x2 (apply #'min (mapcar (lambda (c) (aref c 0)) coordinates)))))))

(defmethod generate-trace ((tracer (eql :trivial)) model)
  (let ((state (src/model:make-pseudo-state-from-model model))
        (commands nil))
    (when (eq (state-harmonics state) :low)
      (push (make-instance 'flip) commands))

    commands))
