(uiop:define-package :src/tracer
    (:use :common-lisp)
  (:shadow #:fill)
  (:use
   :src/coordinates
   :src/state
   :src/commands)
  (:export #:generate-trace))

(in-package :src/tracer)

(defgeneric generate-trace (tracer model)
  (:documentation "Dispatch trace generation for a given `model'
   on `tracer' type. Return list of command objects."))

(defmethod generate-trace ((tracer (eql :trivial)) model)
  ;; TODO(theihor): implement
  )

(defmethod generate-trace ((tracer (eql :parallel)) model)
  ;; TODO(whythat): implement
  )
