(uiop:define-package :src/tracer
    (:use :common-lisp)
  (:shadow #:fill)
  (:use
   :src/coordinates
   :src/state
   :src/commands)
  (:export #:generate-trace))

(in-package :src/tracer)

(defgeneric generate-trace (tracer task-type src-model tgt-model)
  (:documentation "Dispatch trace generation for a given `model'
   on `tracer' type. Return list of command objects."))

(defmethod generate-trace ((tracer (eql :trivial)) task-type src-model tgt-model)
  ;; TODO(theihor): implement
  )

(defmethod generate-trace ((tracer (eql :parallel)) task-type src-model tgt-model)
  )


