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
  )

;; (defun get-shortest-path (p1 p2)
;;   (let ((nd (pos-diff p2 p1)))
;;     (let* ((x (aref p1 0))
;;            (y (aref p1 1))
;;            (z (aref p1 2))
;;            (dx (aref nd 0))
;;            (dy (aref nd 1))
;;            (dz (aref nd 2))
;;            (sorted (sort (list (cons :x dx) (cons :y dy) (cons :z dz))
;;                          #'>
;;                          :key (lambda (el) (abs (cdr el)))))
;;            (sorted (remove-if (lambda (el) (= (cdr el) 0)) sorted)))
;;       (when sorted
;;         (let* ((fst (car sorted))
;;                (fnum (cdr fst))
;;                (fabs (abs fnum))
;;                (fdim (car fst)))
;;           (cond
;;             ((or (> fabs 5)
;;                  (/= (length sorted) 2))
;;              (let ((step (min 15 fabs)))
;;                (cons (list fdim (if (> fnum 0) step (- step)))
;;                      (get-shortest-path
;;                       (make-point 
;;                        (if (eq fdim :x) (+ x step) x)
;;                        (if (eq fdim :y) (+ y step) y)
;;                        (if (eq fdim :z) (+ z step) z))
;;                       p2))))
;;             (t (let* ((snd (second sorted))
;;                       (snum (cdr snd))
;;                       (sdim (car snd)))
;;                  (cons (list fdim sdim fnum snum)
;;                        nil)))))))))
