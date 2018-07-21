(uiop:define-package :src/grounded
    (:use :common-lisp
          :src/coordinates
          :src/state)
  (:import-from :cl-containers)
  (:export #:grounded-state
           #:grounded-add-voxel
           #:grounded-check))

(in-package :src/grounded)

(defclass grounded-state ()
  ((uf-container :initform (make-instance 'cl-containers:union-find-container
                                          :test #'equalp)
                 :accessor uf-container)
   (coords :initform nil
           :accessor coords)))

(defmethod init-instance :after ((gs grounded-state) &rest params &key &allow-other-keys)
  (declare (ignore params))
  (cl-containers:insert-item (uf-container gs) :ground))

(defun grounded-add-voxel (gs coord state)
  (with-slots (uf-container coords)
      gs
    (push coord coords)
    (unless (cl-containers:find-item uf-container coord)
      (cl-containers:insert-item uf-container coord))
    (mapc-adjacent
     coord (state-r state)
     (lambda (adj-coord)
       (when (not (zerop (get-voxel state adj-coord)))
         (cl-containers:graft-nodes
          (cl-containers:representative-node uf-container coord)
          (cl-containers:representative-node uf-container adj-coord)))))
    (with-coordinates (x y z) coord
      (declare (ignore x z))
      (when (= 0 y)
        (cl-containers:graft-nodes
          (cl-containers:representative-node uf-container coord)
          (cl-containers:representative-node uf-container :ground))))))

(defun grounded-check (gs)
  (with-slots (uf-container coords)
      gs
    (let ((coords-to-check coords))
      (setf coords nil)
      (loop :for coord :in coords-to-check
         :do (unless (eq (cl-containers:representative-node uf-container coord)
                         (cl-containers:representative-node uf-container :ground))
               (return-from grounded-check nil))))
    t))
