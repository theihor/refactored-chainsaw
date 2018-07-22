(uiop:define-package :src/destructor
    (:use :common-lisp
          :src/model
          :src/state
          :src/coordinates)
  (:export #:build-ground-dep-graph))

(in-package :src/destructor)


(defconstant +full+ 1)
(defconstant +void+ 0)

(defconstant +grounded+ 1)

(defun is-voxel-state-full (voxel-state)
  (= voxel-state +full+))

(defun 0th-floor-ground-checker (coordinate matrix resolution)
  (is-voxel-state-full
   (voxel-state coordinate matrix resolution)))

(defun make-0th-floor-ground-action (grounded-matrix dep-graph)
  (lambda (coordinate matrix resolution)
    (declare (ignore matrix))
    (let ((current-matrix-index (matrix-index coordinate resolution)))
      (setf (aref grounded-matrix current-matrix-index)
            +grounded+)
      (cl-graph:add-vertex dep-graph current-matrix-index))))

(defun is-there-grounded-neighbour (coordinate grounded-matrix resolution)
  (let (result)
    (mapc-adjacent
     coordinate
     resolution
     (lambda (neighbour-coordinate)
       (let ((neighbour-index
              (matrix-index neighbour-coordinate resolution)))
         (setf result
               (or result
                   (= (aref grounded-matrix neighbour-index) +grounded+))))))
    result))

(defun get-grounded-neighbour (coordinate grounded-matrix resolution)
  (let (neighbour)
    (mapc-adjacent
     coordinate
     resolution
     (lambda (neighbour-coordinate)
       (let ((neighbour-index
              (matrix-index neighbour-coordinate resolution)))
         (setf neighbour
               (or neighbour
                   (when (= (aref grounded-matrix neighbour-index) +grounded+)
                     neighbour-index))))))
    neighbour))

(defun make-ground-checker (grounded-matrix)
  (lambda (coordinate matrix resolution)
    (and (is-voxel-state-full (voxel-state coordinate matrix resolution))
         (get-grounded-neighbour coordinate grounded-matrix resolution))))

(defun make-ground-action (grounded-matrix dep-graph)
  (lambda (coordinate matrix resolution)
    (declare (ignore matrix))
    (let ((current-index (matrix-index coordinate resolution))
          (neighbour-index
           (get-grounded-neighbour coordinate grounded-matrix resolution)))
      (setf (aref grounded-matrix current-index) +grounded+)
      (cl-graph:add-vertex dep-graph current-index)
      (cl-graph:add-edge-between-vertexes dep-graph current-index neighbour-index :edge-type :directed))))

(defun mapc-floor (y matrix resolution test-fn action-fn)
  "Traverses all voxels on 'floor' Y that passes TEST-FN and preforms ACTION-FN
on them.
TEST-FN and ACTION-FN have type (coordinate -> matrix -> resolution -> ())"
  (loop :for x :from 0 :below resolution :do
     (loop :for z :from 0 :below resolution
        :for coordinate = (make-point x y z)
        :when (funcall test-fn coordinate matrix resolution)
        :do (funcall action-fn coordinate matrix resolution))))

(defun mark-0th-floor-grounded (resolution matrix grounded-matrix dep-graph)
  (mapc-floor
   0
   matrix
   resolution
   #'0th-floor-ground-checker
   (make-0th-floor-ground-action grounded-matrix dep-graph)))

(defun mark-non-0-floor-grounded (y matrix resolution grounded-matrix dep-graph)
  (mapc-floor
   y
   matrix
   resolution
   (make-ground-checker grounded-matrix)
   (make-ground-action grounded-matrix dep-graph)))

(defun mark-grounded-bottom-up (resolution matrix grounded-matrix dep-graph)
  "Traverses matrix botom up from y = 1 to y = (resolution - 1)"
  (loop :for y from 1 :below resolution :do
     (mark-non-0-floor-grounded y matrix resolution grounded-matrix dep-graph)))

(defun mark-grounded-top-down (resolution matrix grounded-matrix dep-graph)
  "Traverses matrix top down from y = (resolution - 1) to y = 1"
  (loop :for y :from (1- resolution) :downto 1 :do
     (mark-non-0-floor-grounded y matrix resolution grounded-matrix dep-graph)))

(defun is-grounded (index grounded-matrix)
  (= (aref grounded-matrix index) +grounded+))

(defconstant +visited+ 1)
(defun is-visited (index visited-matrix)
  (= (aref visited-matrix index) +visited+))

(defun is-full (index matrix)
  (= (aref matrix index) +full+))

(defun mark-coordinates-as-grounded(roots matrix grounded-matrix visited-matrix resolution dep-graph)
  ;; ROOTS are coordinates
  (when roots
    (let (future-roots)
      (dolist (root roots)
        (let ((root-index (matrix-index root resolution)))
          (mapc-adjacent
           root
           resolution
           (lambda (neighbour-coordinate)
             (let ((neighbour-index
                    (matrix-index neighbour-coordinate resolution)))
               (when (and (is-full neighbour-index matrix)
                          (not (is-visited neighbour-index visited-matrix)))
                 (setf (aref grounded-matrix neighbour-index) +grounded+)
                 (setf (aref visited-matrix neighbour-index) +visited+)
                 (push neighbour-coordinate future-roots)
                 (cl-graph:add-vertex dep-graph neighbour-index)
                 (cl-graph:add-edge-between-vertexes dep-graph neighbour-index root-index)))))))
      (mark-coordinates-as-grounded future-roots matrix grounded-matrix visited-matrix resolution dep-graph))))

(defun visit-0th-floor (matrix grounded-matrix visited-matrix resolution dep-graph)
  (let (result)
    (loop :for x :from 0 :below resolution :do
       (loop :for z :from 0 :below resolution
          :for coordinate = (make-point x 0 z)
          :for index = (matrix-index coordinate resolution)
          :do
          (when (is-full index matrix)
            (setf (aref grounded-matrix index) +grounded+)
            (setf (aref visited-matrix index) +visited+)
            (push coordinate result)
            (cl-graph:add-vertex dep-graph index))))
    result))

(defun build-ground-dep-graph (model)
  "Builds ground dependence graph.
Vertex A points to vertex B (A -> B) if voxel A is grounded because voxel B is grounded.
If voxel A is grounded because of several voxels, then build just a single edge (dependence)."
  (let* ((matrix (model-matrix model))
         (resolution (model-resolution model))
         ;; Used to track grounded voxels
         (grounded-matrix (make-array (length matrix) :element-type 'bit))
         (visited-matrix (make-array (length matrix) :element-type 'bit))
         (dep-graph (cl-graph:make-graph 'cl-graph:graph-container :default-edge-type :directed))
         (roots (visit-0th-floor matrix grounded-matrix visited-matrix resolution dep-graph)))
    (mark-coordinates-as-grounded roots matrix grounded-matrix visited-matrix resolution dep-graph)
    dep-graph))

(defun traverse-vortextes-in-destruction-order (dep-graph)
  "Traverses DEP-GRAPH in order that allows safe destruction in low mode"
  (loop :for roots = (cl-graph:graph-roots dep-graph)
     :while roots
     :do
     (dolist (root roots)
       (format t "kill vertex ~A~%" root)
       (cl-graph:delete-vertex dep-graph root)))
  dep-graph)
