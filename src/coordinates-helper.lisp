(uiop:define-package :src/coordinates-helper
    (:use :common-lisp
          :src/coordinates
          :src/state)
  (:export #:compute-model-bounding-box
           #:get-clusters))

(in-package :src/coordinates-helper)

(defparameter *bot-number* 40)

(defun compute-model-bounding-box (state)
  (with-slots (r) state
    (let ((r (1- r))
          x1 y1 z1 x2 y2 z2)

      (block x1-search
        (loop :for i :from 0 :to r :do
             (loop :for j :from 0 :to r :do
                  (loop :for k :from 0 :to r :do
                       (when (voxel-full? state (make-point i j k))
                         (setf x1 i)
                         (return-from x1-search))))))
      (unless x1 (error "Model is without any full voxels"))

      (block x2-search
        (loop :for i :from r :downto 0 :do
             (loop :for j :from 0 :to r :do
                  (loop :for k :from 0 :to r :do
                       (when (voxel-full? state (make-point i j k))
                         (setf x2 i)
                         (return-from x2-search))))))

      (block y1-search
        (loop :for j :from 0 :to r :do
             (loop :for i :from 0 :to r :do
                  (loop :for k :from 0 :to r :do
                       (when (voxel-full? state (make-point i j k))
                         (setf y1 j)
                         (return-from y1-search))))))

      (block y2-search
        (loop :for j :from r :downto 0 :do
             (loop :for i :from 0 :to r :do
                  (loop :for k :from 0 :to r :do
                       (when (voxel-full? state (make-point i j k))
                         (setf y2 j)
                         (return-from y2-search))))))

      (block z1-search
        (loop :for k :from 0 :to r :do
             (loop :for i :from 0 :to r :do
                  (loop :for j :from 0 :to r :do
                       (when (voxel-full? state (make-point i j k))
                         (setf z1 k)
                         (return-from z1-search))))))

      (block z2-search
        (loop :for k :from r :downto 0 :do
             (loop :for i :from 0 :to r :do
                  (loop :for j :from 0 :to r :do
                       (when (voxel-full? state (make-point i j k))
                         (setf z2 k)
                         (return-from z2-search))))))
      (make-region (make-point x1 y1 z1)
                   (make-point x2 y2 z2)))))

(defun get-clusters (state)
  (let* ((region (compute-model-bounding-box state))
         (c1 (car region))
         (c2 (cdr region))
         (pd (pos-diff c2 c1)))
    (with-coordinates (x1 y1 z1) c1
      (with-coordinates (x2 y2 z2) c2
        (with-coordinates (dx dy dz) pd
          (let* ((area (* dx dz))
                 (cluster-size (if (> area *bot-number*)
                                   (ceiling (/ area *bot-number*))
                                   1))
                 (cluster-r (cond
                              ((= area 1) 1)
                              ((<= area 4) 2)
                              ((<= area 9) 3)
                              (t 4)))
                 (x-cl (ceiling (/ dx cluster-r)))
                 (z-cl (ceiling (/ dz cluster-r)))
                 (cluster-number (* x-cl z-cl))
                 (total-voxels 0)
                 (clusters-ht (make-hash-table :test #'equalp)))
            (loop :for x :from x1 :to x2 :by cluster-r
               :do (loop :for z :from z1 :to z2 :by cluster-r
                      :do (let ((cluster (make-region (make-point x1 0 z1)
                                                      (make-point x2 0 z2)))
                                (voxels 0))
                            (loop :for y :from y1 :to z1
                               :do (when (voxel-full? state (make-point x y z))
                                     (incf voxels)
                                     (incf total-voxels)))
                            (setf (gethash cluster clusters-ht) voxels))))
            clusters-ht))))))
