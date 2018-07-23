(uiop:define-package :src/coordinates-helper
    (:use :common-lisp)
  (:shadowing-import-from :src/commands #:fill)
  (:use :src/coordinates
        :src/state
        :src/commands)
  (:export #:compute-model-bounding-box
           #:get-clusters
           #:moves-in-clear-space))

(in-package :src/coordinates-helper)

(defparameter *bot-number* 40)

(defun moves-in-clear-space (c1 c2)
  (let ((diff (pos-diff c2 c1))
        (moves nil))
    (with-coordinates (dx dy dz) diff
      (loop :until (and (= dx 0) (= dy 0) (= dz 0)) :do
          ;; (format t "~A ~A ~A~%" dx dy dz)
           (let ((adx (abs dx))
                 (ady (abs dy))
                 (adz (abs dz))
                 (sdx (signum dx))
                 (sdy (signum dy))
                 (sdz (signum dz)))
             (cond
               ((> ady 15)
                (push (make-instance 'smove :lld (make-point 0 (* sdy 15) 0)) moves)
                (decf dy (* sdy 15)))
               ((> adx 15)
                (push (make-instance 'smove :lld (make-point (* sdx 15) 0 0)) moves)
                (decf dx (* sdx 15)))
               ((> adz 15)
                (push (make-instance 'smove :lld (make-point 0 0 (* sdz 15))) moves)
                (decf dz (* sdz 15)))
               ((and (<= ady 15) (> ady 5))
                (push (make-instance 'smove :lld (make-point 0 dy 0)) moves)
                (setf dy 0))
               ((and (<= adx 15) (> adx 5))
                (push (make-instance 'smove :lld (make-point dx 0 0)) moves)
                (setf dx 0))
               ((and (<= adz 15) (> adz 5))
                (push (make-instance 'smove :lld (make-point 0 0 dz)) moves)
                (setf dz 0))
               ((= dy 0)
                (if (and (/= dx 0) (/= dz 0))
                    (push (make-instance 'lmove :sld1 (make-point dx 0 0) :sld2 (make-point 0 0 dz)) moves)
                    (if (/= dx 0)
                        (push (make-instance 'smove :lld (make-point dx 0 0)) moves)
                        (push (make-instance 'smove :lld (make-point 0 0 dz)) moves)))
                (setf dx 0) (setf dz 0))
               ((= dx 0)
                (if (and (/= dy 0) (/= dz 0))
                    (push (make-instance 'lmove :sld1 (make-point 0 dy 0) :sld2 (make-point 0 0 dz)) moves)
                    (if (/= dy 0)
                        (push (make-instance 'smove :lld (make-point 0 dy 0)) moves)
                        (push (make-instance 'smove :lld (make-point 0 0 dz)) moves)))
                (setf dy 0) (setf dz 0))
               ((= dz 0)
                (if (and (/= dx 0) (/= dy 0))
                    (push (make-instance 'lmove :sld1 (make-point dx 0 0) :sld2 (make-point 0 dy 0)) moves)
                    (if (/= dx 0)
                        (push (make-instance 'smove :lld (make-point dx 0 0)) moves)
                        (push (make-instance 'smove :lld (make-point 0 dy 0)) moves)))
                (setf dx 0) (setf dy 0))
               (t ;; (0 < dx <= 5) and (0 < dy <= 5) and (0 < dz <= 5)
                (push (make-instance 'lmove :sld1 (make-point dx 0 0) :sld2 (make-point 0 0 dz)) moves)
                (push (make-instance 'smove :lld (make-point 0 dy 0)) moves)
                (setf dx 0) (setf dy 0) (setf dz 0))))))
    (reverse moves)))

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
         (pd (pos-diff c2 c1))
         (state-r (state-r state))
         (max (1- state-r)))
    (with-coordinates (x1 y1 z1) c1
      (with-coordinates (x2 y2 z2) c2
        (with-coordinates (bdx bdy bdz) pd
          (labels ((%get-clusters (mult)
                     (let* ((area (* bdx bdz mult))
                            (cluster-size (if (> area *bot-number*)
                                              (ceiling (/ area *bot-number*))
                                              1))
                            (r (ceiling (sqrt cluster-size)))
                            ;; (x-cl (ceiling (/ bdx r)))
                            ;; (z-cl (ceiling (/ bdz r)))
                            ;; (cluster-number (* x-cl z-cl))
                            (total-voxels 0)
                            (clusters-ht (make-hash-table :test #'equalp)))
                       (loop :for x :from x1 :to x2 :by r
                          :do (loop :for z :from z1 :to z2 :by r
                                 :do (let* ((dx (min (1- (+ x r)) max))
                                            (dz (min (1- (+ z r)) max))
                                            (cluster (make-region (make-point x 0 z)
                                                                  (make-point dx 0 dz)))
                                            (voxels 0))
                                       (loop :for y :from y1 :to y2
                                          :do (loop :for xi :from x :to dx
                                                 :do (loop :for zi :from z :to dz
                                                        :do (when (voxel-full? state (make-point xi y zi))
                                                              (incf voxels)
                                                              (incf total-voxels)))))
                                       (when (> voxels 0)
                                         (setf (gethash cluster clusters-ht) voxels)))))
                       (if (<= (hash-table-count clusters-ht) *bot-number*)
                           clusters-ht
                           (%get-clusters (* mult 1.1))))))
            (%get-clusters 1)))))))
