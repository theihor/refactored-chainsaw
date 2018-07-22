(uiop:define-package :src/wave-model
    (:use :common-lisp
          :src/coordinates
          :src/state)
  (:export #:get-wave-matrix))

(in-package :src/wave-model)

(defun get-wave-matrix (model)
  (let* ((r (state-r model))
         (level-ht (make-hash-table :test #'eq))
         (voxel-ht (make-hash-table :test #'equalp))
         (next-level? t))
    (loop :for x :below r
       :do (loop :for z :below r
              :do (let ((c (make-point x 0 z)))
                    (when (voxel-full? model c)
                      (push c (gethash 1 level-ht))
                      (setf (gethash c voxel-ht) 1)))))
    (loop :for lv :from 1 :by 1
       :while next-level?
       :do (let ((cur-nodes (gethash lv level-ht))
                 (nlv (1+ lv)))
             (setf next-level? nil)
             (mapc (lambda (n)
                     (mapc-adjacent n r (lambda (nn)
                                          (when (and (voxel-full? model nn)
                                                     (null (gethash nn voxel-ht)))
                                            (setf next-level? t)
                                            (setf (gethash nn voxel-ht) nlv)
                                            (push nn (gethash nlv level-ht))))))
                   cur-nodes)))
    (values level-ht
            voxel-ht)))
