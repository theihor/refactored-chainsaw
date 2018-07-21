(uiop:define-package :src/path
    (:use :common-lisp
          :src/coordinates
          :src/state)
  (:import-from :alexandria))

(in-package :src/path)

(defstruct move
  kind
  diffs)

(defun one-move-list ()
  (let ((tab (make-hash-table :test #'equalp)))
    (loop :for comp1 :below 3
       :do (loop :for val1 :from -5 :to 5
              :unless (zerop val1)
              :do (loop :for comp2 :below 3
                     :unless (= comp1 comp2)
                     :do (loop :for val2 :from -5 :to 5
                            :unless (zerop val2)
                            :do (let ((d1 (make-point 0 0 0))
                                      (d2 (make-point 0 0 0)))
                                  (setf (aref d1 comp1) val1)
                                  (setf (aref d2 comp2) val2)
                                  (let ((pos (pos-add d1 d2)))
                                    (setf (gethash pos tab)
                                          (make-move :kind :short
                                                     :diffs (list d1 d2)))))))))
    (loop :for comp :below 3
       :do (loop :for val :from -15 :to 15
              :unless (zerop val)
              :do (let ((d (make-point 0 0 0)))
                    (setf (aref d comp) val)
                    (setf (gethash d tab)
                          (make-move :kind :long
                                     :diffs (list d))))))
    (alexandria:hash-table-alist tab)))

(defun try-moves (coord check-func register-func)
  (labels ((%one-dir (coord component iter sign num-moves func)
             (when (<= iter num-moves)
               (let ((delta (make-point 0 0 0)))
                 (setf (aref delta component) (* iter sign))
                 (let ((c1 (pos-add coord delta)))
                   (when (funcall check-func c1)
                     (funcall c1 iter component)
                     (%one-dir c1 component (1+ iter) sign num-moves func))))))
           (%try-l-move (coord component)
             (loop :for sign :in '(-1 1)
                :do (loop :for comp1 :below 3
                       :unless (= comp1 component)
                       :do (%one-dir coord comp1 1 sign 5
                                     (lambda (c1 i1 comp2)
                                       (declare (ignore i1 comp2))
                                       (funcall register-func (list coord c1))))))))
    (loop :for sign :in '(-1 1)
       :do (loop :for comp :below 3
              :do (%one-dir coord comp 1 sign 15
                            (lambda (coord1 iter1 comp1)
                              (funcall register-func (list coord1))
                              (when (<= iter1 5)
                                (%try-l-move coord1 comp1))))))))

(defun coords-to-move (orig-coord coord-list)
  (if (null (cdr coord-list))
      (make-move :kind :strait
                 :diffs (list (pos-diff (first coord-list) orig-coord)))
      (make-move :kind :l
                 :diffs (list (pos-diff (first coord-list) orig-coord)
                              (pos-diff (second coord-list) (first coord-list))))))

(defun shortest-path (coord targets state)
  (let ((wave (make-hash-table :test #'equalp)))
    (labels ((%iter (coords)
               (let ((new-front nil))
                 (loop :for coord :in coords
                    :do (if (gethash coord targets)
                            (return (cdr (reverse (gethash wave coord))))
                            (try-moves
                             coord
                             (lambda (coord1)
                               (not (voxel-full? state coord1)))
                             (lambda (coord-list)
                               (let ((coord1 (car (last coord-list))))
                                 (when (null (gethash coord1 wave))
                                   (push coord1 new-front)
                                   (setf (gethash coord1 wave)
                                         (cons (coords-to-move coord coord-list)
                                               (gethash coord wave)))))))))
                 (when new-front
                   (%iter new-front)))))
      (setf (gethash coord wave)
            :start)
      (%iter (list coord)))))

(defun initial-to-fill-set (model)
  (let ((tab (make-hash-table :test #'equalp)))
    (loop :for x :below (state-r model)
       :do (loop :for z :below (state-r model)
              :do (let ((c (make-point x 0 z)))
                    (when (voxel-full? model c)
                      (setf (gethash c tab) t)))))
    tab))

(defun fill-set-to-target-set (fill-set r)
  (let ((tab (make-hash-table :test #'equalp)))
    (maphash (lambda (coord v)
               (declare (ignore v))
               (mapc-adjacent
                coord r
                (lambda (coord1)
                  (unless (gethash coord1 fill-set)
                    (setf (gethash coord1 tab) t)))))
             fill-set)
    tab))

(defun update-to-fill (coord to-fill state model)
  (remhash coord to-fill)
  (mapc-adjacent
   coord (state-r state)
   (lambda (coord1)
     (when (and (voxel-full? model coord1)
                (voxel-void? state coord1))
       (setf (gethash coord1 to-fill) t)))))

;; (defun update-target-set (filled-coord target-set state model)
;;   (remhash filled-coord to-fill)
;;   )
