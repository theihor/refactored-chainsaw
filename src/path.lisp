(uiop:define-package :src/path
    (:use :common-lisp
          :src/coordinates
          :src/state)
  (:use :src/commands
        :src/model)
  (:import-from :alexandria)
  (:shadowing-import-from :src/commands
                          #:fill))

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
                 (setf (aref delta component) sign)
                 (let ((c1 (pos-add coord delta)))
                   (when (funcall check-func c1)
                     (funcall func c1 iter component)
                     (%one-dir c1 component (1+ iter) sign num-moves func))))))
           (%try-l-move (coord1 component)
             (loop :for sign :in '(-1 1)
                :do (loop :for comp1 :below 3
                       :unless (= comp1 component)
                       :do (%one-dir coord1 comp1 1 sign 5
                                     (lambda (c1 i1 comp2)
                                       (declare (ignore i1 comp2))
                                       ;; (format t "Try ~A -> ~A -> ~A~%"
                                       ;;         coord coord1 c1)
                                       (funcall register-func (list coord1 c1))))))))
    (loop :for sign :in '(-1 1)
       :do (loop :for comp :below 3
              :do (%one-dir coord comp 1 sign 15
                            (lambda (coord1 iter1 comp1)
                              (funcall register-func (list coord1))
                              ;; (format t "Try ~A -> ~A~%"
                              ;;                  coord coord1)
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
               ;; (format t "Front: ~A~%" coords)
               (let ((new-front nil))
                 (loop :for coord :in coords
                    :do (if (gethash coord targets)
                            (return-from shortest-path
                              (values
                               coord
                               (cdr (reverse (gethash coord wave)))))
                            (try-moves
                             coord
                             (lambda (coord1)
                               (and (inside-field? coord1 (state-r state))
                                    (not (voxel-full? state coord1))))
                             (lambda (coord-list)
                               (let ((coord1 (car (last coord-list))))
                                 (when (null (gethash coord1 wave))
                                   (push coord1 new-front)
                                   (setf (gethash coord1 wave)
                                         (cons (coords-to-move coord coord-list)
                                               (gethash coord wave)))))))))
                 ;; (format t "New front: ~A~%" new-front)
                 (when new-front
                   (%iter new-front)))))
      (setf (gethash coord wave)
            (list :start))
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

(defun update-fill-set (coord fill-set state model)
  (remhash coord fill-set)
  (mapc-adjacent
   coord (state-r state)
   (lambda (coord1)
     (when (and (voxel-full? model coord1)
                (voxel-void? state coord1))
       (setf (gethash coord1 fill-set) t)))))

(defun remove-adj-from-target-set (coord fill-set target-set state)
  (let ((still-target nil))
    (mapc-adjacent coord (state-r state)
                   (lambda (c1)
                     (when (gethash c1 fill-set)
                       (setf still-target t))))
    (unless still-target
      (remhash coord target-set))))

(defun update-target-set (filled-coord fill-set target-set state)
  (remhash filled-coord target-set)
  (mapc-adjacent
   filled-coord (state-r state)
   (lambda (coord1)
     (remove-adj-from-target-set coord1 fill-set target-set state)
     (when (gethash coord1 fill-set)
       (mapc-adjacent
        coord1 (state-r state)
        (lambda (coord2)
          (unless (gethash coord2 fill-set)
            (setf (gethash coord2 target-set) t))))))))

(defun find-to-fill (coord fill-set state)
  (mapc-adjacent
   coord (state-r state)
   (lambda (coord1)
     (when (gethash coord1 fill-set)
       (return-from find-to-fill coord1))))
  nil)

(defun moves-to-commands (moves)
  (loop :for move :in moves
     :collect (ecase (move-kind move)
                (:strait (make-instance 'smove :lld (first (move-diffs move))))
                (:l (make-instance 'lmove
                                   :sld1 (first (move-diffs move))
                                   :sld2 (second (move-diffs move)))))))

(defun go-sucker (true-model)
  (let* ((res-trace nil)
         (bot-coord (make-point 0 0 0))
         (model (make-pseudo-state-from-model true-model))
         (r (state-r model))
         (state (make-state :r r
                            :harmonics :low
                            :matrix (make-array (* r r r)
                                                :element-type 'bit
                                                :initial-element 0)
                            :bots nil
                            :trace nil))
         (fill-set (initial-to-fill-set model))
         (target-set (fill-set-to-target-set fill-set (state-r state))))
    (labels ((%commands (commands)
               (push commands res-trace))
             (%halt ()
               (%commands (list (make-instance 'halt))))
             (%result ()
               (alexandria:mappend #'identity (reverse res-trace)))
             (%move-to (coord)
               (let* ((tab (make-hash-table :test #'equalp))
                      (dummy (setf (gethash coord tab) t))
                      (moves (nth-value 1 (shortest-path bot-coord tab state))))
                 (declare (ignore dummy))
                 (%commands (moves-to-commands moves)))))
      (loop
         :do (progn
               (format t "Bot: ~A~%" bot-coord)
               (format t "Fill set: ~A~%" (alexandria:hash-table-keys fill-set))
               (format t "Target set: ~A~%" (alexandria:hash-table-keys target-set))
               (let ((coord (find-to-fill bot-coord fill-set state)))
                 (format t "Can fill coord: ~A~%" coord)
                 (if coord
                     (progn
                       (%commands (list (make-instance 'fill
                                                       :nd (pos-diff coord bot-coord))))
                       (fill-voxel state coord)
                       (update-fill-set coord fill-set state model)
                       (update-target-set coord fill-set target-set state))
                     (if (= (hash-table-count target-set) 0)
                         (return)
                         (multiple-value-bind (goto-coord moves)
                             (shortest-path bot-coord target-set state)
                           (format t "Then goto ~A~%" goto-coord)
                           (unless goto-coord
                             (return)
                             ;; (error "AAAAAA")
                             )
                           (%commands (moves-to-commands moves))
                           (setf bot-coord goto-coord)))))))
      (unless (pos-eq bot-coord (make-point 0 0 0))
        (%move-to (make-point 0 0 0)))
      (%halt)
      (%result))))

(defun simple-pathfinder (in-file res-file)
  (let* ((true-model (read-model-from-file in-file))
         (commands (go-sucker true-model)))
    (with-open-file (stream res-file
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
      (write-sequence (encode-commands commands) stream))))
