(uiop:define-package :src/layered-bot
    (:use :common-lisp
          :src/coordinates
          :src/state
          :src/commands
          :src/tracer
          :src/wave-model
          :src/model)
  (:import-from :alexandria)
  (:shadowing-import-from :src/commands
                          #:fill))

(in-package :src/layered-bot)

(defun try-moves (coord check-func register-func)
  (labels ((%one-dir (coord component iter sign num-moves func)
             (when (<= iter num-moves)
               (let ((c1 (copy-point coord)))
                 (incf (aref c1 component) sign)
                 (when (funcall check-func c1)
                   (funcall func c1 iter component)
                   (%one-dir c1 component (1+ iter) sign num-moves func)))))
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

(defun coords-to-command (orig-coord coord-list)
  (if (null (cdr coord-list))
      (make-instance 'smove :lld (pos-diff (first coord-list) orig-coord))
      (make-instance 'lmove
                     :sld1 (pos-diff (first coord-list) orig-coord)
                     :sld2 (pos-diff (second coord-list) (first coord-list)))))

(defun shortest-path (start-coord targets state)
  (let ((wave (make-hash-table :test #'equalp)))
    (labels ((%iter (coords)
               ;; (format t "Front: ~A~%" coords)
               (let ((new-front nil))
                 (let ((found-list (loop :for coord :in coords
                                      :append (if (gethash coord targets)
                                                  (list coord)
                                                  nil))))
                   (when found-list
                     (let ((the-one nil)
                           (the-one-mlen nil))
                       (loop :for found :in found-list
                          :do (let ((found-mlem (diff-lens start-coord found)))
                                (when (or (null the-one)
                                          (< found-mlem the-one-mlen))
                                  (setf the-one found
                                        the-one-mlen found-mlem))))
                       (return-from shortest-path
                         (values the-one
                                 (cdr (reverse (gethash the-one wave)))))))
                   (loop :for coord :in coords
                      :do (try-moves
                           coord
                           (lambda (coord1)
                             (and (inside-field? coord1 (state-r state))
                                  (not (voxel-full? state coord1))))
                           (lambda (coord-list)
                             (let ((coord1 (car (last coord-list))))
                               (when (null (gethash coord1 wave))
                                 (push coord1 new-front)
                                 (setf (gethash coord1 wave)
                                       (cons (coords-to-command coord coord-list)
                                             (gethash coord wave)))))))))
                 ;; (format t "New front: ~A~%" new-front)
                 (when new-front
                   (%iter new-front)))))
      (setf (gethash start-coord wave)
            (list :start))
      (%iter (list start-coord)))))

(defun one-target-fill (coord fill-set target-set state)
  (mapc-near
   coord (state-r state)
   (lambda (coord1)
     (unless (or (gethash coord1 fill-set)
                 (voxel-full? state coord1))
       (setf (gethash coord1 target-set) t)))))

(defun fill-set-to-target-set (fill-set state)
  (let ((tab (make-hash-table :test #'equalp)))
    (maphash (lambda (coord v)
               (declare (ignore v))
               (one-target-fill coord fill-set tab state))
             fill-set)
    tab))

(defun remove-near-from-target-set (coord fill-set target-set state)
  (let ((still-target nil))
    (mapc-near coord (state-r state)
               (lambda (c1)
                 (when (gethash c1 fill-set)
                   (setf still-target t))))
    (unless still-target
      (remhash coord target-set))))

(defun update-target-set (filled-coord fill-set target-set state)
  (remhash filled-coord target-set)
  (mapc-near
   filled-coord (state-r state)
   (lambda (coord1)
     (remove-near-from-target-set coord1 fill-set target-set state))))

(defun find-to-fill (coord fill-set state)
  (mapc-near
   coord (state-r state)
   (lambda (coord1)
     (when (gethash coord1 fill-set)
       (return-from find-to-fill coord1))))
  nil)

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
         (level-fill-set (get-wave-matrix model)))
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
                 (%commands moves))))
      (block outer
        (loop
           :for level :from 1 :to (hash-table-count level-fill-set)
           :for fill-set := (let ((tab (make-hash-table :test #'equalp))
                                  (lst (gethash level level-fill-set)))
                              (loop :for coord :in lst
                                 :do (setf (gethash coord tab) t))
                              tab)
           :for target-set := (fill-set-to-target-set fill-set state)
           :do (loop
                  :do (progn
                        (format t "Step: ~A~%" (reduce #'+ (mapcar #'length res-trace)))
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
                                (remhash coord fill-set)
                                (update-target-set coord fill-set target-set state))
                              (if (= (hash-table-count target-set) 0)
                                  (if (= (hash-table-count fill-set) 0)
                                      (return)
                                      ;; Oops
                                      (progn
                                        (format t "Target point was not found~%")
                                        (return-from outer)))
                                  (multiple-value-bind (goto-coord moves)
                                      (shortest-path bot-coord target-set state)
                                    (format t "Then goto ~A~%" goto-coord)
                                    (if goto-coord
                                        (progn
                                          (%commands moves)
                                          (remhash goto-coord target-set)
                                          (setf bot-coord goto-coord))
                                        (progn
                                          (format t "Path to target point was not found~%")
                                          (return-from outer)))))))))))
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

(defmethod generate-trace ((tracer (eql :simple-layered)) model)
  (go-sucker model))
