(uiop:define-package :src/bitonic
    (:use :common-lisp
          :src/coordinates
          :src/state
          :src/wave-model
          :src/model)
  (:import-from :cl-containers)
  (:import-from :src/layered-bot
                #:a-star-path-to-dest)
  (:shadowing-import-from :src/commands
                          #:fill)
  (:export
   #:plane-points
   #:split-into-sections
   #:bitonic-tour))

(in-package :src/bitonic)

(defun plane-points (state y)
  (let ((result nil))
    (loop :for x :below (state-r state) :do
       (loop :for z :below (state-r state) :do
          (let ((point (make-point x y z)))
            (when (voxel-full? state point)
              (push point result)))))
    result))

(defun uf-extract-groups (uf points)
  (let ((repr->points (make-hash-table :test #'equalp)))
    (dolist (p points)
      (let ((repr (cl-containers:representative-node uf p)))
        (push p (gethash repr repr->points))))
    (alexandria:hash-table-values repr->points)))

(defun split-into-sections (state points)
  (let ((uf (make-instance 'cl-containers:union-find-container
                           :test #'equalp))
        (points-set (make-hash-table :test #'equalp)))
    (dolist (p points)
      (setf (gethash p points-set) t))
    (labels ((%add (coord)
               (unless (cl-containers:find-item uf coord)
                 (cl-containers:insert-item uf coord)))
             (%from-points? (p)
               (gethash p points-set)))
      (dolist (p points)
        (%add p)
        (mapc-adjacent
         p (state-r state)
         (lambda (adj)
           (when (%from-points? adj)
             (%add adj)
             (cl-containers:graft-nodes
              (cl-containers:representative-node uf p)
              (cl-containers:representative-node uf adj)))))))
    (uf-extract-groups uf points)))

(defun compare-points-xz (a b)
  (with-coordinates (xa ya za) a
    (declare (ignore ya))
    (with-coordinates (xb yb zb) b
      (declare (ignore yb))
      (or (< xa xb)
          (and (= xa xb)
               (< za zb))))))

;; http://www.cs.huji.ac.il/course/2004/algo/Solutions/bitonic.pdf
(defun bitonic-tour (points)
  (let* ((x-points
          (sort (copy-list points) #'compare-points-xz))
         (sz (length x-points))
         (p (make-array sz :initial-contents x-points))
         (l (make-array (list sz sz)))
         (N (make-array (list sz sz))))
    (labels ((%dist (i j)
               (let ((a (aref p i))
                     (b (aref p j)))
                 (with-coordinates (xa ya za) a
                   (declare (ignore ya))
                   (with-coordinates (xb yb zb) b
                     (declare (ignore yb))
                     (let ((s1 (- xa xb))
                           (s2 (- za zb)))
                       (+ (* s1 s1) (* s2 s2))))))))
      ;; Compute l(i, j) and N(i, j), for all 0 ≤ i < j < n-1
      (loop :for j :from 1 :to (- sz 1) :do
         (loop :for i :from 0 :to (- j 1) :do
            (cond
              ((and (= i 0) (= j 1))
               (setf (aref l i j) (%dist i j)
                     (aref N i j) i))
              ((> j (1+ i))
               (setf (aref l i j) (+ (aref l i (1- j))
                                     (%dist (1- j) j))
                     (aref N i j) (1- j)))
              (t
               (setf (aref l i j) nil)
               (loop :for k :from 0 :to (- i 1) :do
                  (let ((q (+ (aref l k i) (%dist k j))))
                    (when (or (null (aref l i j))
                              (< q (aref l i j)))
                      (setf (aref l i j) q
                            (aref N i j) k)))))))))
    ;; Construct the tour. Stacks S[1] and S[2] will be used to
    ;; construct the two x-monotone parts of the tour. Let S be an
    ;; array of two initially empty stacks S[1] and S[2]
    (let ((k 0)
          (i (- sz 2))
          (j (- sz 1))
          (S (make-array 2 :initial-element nil)))
      (loop :while (> j 0) :do
         (push j (aref S k))
         (setf j (aref N i j))
         (when (< j i)
           (let ((tmp i))
             (setf i j
                   j tmp))
           (setf k (if (= 0 k) 1 0))))
      (push 0 (aref S 0))
      (loop :while (aref S 1) :do
         (push (pop (aref S 1)) (aref S 0)))
      (mapcar (lambda (x) (aref p x))
              (aref S 0)))))

(defun bitonic-test ()
  (let* ((_ 0)
         (l (list
             (make-point 1 _ 2)
             (make-point 2 _ 1)
             (make-point 3 _ 2)
             (make-point 4 _ 3)
             (make-point 6 _ 2)
             (make-point 3 _ 7)
             (make-point 4 _ 2)
             )))
    (reverse (bitonic-tour l))))

(defun bitonic? (points)
  (let* ((f (first points))
         (s (second points))
         (direction (compare-points-xz f s))
         (changed-direction? nil))
    (loop
       :for (a b) :on (cdr points)
       :while b
       :do (cond
             ((eq direction (compare-points-xz a b))
              t)
             ((null changed-direction?)
              (setf changed-direction? t
                    direction (compare-points-xz a b)))
             ((return-from bitonic? nil))))
    t))

(defun bitonic-random-test (size points-num)
  (let ((points (make-hash-table :test #'equalp))
        (generated 0))
    (loop :while (< generated points-num) :do
       (let* ((x (random size))
              (z (random size))
              (p (make-point x 0 z)))
         (unless (gethash p points)
           (setf (gethash p points) t)
           (incf generated))))
    (let ((tour (bitonic-tour
                 (alexandria:hash-table-keys points))))
      (assert (bitonic? tour))
      (assert (= points-num (length tour))))
    t))

(defclass section-info ()
  ((points :accessor section-points
           :initarg :points)
   (level :accessor section-level
          :initarg :level)
   (start-point :accessor section-start-point
                :initarg :start-point)))

(defun rank-sections (state)
  (let* ((voxel->level
          (time
           (nth-value 1 (get-wave-matrix state))))
         (sections
          (time
           (loop :for y :below (state-r state) :appending
              (split-into-sections
               state
               (plane-points state y)))))
         (section-infos nil))
    (dolist (section sections)
      (let ((section-level nil)
            (start-point nil))
        (dolist (p section)
          (let ((p-level (gethash p voxel->level)))
            (when (or (null section-level)
                      (< p-level section-level))
              (setf section-level p-level
                    start-point p))))
        (push (make-instance 'section-info
                             :points section
                             :level section-level
                             :start-point start-point)
              section-infos)))
    (sort (copy-list section-infos)
          #'<
          :key #'section-level)))

(defun compute-section-tour (info)
  (let* ((points (section-points info))
         (start-index (position (section-start-point info)
                                points
                                :test #'equalp)))
    (assert start-index)
    (append
     (subseq points start-index)
     (subseq points 0 start-index))))

(defun bitonic-bot (true-model)
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
         (sections (rank-sections model))
         (total-section (length sections))
         (commands-count 0))
    (labels ((%commands (commands)
               (incf commands-count (length commands))
               (push commands res-trace))
             (%halt ()
               (%commands (list (make-instance 'halt))))
             (%result ()
               (alexandria:mappend #'identity (reverse res-trace)))
             (%move-to (coord)
               (let* ((moves (nth-value 1 (a-star-path-to-dest bot-coord coord state))))
                 (%commands moves))))
      (block outer
        (loop
           :for section :in sections
           :for section-tour := (compute-section-tour section)
           :for section-number :from 1
           :do
           (loop :for coord :in section-tour :do
              (progn
                (format t "Step      : ~A~%" commands-count)
                (format t "Section # : ~A / ~A~%" section-number total-section)
                (format t "Bot       : ~A~%" bot-coord)
                (format t "Can fill coord: ~A~%" coord)
                (multiple-value-bind (goto-coord moves)
                    (a-star-path-to-dest bot-coord coord state :near t)
                  (unless goto-coord
                    (format t "Critical critical error, no way, no way you could get there: ~A -> ~A~%"
                            bot-coord coord)
                    (return-from outer))
                  (%commands moves)
                  (setf bot-coord goto-coord)
                  (%commands (list (make-instance
                                    'fill
                                    :nd (pos-diff coord bot-coord))))
                  (fill-voxel state coord))))))
      (unless (pos-eq bot-coord (make-point 0 0 0))
        (%move-to (make-point 0 0 0)))
      (%halt)
      (%result))))

(defmethod generate-trace ((tracer (eql :simple-bitonic)) model)
  (bitonic-bot model))
