(uiop:define-package :src/trivial
    (:use :common-lisp)
  (:shadowing-import-from :src/commands #:fill)
  (:use :src/coordinates
        :src/state
        :src/commands
        :src/model))

(in-package :src/trivial)

(defun get-shortest-path (p1 p2)
  (let ((nd (pos-diff p2 p1)))
    (let* ((x (aref p1 0))
           (y (aref p1 1))
           (z (aref p1 2))
           (dx (aref nd 0))
           (dy (aref nd 1))
           (dz (aref nd 2))
           (sorted (sort (list (cons :x dx) (cons :y dy) (cons :z dz))
                         #'>
                         :key (lambda (el) (abs (cdr el)))))
           (sorted (remove-if (lambda (el) (= (cdr el) 0)) sorted)))
      (when sorted
        (let* ((fst (car sorted))
               (fnum (cdr fst))
               (fabs (abs fnum))
               (fdim (car fst)))
          (cond
            ((or (> fabs 5)
                 (/= (length sorted) 2))
             (let ((step (min 15 fabs)))
               (cons (list fdim (if (> fnum 0) step (- step)))
                     (get-shortest-path
                      (make-point
                       (if (eq fdim :x) (+ x step) x)
                       (if (eq fdim :y) (+ y step) y)
                       (if (eq fdim :z) (+ z step) z))
                      p2))))
            (t (let* ((snd (second sorted))
                      (snum (cdr snd))
                      (sdim (car snd)))
                 (cons (list fdim sdim fnum snum)
                       nil)))))))))

(defun gen-move-commands (c1 c2)
  (let ((moves (get-shortest-path c1 c2)))
    (loop :for move :in moves :collect
         (ecase (length move)
           (2 (make-instance 'smove
                             :lld (ecase (first move)
                                    (:x (make-point (second move) 0 0))
                                    (:y (make-point 0 (second move) 0))
                                    (:z (make-point 0 0 (second move))))))
           (4 (make-instance 'lmove
                             :sld1 (ecase (first move)
                                     (:x (make-point (third move) 0 0))
                                     (:y (make-point 0 (third move) 0))
                                     (:z (make-point 0 0 (third move))))
                             :sld2 (ecase (second move)
                                     (:x (make-point (fourth move) 0 0))
                                     (:y (make-point 0 (fourth move) 0))
                                     (:z (make-point 0 0 (fourth move))))
                             ))))))

(defun moves-in-clear-space (c1 c2)
  (let ((diff (pos-diff c2 c1))
        (moves nil))
    (with-coordinates (dx dy dz) diff
      (loop :until (and (= dx 0) (= dy 0) (= dz 0)) :do
           (format t "~A ~A ~A~%" dx dy dz)
           (let ((adx (abs dx))
                 (ady (abs dy))
                 (adz (abs dz))
                 (sdx (signum dx))
                 (sdy (signum dy))
                 (sdz (signum dz)))
             (cond
               ((> adx 15)
                (push (make-instance 'smove :lld (make-point (* sdx 15) 0 0)) moves)
                (decf dx (* sdx 15)))
               ((> ady 15)
                (push (make-instance 'smove :lld (make-point 0 (* sdy 15) 0)) moves)
                (decf dy (* sdy 15)))
               ((> adz 15)
                (push (make-instance 'smove :lld (make-point 0 0 (* sdz 15))) moves)
                (decf dz (* sdz 15)))
               ((and (<= adx 15) (> adx 5))
                (push (make-instance 'smove :lld (make-point dx 0 0)) moves)
                (setf dx 0))
               ((and (<= ady 15) (> ady 5))
                (push (make-instance 'smove :lld (make-point 0 dy 0)) moves)
                (setf dy 0))
               ((and (<= adz 15) (> adz 5))
                (push (make-instance 'smove :lld (make-point 0 0 dz)) moves)
                (setf dz 0))
               ((= dx 0)
                (push (make-instance 'lmove :sld1 (make-point 0 dy 0) :sld2 (make-point 0 0 dz)) moves)
                (setf dy 0) (setf dz 0))
               ((= dy 0)
                (push (make-instance 'lmove :sld1 (make-point dx 0 0) :sld2 (make-point 0 0 dz)) moves)
                (setf dx 0) (setf dz 0))
               ((= dz 0)
                (push (make-instance 'lmove :sld1 (make-point dx 0 0) :sld2 (make-point 0 dy 0)) moves)
                (setf dx 0) (setf dy 0))
               (t ;; (0 < dx <= 5) and (0 < dy <= 5) and (0 < dz <= 5)
                (push (make-instance 'lmove :sld1 (make-point dx 0 0) :sld2 (make-point 0 0 dz)) moves)
                (push (make-instance 'smove :lld (make-point 0 dy 0)) moves)
                (setf dx 0) (setf dy 0) (setf dz 0))))))
    (reverse moves)))

(defun compute-model-bounding-box (model)
  (with-slots (coordinates) model
    (let ((x1 (apply #'min (mapcar (lambda (c) (aref c 0)) coordinates)))
          (x2 (apply #'max (mapcar (lambda (c) (aref c 0)) coordinates)))
          (y1 (apply #'min (mapcar (lambda (c) (aref c 1)) coordinates)))
          (y2 (apply #'max (mapcar (lambda (c) (aref c 1)) coordinates)))
          (z1 (apply #'min (mapcar (lambda (c) (aref c 2)) coordinates)))
          (z2 (apply #'max (mapcar (lambda (c) (aref c 2)) coordinates))))
      (make-region (make-point x1 y1 z1)
                   (make-point x2 y2 z2)))))

(defmethod generate-trace ((tracer (eql :trivial)) model)
  (let ((state (src/model:make-pseudo-state-from-model model))
        (commands nil)
        (bot-pos #(0 0 0)))

    (when (eq (state-harmonics state) :low)
      (push (make-instance 'flip) commands))

    (labels ((%move-to-and-fill (x y z)
               (let ((new-bot-pos (make-point x (1+ y) z))
                     (c (make-point x y z)))
                 (when (voxel-void? state c)
                   (setf commands (append
                                   (reverse (gen-move-commands
                                             bot-pos new-bot-pos))
                                   commands))
                   (setf bot-pos new-bot-pos)
                   (push (make-instance 'fill) commands)))))
      (destructuring-bind (c1 . c2) (compute-model-bounding-box model)
        (with-coordinates (x1 y1 z1) c1
          (with-coordinates (x2 y2 z2) c2
            (loop :for y :from y1 :to y2 :do
                 (loop :for x :from x1 :to x2 :do
                      (if (oddp x)
                          (loop :for z :from z1 :to z2 :do
                               (%move-to-and-fill x y z))
                          (loop :for z :from z2 :downto z1 :do
                               (%move-to-and-fill x y z)))))))))

    (setf commands (append (reverse (gen-move-commands bot-pos #(0 0 0)))
                           commands))

    (when (eq (state-harmonics state) :high)
      (push (make-instance 'flip) commands))

    (push (make-instance 'halt) commands)

    commands))
