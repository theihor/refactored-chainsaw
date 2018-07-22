(uiop:define-package :src/trivial
    (:use :common-lisp)
  (:shadowing-import-from :src/commands #:fill)
  (:use :src/coordinates
        :src/state
        :src/coordinates-helper
        :src/commands
        :src/model
        :src/grounded)
  (:export #:moves-in-clear-space))

(in-package :src/trivial)

;; (defun get-shortest-path (p1 p2)
;;   (let ((nd (pos-diff p2 p1)))
;;     (let* ((x (aref p1 0))
;;            (y (aref p1 1))
;;            (z (aref p1 2))
;;            (dx (aref nd 0))
;;            (dy (aref nd 1))
;;            (dz (aref nd 2))
;;            (sorted (sort (list (cons :x dx) (cons :y dy) (cons :z dz))
;;                          #'>
;;                          :key (lambda (el) (abs (cdr el)))))
;;            (sorted (remove-if (lambda (el) (= (cdr el) 0)) sorted)))
;;       (when sorted
;;         (let* ((fst (car sorted))
;;                (fnum (cdr fst))
;;                (fabs (abs fnum))
;;                (fdim (car fst)))
;;           (cond
;;             ((or (> fabs 5)
;;                  (/= (length sorted) 2))
;;              (let ((step (min 15 fabs)))
;;                (cons (list fdim (if (> fnum 0) step (- step)))
;;                      (get-shortest-path
;;                       (make-point
;;                        (if (eq fdim :x) (+ x step) x)
;;                        (if (eq fdim :y) (+ y step) y)
;;                        (if (eq fdim :z) (+ z step) z))
;;                       p2))))
;;             (t (let* ((snd (second sorted))
;;                       (snum (cdr snd))
;;                       (sdim (car snd)))
;;                  (cons (list fdim sdim fnum snum)
;;                        nil)))))))))

;; (defun gen-move-commands (c1 c2)
;;   (let ((moves (get-shortest-path c1 c2)))
;;     (loop :for move :in moves :collect
;;          (ecase (length move)
;;            (2 (make-instance 'smove
;;                              :lld (ecase (first move)
;;                                     (:x (make-point (second move) 0 0))
;;                                     (:y (make-point 0 (second move) 0))
;;                                     (:z (make-point 0 0 (second move))))))
;;            (4 (make-instance 'lmove
;;                              :sld1 (ecase (first move)
;;                                      (:x (make-point (third move) 0 0))
;;                                      (:y (make-point 0 (third move) 0))
;;                                      (:z (make-point 0 0 (third move))))
;;                              :sld2 (ecase (second move)
;;                                      (:x (make-point (fourth move) 0 0))
;;                                      (:y (make-point 0 (fourth move) 0))
;;                                      (:z (make-point 0 0 (fourth move))))
;;                              ))))))

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
                (if (and (/= dy 0) (/= dz 0))
                    (push (make-instance 'lmove :sld1 (make-point 0 dy 0) :sld2 (make-point 0 0 dz)) moves)
                    (if (/= dy 0)
                        (push (make-instance 'smove :lld (make-point 0 dy 0)) moves)
                        (push (make-instance 'smove :lld (make-point 0 0 dz)) moves)))
                (setf dy 0) (setf dz 0))
               ((= dy 0)
                (if (and (/= dx 0) (/= dz 0))
                    (push (make-instance 'lmove :sld1 (make-point dx 0 0) :sld2 (make-point 0 0 dz)) moves)
                    (if (/= dx 0)
                        (push (make-instance 'smove :lld (make-point dx 0 0)) moves)
                        (push (make-instance 'smove :lld (make-point 0 0 dz)) moves)))
                (setf dx 0) (setf dz 0))
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

(defmethod generate-trace ((tracer (eql :trivial)) model)
  (let ((target-state (src/model:make-pseudo-state-from-model model))
        (commands nil))

    (when (eq (state-harmonics target-state) :low)
      (push (make-instance 'flip) commands))

    (let* ((region (compute-model-bounding-box target-state)))
      (loop :for move :in (moves-in-clear-space #(0 0 0) (car region)) :do
           (push move commands))
      (multiple-value-bind (moves-and-fills bot-pos)
          (generate-trivial-trace-for-region :state :gs target-state region)
        ;; (format t "bot-pos after filling: ~A~%" bot-pos)
        ;; (format t "moves to ~A: ~A~%"
        ;;         (make-point 0 (aref bot-pos 1) 0)
        ;;         (moves-in-clear-space bot-pos (make-point 0 (aref bot-pos 1) 0)))
        ;; (format t "moves to ~A: ~A~%"
        ;;         (make-point 0 0 0)
        ;;         (moves-in-clear-space (make-point 0 (aref bot-pos 1) 0) #(0 0 0)))
        (loop :for m :in (append
                          moves-and-fills
                          (moves-in-clear-space
                           bot-pos (make-point 0 (aref bot-pos 1) 0))
                          (moves-in-clear-space
                           (make-point 0 (aref bot-pos 1) 0) #(0 0 0)))
           :do (push m commands))

        (push (make-instance 'flip) commands)
        (push (make-instance 'halt) commands)

        (reverse commands)))))

(defmethod generate-trace ((tracer (eql :trivial-low)) model)
  (let* ((target-state (src/model:make-pseudo-state-from-model model))
         (r (model-resolution model))
         (bot (make-instance 'nanobot
                             :bid 1
                             :pos #(0 0 0)
                             :seeds (loop :for i :from 2 :to 20 :collect i)))
         (state (src/state:make-state :r r
                                      :harmonics :low
                                      :matrix (make-array (* r r r)
                                                          :element-type 'bit
                                                          :initial-element 0)
                                      :bots (list bot)
                                      :trace nil))
         (gs (make-instance 'grounded-state))
         (commands nil))

    ;; (when (eq (state-harmonics state) :low)
    ;;   (push (make-instance 'flip) commands))

    (let* ((region (compute-model-bounding-box target-state)))
      (loop :for move :in (moves-in-clear-space #(0 0 0) (car region)) :do
           (push move commands))

      (setf (state-trace state) (reverse commands))
      (loop :while (state-trace state) :do
           (src/execution:execute-one-step state gs))

      (multiple-value-bind (moves-and-fills bot-pos)
          (generate-trivial-trace-for-region state gs target-state region :use-gs t)
        (loop :for m :in (append
                          moves-and-fills
                          (moves-in-clear-space
                           bot-pos (make-point 0 (aref bot-pos 1) 0))
                          (moves-in-clear-space
                           (make-point 0 (aref bot-pos 1) 0) #(0 0 0)))
           :do (push m commands))

        ;; (push (make-instance 'flip) commands)
        (push (make-instance 'halt) commands)
        ;; (format t "commands: ~A~%" (reverse commands))
        (reverse commands)))))

(defun generate-trivial-trace-for-region (state gs target-state region &key (use-gs nil))
  "Assumes bot to be already in c1 of `region' and state with :high `harmonics'"
  (let ((commands nil)
        (bot-pos (car region)))

    (labels ((%move-to-and-fill (x y z)
               (let* ((new-bot-pos (make-point x (1+ y) z))
                      (c0 (make-point x y z))
                      (voxels-to-fill (list c0)))

                 (loop :for dx :in '(-1 0 1)
                    :do (loop :for dz :in '(-1 0 1)
                           :do (let ((c1 (make-point (+ x dx)
                                                     y
                                                     (+ z dz))))
                                 (when (and (near? new-bot-pos c1)
                                            (inside-field? c1 (state-r target-state)))
                                   (push c1 voxels-to-fill)))))

                 (let ((moves (moves-in-clear-space bot-pos new-bot-pos))
                       (moved? nil))
                   (loop :for c :in voxels-to-fill :do
                        (when (and (voxel-full? target-state c)
                                   (voxel-void? state c))

                          (unless moved?
                            (loop :for m :in moves :do (push m commands))
                            (when use-gs
                              (setf (state-trace state) moves)
                              (loop :while (state-trace state) :do
                                   (src/execution:execute-one-step state gs)))
                            (setf moved? t)
                            (setf bot-pos new-bot-pos))

                          (let ((fill-cmd (make-instance 'fill :nd (pos-diff c new-bot-pos))))
                            (if use-gs
                                (progn
                                  (setf (state-trace state) (list fill-cmd))
                                  (src/execution:execute-one-step state gs)
                                  ;; if after execution of fill new voxel,
                                  ;; system changed grounding
                                  ;; perform `flip' beforehand
                                  (cond (;; low -> high
                                         (and (eq (state-harmonics state) :low)
                                              (not (grounded-check gs)))
                                         (setf (state-harmonics state) :high)
                                         (push (make-instance 'flip) commands)
                                         (push fill-cmd commands))
                                        (;; high -> low
                                         (and (eq (state-harmonics state) :high)
                                              (grounded-check gs))
                                         (setf (state-harmonics state) :low)
                                         (push fill-cmd commands)
                                         (push (make-instance 'flip) commands))
                                        ;; otherwise just fill
                                        (t (push fill-cmd commands))))
                                (push fill-cmd commands)))))
                   )
                 )))

      (destructuring-bind (c1 . c2) region
        (with-coordinates (x1 y1 z1) c1
          (with-coordinates (x2 y2 z2) c2
            ;; since we ar filling every 3 lines
            ;; traverse appropriately
            (multiple-value-bind (x-start x-end)
                ;; (values x1 x2)
                (case (mod (1+ (- x2 x1)) 3)
                  (0 (values (1+ x1) (1- x2)))
                  (1 (values x1 x2))
                  (2 (values (1+ x1) x2)))
              (loop :for y :from y1 :to y2 :do
                   (if (oddp y)
                       (loop :for x :from x-start :to x-end :by 3 :do
                            (if (oddp x)
                                (loop :for z :from z1 :to z2 :do
                                     (%move-to-and-fill x y z))
                                (loop :for z :from z2 :downto z1 :do
                                     (%move-to-and-fill x y z))))
                       (loop :for x :from x-end :downto x-start :by 3 :do
                            (if (oddp x)
                                (loop :for z :from z1 :to z2 :do
                                     (%move-to-and-fill x y z))
                                (loop :for z :from z2 :downto z1 :do
                                     (%move-to-and-fill x y z)))))))))))

    (values (reverse commands) bot-pos)))

(defun trivial-tracer (in-file out-file)
  (let* ((model (read-model-from-file in-file))
         (commands (generate-trace :trivial-low model)))
    (with-open-file (stream out-file
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
      (write-sequence (encode-commands commands) stream))))
