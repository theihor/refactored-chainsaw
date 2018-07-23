(uiop:define-package :src/trivial
    (:use :common-lisp)
  (:shadowing-import-from :src/commands #:fill)
  (:use :src/coordinates
        :src/state
        :src/coordinates-helper
        :src/commands
        :src/model
        :src/grounded
	    :src/tracer
        :src/spawn
        :src/utils)
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



(defmethod generate-trace ((tracer (eql :trivial)) task-type src-model tgt-model)
  (let ((target-state (src/model:make-pseudo-state-from-model tgt-model))
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

(defmethod generate-trace ((tracer (eql :trivial-low)) task-type src-model tgt-model)
  (let* ((target-state (src/model:make-pseudo-state-from-model tgt-model))
         (r (model-resolution tgt-model))
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

(defun constant-count-list-from-bid->cmds (bid->cmds)
  (let* ((steps (apply #'max
                       (mapcar #'length
                               (alexandria:hash-table-values bid->cmds))))
         (count-list (make-list steps :initial-element (hash-table-count bid->cmds))))
    count-list))

(defmethod generate-trace ((tracer (eql :trivial-parallel)) task-type src-model model)
  (let* ((target-state (src/model:make-pseudo-state-from-model model))
         (r (model-resolution model))
         (bot (make-instance 'nanobot
                             :bid 1
                             :pos #(0 0 0)
                             :seeds (loop :for i :from 2 :to 40 :collect i)))
         (state (src/state:make-state :r r
                                      :harmonics :low
                                      :matrix (make-array (* r r r)
                                                          :element-type 'bit
                                                          :initial-element 0)
                                      :bots (list bot)
                                      :trace nil))
         (gs (make-instance 'grounded-state))
         (commands nil)
         (clusters (get-clusters target-state)))

    (when (eq (state-harmonics state) :low)
      (push (make-instance 'flip) commands))

    (let* ((regions
            (loop :for cluster :in (alexandria:hash-table-keys clusters)
               :collect (with-coordinates (x1 y1 z1) (car cluster)
                                          (with-coordinates (x2 y2 z2) (cdr cluster)
                                                            (make-region (make-point x1 0 z1)
                                                                         (make-point x2 (1- r) z2))))))
           (bots-positions (loop :for reg :in (cdr regions)
                              :for dy :from 2
                              :collect
                                (progn
                                  (when (= 0 (mod dy (1- (state-r state))))
                                    (setf dy 1))
                                  (with-coordinates (x y z) (car reg)
                                                    (make-point x dy z)))))
           (bids (sort (cons (bot-bid bot) (copy-list (take (length bots-positions) (bot-seeds bot)))) #'<))
           (bid->cmds (make-hash-table :test #'eq))
           (bid->pos (make-hash-table :test #'eq)))

      ;; first, let's spawn all bots

      ;; (format t "regions: ~A~%" regions)
      (loop :for (r . rest) :on regions :do
           (some (lambda (p) (loop :for r1 :in rest :do
                             (when (in-region p r1)
                               (error "Regions collide ~A and ~A" r r1))))
                 (region-points r)))
      ;; (format t "bot-positions: ~A~%" bots-positions)
      (setf commands
            (reverse (cons (make-instance 'flip)
                           (primitive-spawn bot bots-positions :n (1- (hash-table-count clusters))))))

      ;; then move them to the regions start

      (let ((init-pos (make-hash-table :test #'eq)))

        ;; (push (make-instance 'wait) (gethash (bot-bid bot) init-pos))

        (loop :for reg :in (cdr regions)
           :for bot1-pos :in bots-positions
           :for bid :in (cdr bids)
           :do (loop :for move :in (moves-in-clear-space bot1-pos (car reg))
                  :do (push move (gethash bid init-pos))))

        (loop :for move :in (moves-in-clear-space #(0 0 0) (car (first regions)))
           :do (loop :for bid :in bids :do
                    (if (= bid (bot-bid bot))
                        (push move (gethash bid init-pos))
                        (push (make-instance 'wait) (gethash bid init-pos)))))

        
        (loop :for cmd :in (sort-commands-for-bots
                            init-pos
                            (constant-count-list-from-bid->cmds init-pos))
           :do (push cmd commands)))

      (setf (state-trace state) (reverse commands))

      ;; (with-open-file (stream "/home/theihor/repo/refactored-chainsaw/test-trace.nbt"
      ;;                         :direction :output
      ;;                         :if-exists :supersede
      ;;                         :if-does-not-exist :create
      ;;                         :element-type '(unsigned-byte 8))
      ;;   (write-sequence (encode-commands (state-trace state)) stream))
      ;; (break)
      ;; (loop :while (state-trace state) :do
      ;;      (src/execution:execute-one-step state gs))

      ;; then generate moves and fills
      (setf (state-bots state) (list bot))
      (loop :for region :in regions
         :for bid :in bids
         :do
           (setf (bot-pos bot) (car region))
           (multiple-value-bind (bot-commands new-bot-pos)
               (generate-trivial-trace-for-region
                state gs target-state region :use-gs t)
             (setf (gethash bid bid->cmds) (remove-if (lambda (c) (typep c 'flip)) bot-commands))
             (setf (gethash bid bid->pos) new-bot-pos)))

      (setf commands (append (reverse commands)
                             (sort-commands-for-bots
                              bid->cmds
                              (constant-count-list-from-bid->cmds bid->cmds))))
      ;; at this point all bots have finished construction and now are waiting


      ;; now fuse them one by one
      (let* ((r (state-r state))
             (bid->cmds (make-hash-table :test #'eq))
             (count-list nil)
             (bid0 (find (bot-bid bot) bids))
             (bids (remove bid0 bids)))
        (format t "bids = ~A~%" bids)
        ;; flip harmonics to low
        (push (make-instance 'flip) (gethash bid0 bid->cmds))
        (loop :for bid :in bids :do
             (push (make-instance 'wait) (gethash bid bid->cmds)))
        (push (1+ (length bids)) count-list)
        ;; move first bot to #(0 R-1 0)
        (with-coordinates (x y z)
          (gethash bid0 bid->pos)
          (loop :for m :in (append (moves-in-clear-space
                                    (make-point x y z)
                                    (make-point x (1- r) z))
                                   (moves-in-clear-space
                                    (make-point x (1- r) z)
                                    (make-point 0 (1- r) 0))) :do
               (push m (gethash bid0 bid->cmds))
               (loop :for bid :in bids :do
                    (push (make-instance 'wait) (gethash bid bid->cmds)))
               (push (1+ (length bids)) count-list)))
        ;; then move each bot to #(0 R-1 1) and fuse
        (loop :while bids :do
             (let ((bid1 (pop bids)))
               (with-coordinates (x y z)
                 (gethash bid1 bid->pos)
                 ;; move to #(x R-1 z) and to #(0 R-1 1)
                 (loop :for m :in (append (moves-in-clear-space
                                           (make-point x y z)
                                           (make-point x (1- r) z))
                                          (moves-in-clear-space
                                           (make-point x (1- r) z)
                                           (make-point 0 (1- r) 1))) :do
                      (push m (gethash bid1 bid->cmds))
                      (loop :for bid :in (cons bid0 bids) :do
                           (push (make-instance 'wait) (gethash bid bid->cmds)))
                      (push (+ (length bids) 2) count-list))
                 ;; finally fuse
                 (push (make-instance 'fusionp :nd (make-point 0 0 1))
                       (gethash bid0 bid->cmds))
                 (push (make-instance 'fusions :nd (make-point 0 0 -1))
                       (gethash bid1 bid->cmds))
                 (loop :for bid :in bids :do
                      (push (make-instance 'wait) (gethash bid bid->cmds)))
                 (push (+ (length bids) 2) count-list)
                 )))
        ;; move to #(0 0 0) and halt
        (loop :for m :in (moves-in-clear-space (make-point 0 (1- r) 0)
                                               (make-point 0 0 0)) :do
             (push m (gethash bid0 bid->cmds))
             (push 1 count-list))
        (push (make-instance 'halt) (gethash bid0 bid->cmds))
        (push 1 count-list)
        ;; (format t "fuse count-list: ~A~%" (reverse count-list))
        (loop :for bid :in (alexandria:hash-table-keys bid->cmds) :do
             (setf (gethash bid bid->cmds) (reverse (gethash bid bid->cmds))))
        (append commands (sort-commands-for-bots bid->cmds (reverse count-list)))))))

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
                                            (inside-field? c1 (state-r target-state))
                                            (in-region c1 region))
                                   (push c1 voxels-to-fill)))))

                 (let ((moves (moves-in-clear-space bot-pos new-bot-pos))
                       (moved? nil))
                   (loop :for c :in voxels-to-fill :do
                        (when (and (voxel-full? target-state c)
                                   (voxel-void? state c))

                          (unless moved?
                            (loop :for m :in moves :do (push m commands))
                            ;; (when use-gs
                            ;;   (setf (state-trace state) moves)
                            ;;   (loop :while (state-trace state) :do
                            ;;        (src/execution:execute-one-step state gs)))
                            (setf moved? t)
                            (setf bot-pos new-bot-pos))

                          (let ((fill-cmd (make-instance 'fill :nd (pos-diff c new-bot-pos))))
                            (if use-gs
                                (progn
                                  (setf (bot-pos (first (state-bots state))) bot-pos)
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
         (commands (generate-trace :trivial-parallel :assembly nil model)))
    (with-open-file (stream out-file
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
      (write-sequence (encode-commands commands) stream))))
