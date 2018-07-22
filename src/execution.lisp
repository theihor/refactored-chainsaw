(uiop:define-package :src/execution
    (:use :common-lisp)
  (:shadow #:fill)
  (:use :src/commands
        :src/state
        :src/coordinates
        :src/grounded
        :src/utils)
  (:export #:execute-state-trace
           #:execute-one-step))

(in-package :src/execution)

(defun well-formed? (s gs)
  (and (if (eq (state-harmonics s) :low)
           (if (not (grounded-check gs)) (error "NOT GROUNDED") t)
           t)
       (progn
         (loop :for (b . rest) :on (state-bots s) :do
              (unless (and (every (lambda (b1)
                                    (and (not (= (bot-bid b1)
                                                 (bot-bid b)))
                                         (not (pos-eq (bot-pos b1)
                                                      (bot-pos b)))))
                                  rest)
                           (every (lambda (seed)
                                    (not (member seed (state-bots s)
                                                 :key #'bot-bid :test #'=)))
                                  (bot-seeds b)))
                (return-from well-formed? nil))
              (loop :for (s . s-rest) :on (bot-seeds b) :do
                   (unless (every (lambda (s1) (not (= s1 s)))
                                  s-rest)
                     (return-from well-formed? nil))))
         t)))

(defun execute-state-trace (state)
  (let ((gs (make-instance 'grounded-state)))
    (loop :for i :from 1
       :while (state-bots state) :do
         (assert (well-formed? state gs))
         (execute-one-step state gs)))
  (format t "Energy at the end of simulation: ~A~%" (state-energy state))
  state)

(defun group-bots (bot-command-alist)
  "Returns list of bot-command-alists"
  (let ((alist (copy-list bot-command-alist))
        (groups nil))
    (labels ((%group-one (bot-cmd)
               (destructuring-bind (bot . cmd) bot-cmd
                 (typecase cmd
                   ((or fusionp fusions)
                    (let ((bot2-cmd2
                           (find-if (lambda (b.c)
                                      (and (typep (cdr b.c) '(or fusions fusionp))
                                           (not (typep (cdr b.c) (type-of cmd)))
                                           (pos-eq (bot-pos (car b.c))
                                                   (nd cmd))))
                                    alist)))
                      (push (list bot-cmd bot2-cmd2) groups)
                      (setf alist (remove bot2-cmd2 alist :test #'eq))))
                   ((or gfill gvoid)
                    (push
                     (loop
                        :with start-point := (pos-add (bot-pos bot) (nd cmd))
                        :for next-point := (pos-add start-point (fd cmd))
                        :while (not (pos-eq next-point start-point))
                        :collect
                          (let ((bot2-cmd2
                                 (find-if (lambda (b.c)
                                            (and (typep (cdr b.c) (type-of cmd))
                                                 (pos-eq (pos-add (bot-pos (car b.c))
                                                                  (nd (cdr b.c)))
                                                         next-point)))
                                          alist)))
                            (setf next-point (bot-pos (car bot2-cmd2)))
                            (setf alist (remove bot2-cmd2 alist :test #'eq))))
                     groups))
                   (t (push (list bot-cmd) groups))))))
      (loop :while alist :do
           (let ((bot-cmd (pop alist)))
             (%group-one bot-cmd)))

      ;; check if groups are correct
      (loop :for group :in groups :do
           (ecase (length group)
             (1 (assert (not (typep (cdr (car group))
                                    '(or fusionp fusions))))
                t)
             (2 (destructuring-bind (b.c1 b.c2) group
                  (unless (or (and (typep (cdr b.c1) 'fusionp)
                                   (typep (cdr b.c2) 'fusions))
                              (and (typep (cdr b.c2) 'fusionp)
                                   (typep (cdr b.c1) 'fusions)))
                    (error "Group of 2 bots has invalid commands: ~A ~A~%"
                           (type-of (cdr b.c1)) (cdr b.c2))))
                t)
             ;; (t (error "Invalid group length: ~A ~A~%" (length group) group))
             )))

    groups))

(defun check-volatile-regions (region-groups r)
  "`region-groups' is list of lists of regions "
  (let ((points (make-hash-table :test #'eq)))
    (loop :for region-group :in region-groups :do
         (loop :for region :in region-group :do
              (loop :for point :in (region-points region) :do
                   (let ((i (matrix-index point r)))
                     (if (gethash i points)
                         (error "Volatile regions intersect: ~A~%"
                                region-groups)
                         (setf (gethash i points) t))))))
    t))

(defun execute-one-step (state gs)
  (with-slots (trace energy harmonics r) state
    (let* ((bots (sort (copy-list (state-bots state)) #'< :key #'bot-bid))
           (n (length bots))
           (commands (take n trace))
           (groups
            (progn
              (assert (= (length commands) n))
              (group-bots (mapcar #'cons bots commands))))
           (volatile-region-groups
            (loop :for group :in groups
               :collect (destructuring-bind (_ . cmd) (first group)
                          (declare (ignore _))
                          (get-volatile-regions cmd state group)))))
      (check-volatile-regions volatile-region-groups r)
      (loop :for group :in groups
         :do (destructuring-bind (bot . cmd) (first group)
               (unless (check-preconditions cmd state group)
                 (error "Preconditions failed for command ~A and group ~A" bot group))))

      ;; global maintainance
      (ecase harmonics
        (:high (incf energy (* 30 r r r)))
        (:low (incf energy (* 3 r r r))))

      ;; bots maintainance
      (incf energy (* 20 n))

      (loop :for group :in groups
         :do (destructuring-bind (_ . cmd) (first group)
               (declare (ignore _))
               (execute cmd state group)))

      (loop :for group :in groups
         :do (destructuring-bind (bot . cmd) (first group)
               (typecase cmd
                 (src/commands:fill
                  (grounded-add-voxel gs (pos-add (bot-pos bot) (nd cmd)) state))
                 (src/commands:void
                  (grounded-rm-voxel gs (pos-add (bot-pos bot) (nd cmd)) state))
                 (src/commands:gfill
                  (dolist (c (region-points (group-region group)))
                    (grounded-add-voxel gs c state)))
                 (src/commands:gvoid
                  (dolist (c (region-points (group-region group)))
                    (grounded-rm-voxel gs c state))))))

      (setf trace (nthcdr n trace)))))
