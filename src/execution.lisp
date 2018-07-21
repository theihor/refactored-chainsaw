(uiop:define-package :src/execution
    (:use :common-lisp)
  (:shadow #:fill)
  (:use :src/commands
        :src/state
        :src/coordinates
        :src/grounded
        :src/utils)
  (:export #:execute-state-trace))

(in-package :src/execution)

(defun well-formed? (s gs)
  (and (if (eq (state-harmonics s) :low)
           (grounded-check gs)
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
         (execute-one-step state gs)
         (format t "Energy at step ~A: ~A~%" i (state-energy state))))
  state)

(defun group-bots (bot-command-alist)
  "Returns list of bot-command-alists"
  (let ((alist (copy-list bot-command-alist))
        (groups nil))
    (labels ((%group-one (bot-cmd)
               (destructuring-bind (_ . cmd) bot-cmd
                 (declare (ignore _))
                 (typecase cmd
                   ((or fusionp fusions)
                    (let ((bot-cmd2 (find-if (lambda (b.c)
                                               (pos-eq (bot-pos (car b.c))
                                                       (nd cmd)))
                                             alist)))
                      (push (list bot-cmd bot-cmd2) groups)
                      (setf alist (remove bot-cmd2 alist :test #'eq))))
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

(defun check-volatile-regions (region-groups)
  "`region-groups' is list of lists of regions "
  (let ((points (make-hash-table :test #'eq)))
    (loop :for region-group :in region-groups :do
         (loop :for region :in region-group :do
              (loop :for point :in (region-points region) :do
                   (let ((i (matrix-index point)))
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
           (groups (progn (assert (= (length commands) n))
                          (group-bots (mapcar #'cons bots commands))))
           (volatile-region-groups
            (loop :for group :in groups :collect
                 (loop :for (bot . cmd) :in group :append
                      (get-volatile-regions cmd bot)))))
      (check-volatile-regions volatile-region-groups)
      (loop :for group :in groups :do
           (loop :for (bot . cmd) :in group :do
                (unless (check-preconditions
                         cmd state (mapcar #'car group))
                  (error "Preconditions failed for command ~A and group ~A"
                         cmd group))))

      ;; global maintainance
      (ecase harmonics
        (:high (incf energy (* 30 r r r)))
        (:low (incf energy (* 3 r r r))))
      ;; bots maintainance
      (incf energy (* 20 n))

      (loop :for group :in groups :do
           (loop :for (bot . cmd) :in group :do
                (execute cmd state bot)))

      (loop :for group :in groups :do
           (loop :for (bot . cmd) :in group :do
                (when (typep cmd 'src/commands:fill)
                  (grounded-add-voxel gs (pos-add (bot-pos bot) (nd cmd)) state))))

      (setf trace (nthcdr n trace)))))
