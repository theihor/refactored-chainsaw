(uiop:define-package :src/execution
    (:use :common-lisp
          :src/state
          :src/commands
          :src/coordinates))

(defun execute (state)
  (assert (well-formed? state))
  (execute-one-step state))

(defun group-bots (bot-command-alist)
  "Returns list of bot-command-alists"
  (let ((alist (copy-list bot-command-alist))
        (groups nil))
    (labels ((%group-one (bot)
               (destructuring-bind (bot . cmd) bot-cmd
                 (typecase cmd
                   ((or fusionp fusions)
                    (let ((bot-cmd2 (find-if (lambda (b.c)
                                               (pos-eq (bot-pos (car b.c))
                                                       (bot-pos bot)))
                                             alist)))
                      (push (list bot-cmd bot-cmd2) groups)
                      (setf alist (remove bot-cmd2 alist :test #'eq))))
                   (t (push (list bot-cmd) groups))))))
      (loop :while alist
           (let ((bot-cmd (pop alist)))
             (%group-one bot-cmd alist))))

    ;; check if groups are correct
    (loop :for group :in groups :do
         (ecase (length group)
           (1 (assert (not (typep (cdr (car group)) '(or fusionp fusions)))))
           (2 (destructuring-bind (b.c1 b.c2) group
                (unless (or (and (typep (cdr b.c1) 'fusionp)
                                 (typep (cdr b.c2) 'fusions))
                            (and (typep (cdr b.c2) 'fusionp)
                                 (typep (cdr b.c1) 'fusions)))
                  (error "Group of 2 bots has invalid commands: ~A ~A~%"
                         (type-of (cdr b.c1)) (cdr b.c2)))))
           (t (error "Invalid group length: ~A ~A~%" (length group) group))))

    groups))

(defun execute-one-step (state)
  (with-slots (trace) state
    (let* ((bots (sort #'< (copy-list (state-bots state)) :key #'bot-bid))
           (commands (take (length bots) commands))
           (groups (progn (assert (= (length commands) (length bots)))
                          (group-bots (mapcar #'cons bots commands)))))
      )))
