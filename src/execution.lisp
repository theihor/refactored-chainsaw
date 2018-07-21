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
    groups))

(defun execute-one-step (state)
  (with-slots (trace) state
    (let* ((bots (sort #'< (copy-list (state-bots state)) :key #'bot-bid))
           (commands (take (length bots) commands)))
      (assert (= (length commands) (length bots)))
      (group-bots (mapcar #'cons bots commands))
      )))
