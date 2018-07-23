(uiop:define-package :src/spawn
    (:use :common-lisp)
  (:shadowing-import-from :src/commands #:fill)
  (:use :src/coordinates
        :src/state
        :src/commands
        :src/model
        :src/utils)
  (:import-from :src/coordinates-helper
                #:moves-in-clear-space)
  (:export #:primitive-spawn))

(in-package :src/spawn)

(defun primitive-spawn (bot new-bots-positions &key n)
  "Generates sequence of commands to spawn all seeds around bot
   `n' is number of mots to spawn "
  (let* ((cmd-alist nil)
         (n (or n (length (bot-seeds bot))))
         (i 0) ;; number of spawned bots
         (seeds (sort (copy-list (take n (bot-seeds bot))) #'<)))
    ;; (format t "bids = ~A~%" bids)
    (loop :while seeds :do
         (push (cons (bot-bid bot) (make-instance 'fission
                                                  :nd (if (oddp i)
                                                          (make-point 1 0 0)
                                                          (make-point 0 0 1))
                                                  :m 0))
               cmd-alist)
         (let* ((bot1-bid (pop seeds))
                (bot1-pos (pop new-bots-positions))
                ;; first move to y, then to destination
                (bot1-start-pos (pos-add (bot-pos bot)
                                         (if (oddp i) #(1 0 0) #(0 0 1))))
                (proxy (with-coordinates (x y z) bot1-pos
                                         (with-coordinates (x0 y0 z0) bot1-start-pos
                                                           (make-point x0 y z0))))
                (moves (append (moves-in-clear-space bot1-start-pos proxy)
                               (moves-in-clear-space proxy bot1-pos))
                  ))
           (incf i)
           (loop :for m :in moves :do
                (push (cons bot1-bid m) cmd-alist))))

    (let* ((bot->cmds (make-hash-table :test #'eq)))

      (loop :for (bid . cmd) :in cmd-alist :do
           (push cmd (gethash bid bot->cmds)))

      ;; (maphash (lambda (bid cmds) (format t "cmds of ~A: ~A~%" bid cmds))
      ;;          bot->cmds)

      (let* ((steps (let ((max-len n))
                      (loop :for bid :in (sort (copy-list (take n (bot-seeds bot))) #'<)
                         :for i :from 1
                         :do (setf max-len (max (+ i (length (gethash bid bot->cmds)))
                                                max-len)))
                      max-len))
             (init-count-lst (loop :for i :from 1 :to n :collect i))
             (count-list (append init-count-lst
                                 (if (> steps n)
                                     (make-list (- steps n)
                                                :initial-element (1+ n))
                                     nil))))
        ;; (format t "steps = ~A; count-list: ~A~%" steps count-list)
        (sort-commands-for-bots bot->cmds count-list)))))

;; (defun spawn-in-line (bot region &key (n :all) (commands-acc nil) (spawned))
;;   "Generates  a sequence  of commands  to spawn  `n` bots  in `region'
;;    starting from `bot' Region should be of length at least `n'
;;    If `n' = :all or if `n' > bot.seeds, then all seeds of `bots' are spawned
;;    WARNING: `bot' should be the only active bot in the system"
;;   (let ((n (if (or (eq n :all) (> n (length (bot-seeds bot))))
;;                (length (bot-seeds bot))
;;                n))
;;         (m (/ n 2))
;;         (commands nil))
;;     ;; spawn a bot' and move it to the center of region by x axis
;;     (push (make-instance 'fission :nd #(1 0 0) :m m) commands)
;;     (with-coordinates (x1 y1 z1) (car region)
;;       (with-coordinates (x2 y2 z2) (cdr region)
;;         (loop :for cmd :in (moves-in-clear-space
;;                             (car region)
;;                             (make-point (/ (- x2 x1) 2) y1 z1)) :do
;;              (push (make-instance 'wait) commands)
;;              (push cmd commands))))))
