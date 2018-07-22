(uiop:define-package :src/spawn
    (:use :common-lisp)
  (:shadowing-import-from :src/commands #:fill)
  (:use :src/coordinates
        :src/state
        :src/commands
        :src/model
        :src/utils)
  (:import-from :src/trivial
                #:moves-in-clear-space))

(in-package :src/spawn)

(defun primitive-spawn (bot new-bots-positions &key n)
  "Generates sequence of commands to spawn all seeds around bot
   `n' is number of mots to spawn "
  (let* ((cmd-alist nil)
         (n (or n (length (bot-seeds bot))))
         (i 0) ;; number of spawned bots
         (seeds (sort (copy-list (take n (bot-seeds bot))) #'<))
         (bids (sort (cons (bot-bid bot) (copy-list seeds)) #'<)))
    ;; (format t "bids = ~A~%" bids)
    (loop :while seeds :do
         (push (cons (bot-bid bot) (make-instance 'fission
                                                  :nd (if (oddp i)
                                                          #(1 0 0)
                                                          #(0 0 1))
                                                  :m 0))
               cmd-alist)
         (let* ((bot1-bid (pop seeds))
                (bot1-pos (pop new-bots-positions))
                (moves (moves-in-clear-space
                        (pos-add (bot-pos bot) #(1 0 0))
                        bot1-pos)))
           (incf i)
           (loop :for m :in moves :do
                (push (cons bot1-bid m) cmd-alist))))

    (let ((bot->cmds (make-hash-table :test #'eq))
          (commands nil))
      (loop :for (bid . cmd) :in cmd-alist :do
           (push cmd (gethash bid bot->cmds)))

      (labels ((%print ()
                 (format t "~%tab:~%")
                 (maphash (lambda (bot cmds)
                            (format t "~A: ~A~%" bot cmds))
                          bot->cmds))
               (%sort-cmd (bots)
                 (loop :for bid :in bots :do
                      (let ((cmd (pop (gethash bid bot->cmds))))
                        (unless (gethash bid bot->cmds)
                          (remhash bid bot->cmds))
                        (if cmd
                            (push cmd commands)
                            (push (make-instance 'wait) commands))))))

        (loop :for i :from 1 :to (1+ n) :do
             (let ((bots (take i bids)))
               (%sort-cmd bots)))

        (loop :until (= (hash-table-count bot->cmds) 0) :do
             (%sort-cmd (take (1+ n) bids))))

      (reverse commands))))

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
