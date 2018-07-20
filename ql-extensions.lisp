(proclaim '(optimize (debug 3) (safety 3)))

(load #P"~/quicklisp/setup.lisp")

(require 'asdf)

(defclass ql-repo ()
  ((name
    :accessor ql-repo-name
    :initarg :name)
   (url
    :accessor ql-repo-url
    :initarg :url)
   (cmd
    :accessor ql-repo-cmd
    :initform (error "Abstract ql-repo cannot be instantiated."))))

(defmethod ql:quickload ((target ql-repo) &key &allow-other-keys)
  (labels ((%target-dir ()
             (format nil "~alocal-projects/~a"
                     ql:*quicklisp-home*
                     (string-downcase (symbol-name (ql-repo-name target))))))
    (unless (uiop/filesystem:directory-exists-p (%target-dir))
      (uiop:run-program (format nil (ql-repo-cmd target) (ql-repo-url target) (%target-dir)))))
  (ql:quickload (ql-repo-name target)))

(defclass ql-git-repo (ql-repo)
  ((cmd :initform "git clone '~a' '~a'")))

(defun ql-git (name url)
  (make-instance 'ql-git-repo :name name :url url))

