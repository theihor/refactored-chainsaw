(defpackage :src/main
  (:nicknames :main)
  (:use :common-lisp :anaphora))

(in-package :src/main)

(defun main () 
  (when sb-ext:*posix-argv*
    (let* ((parsed-args (apply-argv:parse-argv* sb-ext:*posix-argv*))
	   (files))
      ;; (format t "~A~%~A~%" parsed-args (alexandria:plist-alist (cdr parsed-args)))
      (mapcar (lambda (p)
		(let ((o (string (car p)))
		      (v (cdr p)))
		  (cond
		    ((string= "-f" o) (push v files)))))
	      (alexandria:plist-alist (cdr parsed-args)))
      ;; (format t "~A~%" files)
      (let ((result-list nil))
	(dolist (f (reverse files))			
	  (when (probe-file f)
            (format *error-output* "Processing file ~A~%" f)))))))

