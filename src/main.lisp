(defpackage :src/main
  (:nicknames :main)
  (:use :common-lisp :anaphora
        :src/state
        :src/execution
        :src/model
        :src/tracer)
  (:import-from :src/commands
                #:read-trace-from-file)
  (:export
   #:main
   #:execute-trace-on-model
   #:generate-trace-for-model))

(in-package :src/main)

(defun execute-trace-on-model (model trace)
  (let* ((r (model-resolution model))
         (bot (make-instance 'nanobot
                             :bid 1
                             :pos #(0 0 0)
                             :seeds (loop :for i :from 2 :to 20 :collect i)))
         (state (make-state :r r
                            :harmonics :low
                            :matrix (make-array (* r r r)
                                                :element-type 'bit
                                                :initial-element 0)
                            :bots (list bot)
                            :trace trace)))
    (execute-state-trace state)))

(defun execute-trace-on-model (model-file trace-file)
  (let ((model (read-model-from-file model-file))
        (trace (read-trace-from-file trace-file)))
    (execute-trace-on-model model trace)))

(defun generate-trace-for-model (model-file tracer)
  "Load model from `model-file' and generate the trace for it
   using the specified `tracer'."
  (let ((model (read-model-from-file model-file)))
    (generate-trace tracer model)))

(defun main ()
  (when sb-ext:*posix-argv*
    (let* ((parsed-args (apply-argv:parse-argv* sb-ext:*posix-argv*))
	       (files))
      (format t "~A~%~A~%" parsed-args (alexandria:plist-alist (cdr parsed-args)))
      (mapcar (lambda (p)
		        (let ((o (string (car p)))
		              (v (cdr p)))
		          (cond
		            ((string= "-f" o) (push v files)))))
	          (alexandria:plist-alist (cdr parsed-args)))
      (format t "~A~%" files)
      (dolist (f (reverse files))
	(when (probe-file f)
          (format *error-output* "Processing file ~A~%" f))))))

