(uiop:define-package :src/main
  (:nicknames :main)
  (:use :common-lisp :anaphora
        :src/state
        :src/execution
        :src/model
        :src/tracer
        :src/trivial
        :src/layered-bot
        :src/bitonic)
  (:import-from :src/commands
                #:read-trace-from-file
		#:encode-commands)
  (:export
   #:main
   #:execute-trace-on-model
   #:generate-trace-for-model))

(in-package :src/main)

(defun execute-trace-on-model (task-type src-model tgt-model trace-file)
  (execute-trace-on-model-aux 
   task-type 
   src-model 
   tgt-model 
   (read-trace-from-file trace-file)))

(defun execute-trace-on-model-aux (task-type src-model tgt-model trace)
  (case task-type
    (:assembly
     (let* ((model (read-model-from-file tgt-model))
	    (model-name (subseq (file-namestring tgt-model) 0 (- (position #\. (file-namestring tgt-model)) 4)))
	    (r (model-resolution model))
	    (bot (make-instance 'nanobot
				:bid 1
				:pos #(0 0 0)
				:seeds (loop :for i :from 2 :to 40 :collect i)))
	    (state (make-state :r r
			       :harmonics :low
			       :matrix (make-array (* r r r)
						   :element-type 'bit
						   :initial-element 0)
			       :bots (list bot)
			       :trace trace))
	    (final-state (execute-state-trace state))
	    (xor-res (bit-xor (model-matrix model) (state-matrix final-state)))
	    (zero-model (make-array (* r r r)
				    :element-type 'bit
				    :initial-element 0)))
       
       (values model-name (equalp xor-res zero-model) (state-energy final-state))))
    (:disassembly
     (let* ((model (read-model-from-file src-model))
	    (model-name (subseq (file-namestring src-model) 0 (- (position #\. (file-namestring src-model)) 4)))
	    (r (model-resolution model))
	    (bot (make-instance 'nanobot
				:bid 1
				:pos #(0 0 0)
				:seeds (loop :for i :from 2 :to 20 :collect i)))
	    (state (make-state :r r
			       :harmonics :low
			       :matrix (model-matrix model)
			       :bots (list bot)
			       :trace trace))
	    (final-state (execute-state-trace state))
	    (zero-model (make-array (* r r r)
				    :element-type 'bit
				    :initial-element 0)))
       (values model-name (equalp (state-matrix final-state) zero-model) (state-energy final-state))))
    (:reassembly
     (let* ((model (read-model-from-file src-model))
	    (res-model (read-model-from-file tgt-model))
	    (model-name (subseq (file-namestring src-model) 0 (- (position #\. (file-namestring src-model)) 4)))
	    (r (model-resolution model))
	    (bot (make-instance 'nanobot
				:bid 1
				:pos #(0 0 0)
				:seeds (loop :for i :from 2 :to 20 :collect i)))
	    (state (make-state :r r
			       :harmonics :low
			       :matrix (model-matrix model)
			       :bots (list bot)
			       :trace trace))
	    (final-state (execute-state-trace state)))
       (values model-name (equalp (state-matrix final-state) res-model) (state-energy final-state))))))

(defun generate-trace-for-model (task-type src-model tgt-model tracer)
  "Load model from `model-file' and generate the trace for it
   using the specified `tracer'."
  (case task-type
    (:assembly
     (let* ((model (read-model-from-file tgt-model))
	    (trace (generate-trace tracer :assembly nil model)))
       (format t ":assembly task: ~A~%" tgt-model)
       (multiple-value-bind (model-name success energy) 
	   (execute-trace-on-model-aux :assembly src-model tgt-model trace)
	 (when success
	   (with-open-file (stream (format nil "./traces/~A.nbt.~A" model-name energy)
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
	     (write-sequence (encode-commands trace) stream))))))
    (:disassembly
     (let* ((model (read-model-from-file src-model))
	    (trace (generate-trace tracer :disassembly model nil)))
       (format t ":disassembly task: ~A~%" src-model)
       (multiple-value-bind (model-name success energy) 
	   (execute-trace-on-model-aux :disassembly src-model tgt-model trace)
	 (when success
	   (with-open-file (stream (format nil "./traces/~A.nbt.~A" model-name energy)
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
	     (write-sequence (encode-commands trace) stream))))))
    (:reassembly
     (let* ((model (read-model-from-file src-model))
	    (res-model (read-model-from-file tgt-model))
	    (trace (generate-trace tracer :reassembly model res-model)))
       (format t ":reassembly task: ~A ~A~%" src-model tgt-model)
       (multiple-value-bind (model-name success energy) 
	   (execute-trace-on-model-aux :reassembly src-model tgt-model trace)
	 (when success
	   (with-open-file (stream (format nil "./traces/~A.nbt.~A" model-name energy)
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
	     (write-sequence (encode-commands trace) stream))))))))

(defun main ()
  (handler-case
      (when sb-ext:*posix-argv*
        (let* ((parsed-args (apply-argv:parse-argv* sb-ext:*posix-argv*))
               (src-file) (tgt-file))
          ;; (format t "~A~%~A~%" parsed-args (alexandria:plist-alist (cdr parsed-args)))
          (mapcar (lambda (p)
                    (let ((o (string (car p)))
                          (v (cdr p)))
                      (cond
                        ((string= "-s" o) (setf src-file v))
                        ((string= "-t" o) (setf tgt-file v)))))
                  (alexandria:plist-alist (cdr parsed-args)))
          (if (and src-file tgt-file)
              (generate-trace-for-model :reassembly src-file tgt-file :trivial-low)
              (if src-file
                  (generate-trace-for-model :disassembly src-file nil :trivial-low)
                  (generate-trace-for-model :assembly nil tgt-file :trivial-parallel-low)))))
    (error (e) nil)))

(defun get-best-traces ()
  (let ((best-res (make-hash-table :test #'equal)))
    ;; read initial best results
    (when (probe-file "./best_traces/results.txt")
      (with-open-file (stream "./best_traces/results.txt")
	(let ((content (read stream)))
	  (setf best-res (alexandria::alist-hash-table content)))))
    ;; go over trace-folder files and fill hash table
    (cl-fad::walk-directory 
     "./traces" 
     (lambda (f)
       (let* ((name (file-namestring f))
	      (e-str (subseq name (1+ (position #\. name :from-end t))))
	      (pr-name (intern (subseq name 0 (position #\. name))))
	      (energy))
	 ;; (format t "~A~%" name)
	 (setq energy (parse-integer e-str :junk-allowed t))
	 (when (not (null energy))
	   ;; (format t "energy:~A~%" energy)
	   (aif (gethash pr-name best-res)
		(when (< energy it)
		  (setf (gethash pr-name best-res) energy)
		  (cl-fad::copy-file f (pathname 
					(format nil "./best_traces/~A.nbt" pr-name)) 
				     :overwrite t))
		(progn 
		  (setf (gethash pr-name best-res) energy)
		  (cl-fad::copy-file f (pathname 
					(format nil "./best_traces/~A.nbt" pr-name))
				     :overwrite t)))))))
    ;;dump new version of results
    (with-open-file (stream "./best_traces/results.txt" :direction :output 
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (format stream "~A" (alexandria::hash-table-alist best-res)))))

