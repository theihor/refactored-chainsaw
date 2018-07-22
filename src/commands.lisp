(uiop:define-package :src/commands
    (:use :common-lisp)
  (:use :src/coordinates
        :src/state
        :src/utils)
  (:shadow #:fill)
  (:export
   ;; commands
   #:halt
   #:wait
   #:flip
   #:smove
   #:lmove
   #:fission
   #:fill
   #:void
   #:fusionp
   #:fusions
   #:gfill
   #:gvoid
   ;; command operations
   #:encode-commands
   #:decode-commands
   #:read-trace-from-file
   ;; command accessors
   #:nd
   #:fd

   #:get-volatile-regions
   #:check-preconditions
   #:execute
   #:check-preconditions
   #:group-region

   #:sort-commands-for-bots
   ))

(in-package :src/commands)

(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

;;;------------------------------------------------------------------------------
;;; Generic definitions and utils
;;;------------------------------------------------------------------------------
(defclass singleton ()
  ())

(defclass group ()
  ())

(defgeneric command-group (cmd bot state)
  (:documentation "Return a group for group command `cmd' of a given `bot'."))

(defgeneric encode-command (cmd))

(defgeneric get-volatile-regions (cmd state bot)
  (:documentation "Return list of regions with volatile points"))

(defgeneric check-preconditions (cmd state bots)
  (:documentation "Check if command is valid"))

(defgeneric execute (cmd state bot)
  (:documentation "Execute command and change states"))

(defmacro %bytes (size &rest bytes)
  `(list ,@bytes))

(declaim (ftype (function (point) (values (unsigned-byte 2) (unsigned-byte 4))) encode-sld))
(defun encode-sld (sld)
  "A short linear coordinate difference sld = <dx, dy, dz> is encoded as a 2-bit
   axis a and a 4-bit (unsigned) integer i as follows:
      if dx ≠ 0, then a = [01]2 and i = dx + 5;
      if dy ≠ 0, then a = [10]2 and i = dy + 5;
      if dz ≠ 0, then a = [11]2 and i = dz + 5.
   Returns (values a i)."
  (let ((dx (aref sld 0))
        (dy (aref sld 1))
        (dz (aref sld 2)))
    (cond ((not (zerop dx)) (values #b01 (+ dx 5)))
          ((not (zerop dy)) (values #b10 (+ dy 5)))
          ((not (zerop dz)) (values #b11 (+ dz 5))))))

(declaim (ftype (function (point) (values (unsigned-byte 2) (unsigned-byte 5))) encode-lld))
(defun encode-lld (lld)
  "A long linear coordinate difference lld = <dx, dy, dz> is encoded as a 2-bit
   axis a and a 5-bit (unsigned) integer i as follows:
      if dx ≠ 0, then a = [01]2 and i = dx + 15;
      if dy ≠ 0, then a = [10]2 and i = dy + 15;
      if dz ≠ 0, then a = [11]2 and i = dz + 15.
   Returns (values a i)."
  (let ((dx (aref lld 0))
        (dy (aref lld 1))
        (dz (aref lld 2)))
    (cond ((not (zerop dx)) (values #b01 (+ dx 15)))
          ((not (zerop dy)) (values #b10 (+ dy 15)))
          ((not (zerop dz)) (values #b11 (+ dz 15))))))

(declaim (ftype (function (point) (unsigned-byte 5)) encode-nd))
(defun encode-nd (nd)
  "A near coordinate difference nd = <dx, dy, dz> is encoded as a
   5-bit (unsigned) integer with the value (dx + 1) * 9 + (dy + 1) * 3 + (dz +
   1). Recall that each component of a near coordinate difference must have the
   value -1, 0, or 1, but not all combinations are legal. In particular, <1, 1,
   1> is not a near coordinate difference; hence the 5-bit value [11111]5 = 31 is
   not the encoding of any near coordinate difference."
  (let ((dx (aref nd 0))
        (dy (aref nd 1))
        (dz (aref nd 2)))
    (+ (* (1+ dx) 9) (* (1+ dy) 3) (1+ dz))))

(declaim (ftype (function (point) (values (unsigned-byte 8) (unsigned-byte 8) (unsigned-byte 8)))
                encode-fd))
(defun encode-fd (fd)
  "A far coordinate difference fd = <dx, dy, dz> is encoded as a sequence of
   three 8-bit (unsigned) integers with the values dx + 30, dy + 30, and dz +
   30. Recall that at least one component of a far coordinate difference is
   non-zero and each component of a far coordinate difference has a value
   between -30 and 30. Return (values <byte-1> <byte-2> <byte-3>)."
  (let ((dx (aref fd 0))
        (dy (aref fd 1))
        (dz (aref fd 2)))
    (values (+ dx 30) (+ dy 30) (+ dz 30))))


(declaim (ftype (function ((unsigned-byte 2) (unsigned-byte 4)) point) decode-sld))
(defun decode-sld (a i)
  (ecase a
    (#b01 (make-point (- i 5) 0 0))
    (#b10 (make-point 0 (- i 5) 0))
    (#b11 (make-point 0 0 (- i 5)))))

(declaim (ftype (function ((unsigned-byte 2) (unsigned-byte 5)) point) decode-lld))
(defun decode-lld (a i)
  (ecase a
    (#b01 (make-point (- i 15) 0 0))
    (#b10 (make-point 0 (- i 15) 0))
    (#b11 (make-point 0 0 (- i 15)))))

(declaim (ftype (function ((unsigned-byte 5)) point) decode-nd))
(defun decode-nd (v)
  (multiple-value-bind (dx1 r) (floor v 9)
    (multiple-value-bind (dy1 r) (floor r 3)
      (make-point (1- dx1) (1- dy1) (1- r)))))

(declaim (ftype (function ((unsigned-byte 8) (unsigned-byte 8) (unsigned-byte 8)) point)
                decode-fd))
(defun decode-fd (b1 b2 b3)
  (make-point (- b1 30) (- b2 30) (- b3 30)))


(defun decode-commands (stream)
  "Return list of command objects read from stream."
  (loop
     :for b := (read-byte stream nil nil)
     :then (read-byte stream nil nil)
     :while b
     :collect (cond ((= b #b11111111) (make-instance 'halt))
                    ((= b #b11111110) (make-instance 'wait))
                    ((= b #b11111101) (make-instance 'flip))
                    ((= (logand b #b00001111) #b00000100) ; smove
                     (let* ((b1 (read-byte stream))
                            (a (ash (logand b #b00110000) -4))
                            (i b1))
                       (make-instance 'smove :lld (decode-lld a i))))
                    ((= (logand b #b00001111) #b00001100) ; lmove
                     (let* ((b1 (read-byte stream))
                            (a2 (ash (logand b  #b11000000) -6))
                            (i2 (ash (logand b1 #b11110000) -4))
                            (a1 (ash (logand b  #b00110000) -4))
                            (i1      (logand b1 #b00001111)))
                       (make-instance 'lmove :sld1 (decode-sld a1 i1) :sld2 (decode-sld a2 i2))))
                    ((= (logand b #b00000111) #b00000111) ; fusionp
                     (let ((nd (ash (logand b #b11111000) -3)))
                       (make-instance 'fusionp :nd (decode-nd nd))))
                    ((= (logand b #b00000111) #b00000110) ; fusions
                     (let ((nd (ash (logand b #b11111000) -3)))
                       (make-instance 'fusions :nd (decode-nd nd))))
                    ((= (logand b #b00000111) #b00000001) ; gfill
                     (let ((nd (ash (logand b #b11111000) -3))
                           (b1 (read-byte stream))
                           (b2 (read-byte stream))
                           (b3 (read-byte stream)))
                       (make-instance 'gfill :nd (decode-nd nd) :fd (decode-fd b1 b2 b3))))
                    ((= (logand b #b00000111) #b00000000) ; gvoid
                     (let ((nd (ash (logand b #b11111000) -3))
                           (b1 (read-byte stream))
                           (b2 (read-byte stream))
                           (b3 (read-byte stream)))
                       (make-instance 'gvoid :nd (decode-nd nd) :fd (decode-fd b1 b2 b3))) )
                    ((= (logand b #b00000111) #b00000101) ; fission
                     (let ((m (read-byte stream))
                           (nd (ash (logand b #b11111000) -3)))
                       (make-instance 'fission :m m :nd (decode-nd nd))))
                    ((= (logand b #b00000111) #b00000011) ; fill
                     (let ((nd (ash (logand b #b11111000) -3)))
                       (make-instance 'fill :nd (decode-nd nd))))
                    ((= (logand b #b00000111) #b00000010) ; void
                     (let ((nd (ash (logand b #b11111000) -3)))
                       (make-instance 'void :nd (decode-nd nd)))))))

(defun read-trace-from-file (filename)
  (with-open-file (stream filename :element-type '(unsigned-byte 8))
    (decode-commands stream)))

(defun encode-commands (commands)
  "Encode commands and return octet vector."
  (coerce (alexandria:mappend #'encode-command commands) 'vector))


;;;------------------------------------------------------------------------------
;;; Singleton commands
;;;------------------------------------------------------------------------------

;; Halt
(defclass halt (singleton) ())

(defmethod encode-command ((cmd halt))
  (%bytes 1 #b11111111))

(defmethod get-volatile-regions ((cmd halt) (state state) group)
  (let ((bpos (bot-pos (car (first group)))))
    (list (make-region bpos bpos))))

(defmethod check-preconditions ((cmd halt) (state state) group)
  (let ((bot (car (first group))))
    (and (null (cdr (state-bots state)))
         (eq (state-harmonics state) :low)
         (pos-eq (bot-pos bot) (make-point 0 0 0)))))

(defmethod execute ((cmd halt) (state state) group)
  (setf (state-bots state) nil)
  state)

;; Wait
(defclass wait (singleton) ())

(defmethod encode-command ((cmd wait))
  (%bytes 1 #b11111110))

(defmethod get-volatile-regions ((cmd wait) (state state) group)
  (let ((bpos (bot-pos (car (first group)))))
    (list (make-region bpos bpos))))

(defmethod check-preconditions ((cmd wait) (state state) bots)
  t)

(defmethod execute ((cmd wait) (state state) group)
  state)

;; Flip
(defclass flip (singleton) ())

(defmethod encode-command ((cmd flip))
  (%bytes 1 #b11111101))

(defmethod get-volatile-regions ((cmd flip) (state state) group)
  (let ((bpos (bot-pos (car (first group)))))
    (list (make-region bpos bpos))))

(defmethod check-preconditions ((cmd flip) (state state) group)
  t)

(defmethod execute ((cmd flip) (state state) group)
  (if (eq (state-harmonics state) :high)
      (setf (state-harmonics state) :low)
      (setf (state-harmonics state) :high))
  state)

;; Smove
(defclass smove (singleton)
  ((lld :accessor lld :initarg :lld)))

(defmethod print-object ((m smove) s)
  (with-coordinates (x y z) (lld m)
    (cond ((/= x 0) (format s "#<SMOVE X ~A>" x))
          ((/= y 0) (format s "#<SMOVE Y ~A>" y))
          ((/= z 0) (format s "#<SMOVE Z ~A>" z))
          (t (format s "#<MALFORMED SMOVE ~A>" (lld m))))))

(defmethod encode-command ((cmd smove))
  (multiple-value-bind (a i) (encode-lld (lld cmd))
    (%bytes 2 (logior #b00000100 (ash a 4)) (logior #b00000000 i))))

(defmethod get-volatile-regions ((cmd smove) (state state) group)
  (let* ((bpos (bot-pos (car (first group))))
         (nbpos (pos-add bpos (lld cmd))))
    (list (make-region bpos nbpos))))

(defmethod check-preconditions ((cmd smove) (state state) group)
  (let* ((bot (car (first group)))
         (bpos (bot-pos bot))
         (nbpos (pos-add bpos (lld cmd)))
         (region (make-region bpos nbpos)))
    (and (inside-field? nbpos (state-r state))
         (no-full-in-region state region))))

(defmethod execute ((cmd smove) (state state) group)
  (let* ((bot (car (first group)))
         (bpos (bot-pos bot))
         (nbpos (pos-add bpos (lld cmd))))
    (setf (bot-pos bot) nbpos)
    (setf (state-energy state)
          (+ (state-energy state)
             (* 2 (mlen (lld cmd)))))
    state))

;;Lmove
(defclass lmove (singleton)
  ((sld1 :accessor sld1 :initarg :sld1)
   (sld2 :accessor sld2 :initarg :sld2)))

(defmethod print-object ((m lmove) s)
  (format s "#<LMOVE ~A>" (pos-add (sld1 m) (sld2 m))))

(defmethod encode-command ((cmd lmove))
  (multiple-value-bind (a1 i1) (encode-sld (sld1 cmd))
    (multiple-value-bind (a2 i2) (encode-sld (sld2 cmd))
      (let ((b1 (logior #b00001100 (ash a2 6) (ash a1 4)))
            (b2 (logior (ash i2 4) i1)))
        (%bytes 2 b1 b2)))))

(defmethod get-volatile-regions ((cmd lmove) (state state) group)
  (let* ((bpos (bot-pos (car (first group))))
         (mbpos (pos-add bpos (sld1 cmd)))
         (nbpos (pos-add mbpos (sld2 cmd))))
    (list (make-region bpos mbpos)
          (make-region
           ;; Offset by 1 so these two regions do not interfere
           (pos-add mbpos (ident-vec (sld2 cmd)))
           nbpos))))

(defmethod check-preconditions ((cmd lmove) (state state) group)
  (let* ((bot (car (first group)))
         (bpos (bot-pos bot))
         (mbpos (pos-add bpos (sld1 cmd)))
         (nbpos (pos-add mbpos (sld2 cmd)))
         (region1 (make-region bpos mbpos))
         (region2 (make-region mbpos nbpos)))
    (and (inside-field? mbpos (state-r state))
         (inside-field? nbpos (state-r state))
         (no-full-in-region state region1)
         (no-full-in-region state region2))))

(defmethod execute ((cmd lmove) (state state) group)
  (let* ((bot (car (first group)))
         (bpos (bot-pos bot))
         (mbpos (pos-add bpos (sld1 cmd)))
         (nbpos (pos-add mbpos (sld2 cmd))))
    (setf (bot-pos bot) nbpos)
    (setf (state-energy state)
          (+ (state-energy state)
             (* 2 (+ (mlen (sld1 cmd))
                     (mlen (sld2 cmd))
                     2))))
    state))

;;Fission
(defclass fission (singleton)
  ((nd :accessor nd :initarg :nd)
   (m :accessor m :initarg :m)))

(defmethod encode-command ((cmd fission))
  (%bytes 2 (logior #b00000101 (ash (encode-nd (nd cmd)) 3)) (m cmd)))

(defmethod get-volatile-regions ((cmd fission) (state state) group)
  (let* ((bpos (bot-pos (car (first group))))
         (nbpos (pos-add bpos (nd cmd))))
    (list (make-region bpos nbpos))))

(defmethod check-preconditions ((cmd fission) (state state) group)
  (let* ((bot (car (first group)))
         (bpos (bot-pos bot))
         (nbpos (pos-add bpos (nd cmd))))
    (and (bot-seeds bot)
         (inside-field? nbpos (state-r state))
         (voxel-void? state nbpos)
         (> (length (bot-seeds bot)) (m cmd)) ;; N >= M + 1
         )))

(defmethod execute ((cmd fission) (state state) group)
  (let* ((bot (car (first group)))
         (bpos (bot-pos bot))
         (nbpos (pos-add bpos (nd cmd)))
         (seed (car (bot-seeds bot)))
         (rest-seeds (cdr (bot-seeds bot)))
         (m (m cmd))
         (new-seeds (subseq rest-seeds 0 m))
         (old-seeds (subseq rest-seeds m (length rest-seeds)))
         (new-bot (make-instance 'nanobot
                                 :bid seed
                                 :pos nbpos
                                 :seeds new-seeds)))
    (setf (bot-seeds bot) old-seeds)
    (push new-bot (state-bots state))
    (setf (state-energy state) (+ (state-energy state) 24))
    state))

;;Fill
(defclass fill (singleton)
  ((nd :accessor nd :initarg :nd)))

(defmethod encode-command ((cmd fill))
  (%bytes 1 (logior #b00000011 (ash (encode-nd (nd cmd)) 3))))

(defmethod get-volatile-regions ((cmd fill) (state state) group)
  (let* ((bpos (bot-pos (car (first group))))
         (fpos (pos-add bpos (nd cmd))))
    (list (make-region bpos fpos))))

(defmethod check-preconditions ((cmd fill) (state state) group)
  (let* ((bpos (bot-pos (car (first group))))
         (fpos (pos-add bpos (nd cmd))))
    (inside-field? fpos (state-r state))))

(defmethod execute ((cmd fill) (state state) group)
  (let* ((bpos (bot-pos (car (first group))))
         (fpos (pos-add bpos (nd cmd))))
    (if (voxel-void? state fpos)
        (progn
          (fill-voxel state fpos)
          (setf (state-energy state)
                (+ (state-energy state) 12)))
        (setf (state-energy state)
              (+ (state-energy state) 6)))
    state))

;; Void
(defclass void (singleton)
  ((nd :accessor nd :initarg :nd)))

(defmethod encode-command ((cmd void))
  (%bytes 1 (logior #b00000010 (ash (encode-nd (nd cmd)) 3))))

(defmethod get-volatile-regions ((cmd void) (state state) group)
  (let* ((bpos (bot-pos (car (first group))))
         (fpos (pos-add bpos (nd cmd))))
    (list (make-region bpos fpos))))

(defmethod check-preconditions ((cmd void) (state state) group)
  (let* ((bpos (bot-pos (car (first group))))
         (fpos (pos-add bpos (nd cmd))))
    (inside-field? fpos (state-r state))))

(defmethod execute ((cmd void) (state state) group)
  (let* ((bpos (bot-pos (car (first group))))
         (fpos (pos-add bpos (nd cmd))))
    (if (voxel-full? state fpos)
        (progn
          (void-voxel state fpos)
          (setf (state-energy state)
                (- (state-energy state) 12)))
        (setf (state-energy state)
              (+ (state-energy state) 3)))
    state))


;;;------------------------------------------------------------------------------
;;; Group commands
;;;------------------------------------------------------------------------------
(defun check-preconditions-fusion (state bots)
  (let ((fbpos (bot-pos (first bots)))
        (sbpos (bot-pos (second bots)))
        (r (state-r state)))
    (and (inside-field? fbpos r)
         (inside-field? sbpos r))))

;;Fusionp
(defclass fusionp (group)
  ((nd :accessor nd :initarg :nd)))

(defmethod encode-command ((cmd fusionp))
  (%bytes 1 (logior #b00000111 (ash (encode-nd (nd cmd)) 3))))

(defmethod get-volatile-regions ((cmd fusionp) (state state) group)
  (let* ((bot1 (car (first group)))
         (bot2 (car (second group)))
         (pos1 (bot-pos bot1))
         (pos2 (bot-pos bot2)))
    (list (make-region pos1 pos1) (make-region pos2 pos2))))

(defmethod check-preconditions ((cmd fusionp) (state state) group)
  (check-preconditions-fusion state (mapcar #'car group)))

(defmethod execute ((cmd fusionp) (state state) group)
  (let ((bot (car (first group)))
        (sbot (car (second group))))
    (setf (state-bots state) (remove sbot (state-bots state)))
    (setf (bot-seeds bot) (append (bot-seeds sbot) (bot-seeds bot)))
    (setf (state-energy state) (- (state-energy state) 24))
    state))

;;Fusions
(defclass fusions (group)
  ((nd :accessor nd :initarg :nd)))

(defmethod encode-command ((cmd fusions))
  (%bytes 1 (logior #b00000110 (ash (encode-nd (nd cmd)) 3))))

(defmethod get-volatile-regions ((cmd fusions) (state state) group)
  (let* ((bot1 (car (first group)))
         (bot2 (car (second group)))
         (pos1 (bot-pos bot1))
         (pos2 (bot-pos bot2)))
    (list (make-region pos1 pos1) (make-region pos2 pos2))))

(defmethod check-preconditions ((cmd fusions) (state state) group)
  (check-preconditions-fusion state (mapcar #'car group)))

(defmethod execute ((cmd fusions) (state state) group)
  (let ((bot (car (first group)))
        (sbot (car (second group))))
    (setf (state-bots state) (remove sbot (state-bots state)))
    (setf (bot-seeds bot) (append (bot-seeds sbot) (bot-seeds bot)))
    (setf (state-energy state) (- (state-energy state) 24))
    state))

;; GFill
(defun group-region (group)
  (labels ((%pos-< (x y)
             (and (< (aref x 0) (aref y 0))
                  (< (aref x 1) (aref y 1))
                  (< (aref x 2) (aref y 2))))
           (%opposite-corners ()
             (let ((sorted-group (sort (copy-list group) #'%pos-<)))
               (values (first sorted-group) (first (last sorted-group))))))
    (ecase (length group)
      (1 (make-region (first group) (first group)))
      (2 (make-region (first group) (second group)))
      ((4 8) (apply #'make-region (%opposite-corners))))))

(defclass gfill (group)
  ((nd :accessor nd :initarg :nd)
   (fd :accessor fd :initarg :fd)))

(defmethod encode-command ((cmd gfill))
  (multiple-value-bind (b1 b2 b3) (encode-fd (fd cmd))
    (%bytes 4 (logior #b00000001 (ash (encode-nd (nd cmd)) 3)) b1 b2 b3)))

(defmethod get-volatile-regions ((cmd gfill) (state state) group)
  (append (mapcar (lambda (b.c) (let ((p (bot-pos (car b.c)))) (make-region p p))) group)
          (list (group-region group))))

(defun check-gfill/gvoid-preconditions (state group)
  (let ((r-region (group-region group))
        (ndis nil))
    (and
     (every (lambda (b.c)
              (destructuring-bind (bot . cmd) b.c
                (let* ((pos (bot-pos bot))
                       (ndi (pos-add pos (nd cmd))))
                  (and (inside-field? ndi (state-r state))
                       (inside-field? (pos-add ndi (fd cmd)) (state-r state))
                       (not (in-region pos r-region))
                       (not (unless (member ndi ndis :test #'pos-eq) (push ndi ndis)))))))
            group))))

(defmethod check-preconditions ((cmd gfill) (state state) group)
  (check-gfill/gvoid-preconditions state group))

(defmethod execute ((cmd gfill) (state state) group)
  (dolist (c (region-points (group-region group)))
    (if (voxel-void? state c)
        (progn
          (fill-voxel state c)
          (setf (state-energy state) (+ (state-energy state) 12)))
        (setf (state-energy state) (+ (state-energy state) 6)))))

;; GVoid
(defclass gvoid (group)
  ((nd :accessor nd :initarg :nd)
   (fd :accessor fd :initarg :fd)))

(defmethod encode-command ((cmd gvoid))
  (multiple-value-bind (b1 b2 b3) (encode-fd (fd cmd))
    (%bytes 4 (logior #b00000000 (ash (encode-nd (nd cmd)) 3)) b1 b2 b3)))

(defmethod get-volatile-regions ((cmd gvoid) (state state) group)
  (append (mapcar (lambda (b.c) (let ((p (bot-pos (car b.c)))) (make-region p p))) group)
          (list (group-region group))))

(defmethod check-preconditions ((cmd gvoid) (state state) group)
  (check-gfill/gvoid-preconditions state group))

(defmethod execute ((cmd gvoid) (state state) group)
  (dolist (c (region-points (group-region group)))
    (if (voxel-full? state c)
        (progn
          (void-voxel state c)
          (setf (state-energy state) (- (state-energy state) 12)))
        (setf (state-energy state) (+ (state-energy state) 3)))))


;; commands sort
(defun sort-commands-for-bots (bid->cmds count-list)
  "Accepts hash-table of kind bid -> sequence of commands
   and a list of counts of active bids at avery timestep"
  (let ((bids (sort (alexandria:hash-table-keys bid->cmds) #'<))
        (commands nil))
    (labels ((%print ()
               (format t "~%tab:~%")
               (maphash (lambda (bid cmds)
                          (format t "~A: ~A~%" bid cmds))
                        bid->cmds))
             (%sort-cmd (bids)
               (loop :for bid :in bids :do
                    (let ((cmd (pop (gethash bid bid->cmds))))
                      (unless (gethash bid bid->cmds)
                        (remhash bid bid->cmds))
                      (if cmd
                          (push cmd commands)
                          (push (make-instance 'wait) commands))))))

      (loop
         :for n :in count-list
         :until (= (hash-table-count bid->cmds) 0) :do
           (%sort-cmd (take n bids))))

    (reverse commands)))

