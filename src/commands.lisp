(uiop:define-package :src/commands
    (:use :common-lisp)
  (:use :src/coordinates
        :src/state)
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
   #:fusionp
   #:fusions
   ;; command operations
   #:encode-commands
   #:decode-commands

   #:get-volatile-regions
   #:check-preconditions
   #:execute
   ))

(in-package :src/commands)

;;;------------------------------------------------------------------------------
;;; Generic definitions and utils
;;;------------------------------------------------------------------------------
(defclass singleton ()
  ())

(defclass group ()
  ())

(defgeneric encode-command (cmd))

(defgeneric get-volatile-regions (cmd bot)
  (:documentation "Return list of regions with volatile points"))

(defgeneric check-preconditions (cmd state bots)
  (:documentation "Check if command is valid"))

(defgeneric execute (cmd state bot)
  (:documentation "Execute command and change states"))

(defmacro %bytes (size &rest bytes)
  `(make-array ,size :element-type '(unsigned-byte 8) :initial-contents (list ,@bytes)))

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
                    ((= (logand b #b00000111) #b00000101) ; fission
                     (let ((m (read-byte stream))
                           (nd (ash (logand b #b11111000) -3)))
                       (make-instance 'fission :m m :nd (decode-nd nd))))
                    ((= (logand b #b00000111) #b00000011) ; fill
                     (let ((nd (ash (logand b #b11111000) -3)))
                       (make-instance 'fill :nd (decode-nd nd)))))))

(defun encode-commands (commands)
  "Encode commands and return octet vector."
  (apply #'concatenate 'vector (mapcar #'encode-command commands)))


;;;------------------------------------------------------------------------------
;;; Singleton commands
;;;------------------------------------------------------------------------------

;; Halt
(defclass halt (singleton) ())

(defmethod encode-command ((cmd halt))
  (%bytes 1 #b11111111))

(defmethod get-volatile-regions ((cmd halt) (bot nanobot))
  (let ((bpos (bot-pos bot)))
    (list (make-region bpos bpos))))

(defmethod check-preconditions ((cmd halt) (state state) bots)
  (let ((bot (car bots)))
    (and (null (cdr (state-bots state)))
         (eq (state-harmonics state) :low)
         (pos-eq (bot-pos bot) (make-point 0 0 0)))))

;; Wait
(defclass wait (singleton) ())

(defmethod encode-command ((cmd wait))
  (%bytes 1 #b11111110))

(defmethod get-volatile-regions ((cmd wait) (bot nanobot))
  (let ((bpos (bot-pos bot)))
    (list (make-region bpos bpos))))

(defmethod check-preconditions ((cmd wait) (state state) bots)
  t)

;;Flip
(defclass flip (singleton) ())

(defmethod encode-command ((cmd flip))
  (%bytes 1 #b11111101))

(defmethod get-volatile-regions ((cmd flip) (bot nanobot))
  (let ((bpos (bot-pos bot)))
    (list (make-region bpos bpos))))

(defmethod check-preconditions ((cmd flip) (state state) bots)
  t)

;;Smove
(defclass smove (singleton)
  ((lld :accessor lld :initarg :lld)))

(defmethod encode-command ((cmd smove))
  (multiple-value-bind (a i) (encode-lld (lld cmd))
    (%bytes 2 (logior #b00000100 (ash a 4)) (logior #b00000000 i))))

(defmethod get-volatile-regions ((cmd smove) (bot nanobot))
  (let* ((bpos (bot-pos bot))
         (nbpos (pos-add bpos (lld cmd))))
    (list (make-region bpos nbpos))))

(defmethod check-preconditions ((cmd smove) (state state) bots)
  (let* ((bot (car bots))
         (bpos (bot-pos bot))
         (nbpos (pos-add bpos (lld cmd)))
         (region (make-region bpos nbpos)))
    (and (inside-field? nbpos (state-r state))
         (no-full-in-region state region))))

;;Lmove 
(defclass lmove (singleton)
  ((sld1 :accessor sld1 :initarg :sld1)
   (sld2 :accessor sld2 :initarg :sld2)))

(defmethod encode-command ((cmd lmove))
  (multiple-value-bind (a1 i1) (encode-sld (sld1 cmd))
    (multiple-value-bind (a2 i2) (encode-sld (sld2 cmd))
      (let ((b1 (logior #b00001100 (ash a2 6) (ash a1 4)))
            (b2 (logior (ash i2 4) i1)))
        (%bytes 2 b1 b2)))))

(defmethod get-volatile-regions ((cmd lmove) (bot nanobot))
  (let* ((bpos (bot-pos bot))
         (mbpos (pos-add bpos (sld1 cmd)))
         (nbpos (pos-add mbpos (sld2 cmd))))
    (list (make-region bpos mbpos) (make-region mbpos nbpos))))

(defmethod check-preconditions ((cmd lmove) (state state) bots)
  (let* ((bot (car bots))
         (bpos (bot-pos bot))
         (mbpos (pos-add bpos (sld1 cmd)))
         (nbpos (pos-add mbpos (sld2 cmd)))
         (region1 (make-region bpos mbpos))
         (region2 (make-region mbpos nbpos)))
    (and (inside-field? mbpos (state-r state))
         (inside-field? nbpos (state-r state))
         (no-full-in-region state region1)
         (no-full-in-region state region2))))

;;Fission
(defclass fission (singleton)
  ((nd :accessor nd :initarg :nd)
   (m :accessor m :initarg :m)))

(defmethod encode-command ((cmd fission))
  (%bytes 2 (logior #b00000101 (encode-nd (nd cmd))) (m cmd)))

(defmethod get-volatile-regions ((cmd fission) (bot nanobot))
  (let* ((bpos (bot-pos bot))
         (nbpos (pos-add bpos (nd cmd))))
    (list (make-region bpos nbpos))))

(defmethod check-preconditions ((cmd fission) (state state) bots)
  (let* ((bot (car bots))
         (bpos (bot-pos bot))
         (nbpos (pos-add bpos (nd cmd))))
    (and (bot-seeds bot)
         (inside-field? nbpos (state-r state))
         (voxel-void? state nbpos)
         (> (length (bot-seeds bot)) (m cmd)) ;; N >= M + 1
         )))

;;Fill
(defclass fill (singleton)
  ((nd :accessor nd :initarg :nd)))

(defmethod encode-command ((cmd fill))
  (%bytes 1 (logior #b00000011 (ash (encode-nd (nd cmd)) 3))))

(defmethod get-volatile-regions ((cmd fill) (bot nanobot))
  (let* ((bpos (bot-pos bot))
         (fpos (pos-add bpos (nd cmd))))
    (list (make-region bpos fpos))))

(defmethod check-preconditions ((cmd fission) (state state) bots)
  (let* ((bpos (bot-pos (car bots)))
         (nbpos (pos-add bpos (nd cmd))))
    (inside-field? nbpos (state-r state))))

;;;------------------------------------------------------------------------------
;;; Group commands
;;;------------------------------------------------------------------------------
(defun check-preconditions-fussion (state bots)
  (let ((fbpos (bot-pos (first bots)))
        (sbpos (bot-pos (second bots)))
        (r (state-r state)))
    (and (inside-field? fbpos r)
         (inside-field? sbpos r))))

;;Fusionp
(defclass fusionp (group)
  ((nd :accessor nd :initarg :nd)))

(defmethod encode-command ((cmd fusionp))
  (%bytes 1 (logior #b00000111 (encode-nd (nd cmd)))))

(defmethod get-volatile-regions ((cmd fusionp) (bot nanobot))
  (let ((bpos (bot-pos bot)))
    (list (make-region bpos bpos))))

(defmethod check-preconditions ((cmd fusionp) (state state) bots)
  (check-preconditions-fussion state bots))

;;Fusions
(defclass fusions (group)
  ((nd :accessor nd :initarg :nd)))

(defmethod encode-command ((cmd fusions))
  (%bytes 1 (logior #b00000110 (encode-nd (nd cmd)))))

(defmethod get-volatile-regions ((cmd fusions) (bot nanobot))
  (let ((bpos (bot-pos bot)))
    (list (make-region bpos bpos))))

(defmethod check-preconditions ((cmd fusions) (state state) bots)
  (check-preconditions-fussion state bots))
