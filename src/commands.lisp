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
   #:encode-command
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

(defgeneric get-volotile-regions (cmd bot)
  (:documentation "Return list of regions with volotile points"))

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


;;;------------------------------------------------------------------------------
;;; Singleton commands
;;;------------------------------------------------------------------------------

;; Halt
(defclass halt (singleton) ())

(defmethod encode-command ((cmd halt))
  (%bytes 1 #b11111111))

(defmethod get-volotile-regions ((cmd halt) (bot nanobot))
  (let ((bpos (bot-pos bot)))
    (list (make-region bpos bpos))))

;; Wait
(defclass wait (singleton) ())

(defmethod encode-command ((cmd wait))
  (%bytes 1 #b11111110))

(defmethod get-volotile-regions ((cmd wait) (bot nanobot))
  (let ((bpos (bot-pos bot)))
    (list (make-region bpos bpos))))

;;Flip
(defclass flip (singleton) ())

(defmethod encode-command ((cmd flip))
  (%bytes 1 #b11111101))

(defmethod get-volotile-regions ((cmd flip) (bot nanobot))
  (let ((bpos (bot-pos bot)))
    (list (make-region bpos bpos))))

;;Smove
(defclass smove (singleton)
  ((lld :accessor lld :initarg :lld)))

(defmethod encode-command ((cmd smove))
  (multiple-value-bind (a i) (encode-lld (lld cmd))
    (%bytes 2 (logior #b00000100 (ash a 4)) (logior #b00000000 i))))

(defmethod get-volotile-regions ((cmd smove) (bot nanobot))
  (let* ((bpos (bot-pos bot))
         (nbpos (pos-add bpos (lld cmd))))
    (list (make-region bpos nbpos))))

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

(defmethod get-volotile-regions ((cmd lmove) (bot nanobot))
  (let* ((bpos (bot-pos bot))
         (mbpos (pos-add bpos (sld1 cmd)))
         (nbpos (pos-add mbpos (sld2 cmd))))
    (list (make-region bpos mbpos) (make-region mbpos nbpos))))

;;Fission
(defclass fission (singleton)
  ((nd :accessor nd :initarg :nd)
   (m :accessor m :initarg :m)))

(defmethod encode-command ((cmd fission))
  (%bytes 2 (logior #b00000101 (encode-nd (nd cmd))) (m cmd)))

(defmethod get-volotile-regions ((cmd fission) (bot nanobot))
  (let* ((bpos (bot-pos bot))
         (nbpos (pos-add bpos (nd cmd))))
    (list (make-region bpos nbpos))))

;;Fill
(defclass fill (singleton)
  ((nd :accessor nd :initarg :nd)))

(defmethod encode-command ((cmd fill))
  (%bytes 1 (logior #b00000011 (encode-nd (nd cmd)))))

(defmethod get-volotile-regions ((cmd fill) (bot nanobot))
  (let* ((bpos (bot-pos bot))
         (fpos (pos-add bpos (nd cmd))))
    (list (make-region bpos fpos))))

;;;------------------------------------------------------------------------------
;;; Group commands
;;;------------------------------------------------------------------------------
;;Fusionp
(defclass fusionp (group)
  ((nd :accessor nd :initarg :nd)))

(defmethod encode-command ((cmd fusionp))
  (%bytes 1 (logior #b00000111 (encode-nd (nd cmd)))))

(defmethod get-volotile-regions ((cmd fusionp) (bot nanobot))
  (let ((bpos (bot-pos bot)))
    (list (make-region bpos bpos))))

;;Fusions
(defclass fusions (group)
  ((nd :accessor nd :initarg :nd)))

(defmethod encode-command ((cmd fusions))
  (%bytes 1 (logior #b00000110 (encode-nd (nd cmd)))))

(defmethod get-volotile-regions ((cmd fusions) (bot nanobot))
  (let ((bpos (bot-pos bot)))
    (list (make-region bpos bpos))))
