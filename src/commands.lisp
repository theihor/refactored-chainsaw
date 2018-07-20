(uiop:define-package :src/commands
    (:use :common-lisp)
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

(defmacro %bytes (size &rest bytes)
  `(make-array ,size :element-type '(unsigned-byte 8) :initial-contents ',bytes))


;;;------------------------------------------------------------------------------
;;; Singleton commands
;;;------------------------------------------------------------------------------
(defclass halt (singleton) ())

(defmethod encode-command ((cmd halt))
  (%bytes 1 #b11111111))

(defclass wait (singleton) ())

(defmethod encode-command ((cmd wait))
  (%bytes 1 #b11111110))

(defclass flip (singleton) ())

(defmethod encode-command ((cmd flip))
  (%bytes 1 #b11111101))

(defclass smove (singleton)
  ((lld :accessor lld :initarg :lld)))

(defmethod encode-command ((cmd smove))
  ;; TODO(whythat): implement
  )

(defclass lmove (singleton)
  ((sld1 :accessor sld1 :initarg :sld1)
   (sld2 :accessor sld2 :initarg :sld2)))

(defmethod encode-command ((cmd lmove))
  ;; TODO(whythat): implement
  )

(defclass fission (singleton)
  ((nd :accessor nd :initarg :nd)
   (m :accessor m :initarg :m)))

(defmethod encode-command ((cmd fission))
  ;; TODO(whythat): implement
  )

(defclass fill (singleton)
  ((nd :accessor nd :initarg :nd)))

(defmethod encode-command ((cmd fill))
  ;; TODO(whythat): implement
  )

;;;------------------------------------------------------------------------------
;;; Group commands
;;;------------------------------------------------------------------------------
(defclass fusionp (group)
  ((nd :accessor nd :initarg :nd)))

(defmethod encode-command ((cmd fusionp))
  ;; TODO(whythat): implement
  )

(defclass fusions (group)
  ((nd :accessor nd :initarg :nd)))

(defmethod encode-command ((cmd fusions))
  ;; TODO(whythat): implement
  )
