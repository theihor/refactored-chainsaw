(uiop:define-package :src/model
    (:use :common-lisp
          :src/coordinates
          :src/state)
  (:export
   #:read-model
   #:read-model-from-file
   #:read-models-in-dir))

(in-package :src/model)


(defconstant +dimensions+ 3)


(defclass model ()
  ((resolution :accessor model-resolution
               :initarg :resolution
               :initform 1)
   ;; coordinate is array of three elements, see src/coordinate.lisp
   (coordinates :accessor model-coordinates
                :initarg :coordinates
                :initform nil)))

(defclass extended-model (model)
    ((nanobots :accessor ext-model-nanobots
               :initarg :nanobots
               :initform nil)))


(defun decode-coordinate (encoded-coordinate-index bits-read-so-far resolution)
  (let ((stream-position
         (+ encoded-coordinate-index bits-read-so-far)))
    (let ((number-of-z-rows (floor stream-position resolution)))
      (point (floor number-of-z-rows resolution)
             (mod number-of-z-rows resolution)
             (mod stream-position resolution)))))

;;; Sizes in bits
(defconstant +chunk-size+ 8)
(defconstant +encoded-coordinate-size+ 1)

(defun is-encoded-coordinate-full (encoded-coordinate)
  "0 - Void, 1 - Full"
  (not (zerop encoded-coordinate)))

(defun read-encoded-coordinate (chunk index)
  (ldb (byte +encoded-coordinate-size+ index) chunk))

(defun decode-full-coordinates (chunk bits-read-so-far resolution)
  (loop
     for encoded-coordinate-index = 0
     then (+ encoded-coordinate-index +encoded-coordinate-size+)

     while (< encoded-coordinate-index +chunk-size+)
     unless
       (is-encoded-coordinate-full
        (read-encoded-coordinate chunk encoded-coordinate-index))
     collect
       (decode-coordinate encoded-coordinate-index bits-read-so-far resolution)))

(defun to-bit-array (full-coordinates resolution)
  (let ((bit-array (make-array (* resolution resolution resolution) :element-type 'bit)))
    (dolist (c full-coordinates bit-array)
      (set-voxel-state 1 c bit-array resolution))))

(defun read-full-coordinates (resolution stream)
  "Reads full coordinates from STREAM with given RESOLUTION.
Assumes RESOLUTION^(+DIMENSIONS+) bits"
  (let ((expected-size (expt resolution +dimensions+)))
    (to-bit-array
     (loop
        for bits-read-so-far = 0 then (+ bits-read-so-far +chunk-size+)
        while (< bits-read-so-far expected-size)

        for chunk = (read-byte stream)
        unless (zerop chunk)
        nconc (decode-full-coordinates chunk bits-read-so-far resolution))
     resolution)))

(defun read-resolution (stream)
  "Reads resolution for a model"
  (read-byte stream))

(defun read-model (stream)
  "Reads a model from STREAM.
Reads the first byte to determine the resolution. Then reads using 8 bit chunks."
  (let ((mdl (make-instance 'model)))
    (with-slots (resolution coordinates) mdl
      (setf resolution (read-resolution stream))
      (setf coordinates (read-full-coordinates resolution stream)))
    mdl))

(defun read-model-from-file (filename)
  (with-open-file (stream filename :element-type '(unsigned-byte 8))
    (read-model stream)))


;;; Tests
(defun read-models-in-dir (dirname)
  (labels ((%is-model-file (filename)
             (string= "mdl" (pathname-type filename))))
    (let ((model-files
           (delete-if-not #'%is-model-file (cl-fad:list-directory dirname))))
      (dolist (filename model-files)
        (let ((model (read-model-from-file filename)))
          (format t "~A read successfully: R = ~A, coordinates = ~A~%"
                  (pathname-name filename)
                  (model-resolution model)
                  (length (model-coordinates model))))))))
