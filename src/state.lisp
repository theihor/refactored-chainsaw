(uiop:define-package :src/state
  (:use :common-lisp
        :src/coordinates)
  (:export #:well-formed?
           #:matrix-index
           #:voxel-state
           #:set-voxel-state
           #:get-voxel

           #:state
           #:state-energy
           #:state-harmonics
           #:state-bots
           #:state-trace
           #:state-matrix
           #:state-r

           #:nanobot
           #:bot-pos
           #:bot-trace
           #:bot-bid
           #:bot-seeds
   ))

(in-package :src/state)

;; matrix M is an array of bits
;; 1 <=> Full
;; 0 <=> Void

(defun matrix-index (c)
  "Returns index of coordinate `c' in matrix bitarray"
  (with-coordinates (x y z) c
    (let ((i (+ z (* +dimensions+ y) (* +dimensions+ +dimensions+ x))))
      i)))

(defun voxel-state (c m)
  "Returns a state of the voxel at coordinate `c' in matrix `m' as Full (1) or Void (0).
   `r' is the resolution of the matrix"
  (aref m (matrix-index c)))

(defun get-voxel (state c)
  (voxel-state c (state-matrix state)))

(defun set-voxel-state (s c m)
  (setf (aref m (matrix-index c)) s))

;; TODO: implement check if M grounded
(defun grounded? (m)
  nil)

(defstruct state
  (energy 0 :type integer)                   ;; the amount of energy expended
  (harmonics :low :type (member :low :high)) ;; the (global) field harmonics
  matrix                                     ;; the matrix of voxels (each voxel either Full or Empty)
  (bots nil :type list)                      ;; the set of active nanobots
  (trace nil :type list)                     ;; the sequence of commands to be performed by nanobots
  r                                          ;; resolution of the system
  )

(defclass nanobot ()
  ((bid :initarg :bid
        :type integer
        :accessor bot-bid
        :documentation "the (unique) identifier (a positive integer)")
   (pos :initarg :pos
        :type array ;; coordinate #(x y z)
        :accessor bot-pos)
   (seeds :initarg :seeds
          :initform nil
          :type list
          :accessor bot-seeds
          :documentation "the set of identifiers available for fission")
  ))

(defun well-formed? (s)
  (and (if (eq (state-harmonics s) :low)
           (grounded? (state-matrix s))
           t)
       (loop :for (b . rest) :on (state-bots s) :do
            (unless (every (lambda (b1)
                             (and (not (= (bot-bid b1)
                                          (bot-bid b)))
                                  (not (pos-eq (bot-pos b1)
                                               (bot-pos b)))))
                           rest)
              (return-from well-formed? nil))
            (every (lambda (seed)
                     (not (member seed (state-bots s) :key #'bot-bid :test #'=)))
                   (bot-seeds b)))
       ;; TODO: clarify 'The seeds of each active nanobot are disjoint.'
       ))
