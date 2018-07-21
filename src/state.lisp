(uiop:define-package :src/state
    (:use :common-lisp
          :anaphora
        :src/coordinates)
  (:export #:matrix-index
           #:voxel-state
           #:set-voxel-state
           #:get-voxel
           #:fill-voxel

           #:state
           #:make-state
           #:energy
           #:state-energy
           #:harmonics
           #:state-harmonics
           #:bots
           #:state-bots
           #:trace
           #:state-trace
           #:matrix
           #:state-matrix
           #:r
           #:state-r

           #:nanobot
           #:bot-pos
           #:bot-trace
           #:bot-bid
           #:bot-seeds

           #:no-full-in-region
           #:voxel-full?
           #:voxel-void?
           #:read-nanobots
   ))

(in-package :src/state)

;; matrix M is an array of bits
;; 1 <=> Full
;; 0 <=> Void

(defun matrix-index (c r)
  "Returns index of coordinate `c' in matrix bitarray"
  (with-coordinates (x y z) c
    (let ((i (+ z (* r y) (* r r x))))
      i)))

(defun voxel-state (c m r)
  "Returns a state of the voxel at coordinate `c' in matrix `m' as Full (1) or Void (0).
   `r' is the resolution of the matrix"
  (aref m (matrix-index c r)))

(defun get-voxel (state c)
  (voxel-state c (state-matrix state) (state-r state)))

(defun fill-voxel (state c)
  (set-voxel-state 1 c (state-matrix state) (state-r state)))

(defun set-voxel-state (s c m r)
  (setf (aref m (matrix-index c r)) s))

(defun voxel-full? (state c)
  (= (get-voxel state c) 1))

(defun voxel-void? (state c)
  (= (get-voxel state c) 0))

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



(defun no-full-in-region (state region)
  (not (some (lambda (p) (voxel-full? state p))
             (region-points region))))

(defun read-nanobot-coordinate (stream)
  "Reads coordinate for nanobot from STREAM"
  (make-point (read-byte stream)
              (read-byte stream)
              (read-byte stream)))

(defun read-nanobot (stream)
  "Reads a single nanobot from strea. Returns nil in case of EOF"
  (awhen (read-byte stream nil nil)
    (make-instance 'nanobot
                   :bid it
                   :pos (read-nanobot-coordinate stream))))

(defun read-nanobots (stream)
  "Reads nanobots for extended model"
  (loop for nanobot = (read-nanobot stream)
     while nanobot
     collect nanobot))
