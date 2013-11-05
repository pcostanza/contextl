(in-package :contextl-user)

(define-layered-class figure-element ()
  ())

(define-layered-function move (figure-element dx dy))

(define-layered-class point (figure-element)
  ((x :initarg :x
      :initform 0
      :layered-accessor point-x)
   (y :initarg :y
      :initform 0
      :layered-accessor point-y)))

(define-layered-method move ((elm point) (dx integer) (dy integer))
  (incf (point-x elm) dx)
  (incf (point-y elm) dy))

(define-layered-class line (figure-element)
  ((p1 :initarg :p1
       :initform (make-instance 'point)
       :layered-accessor line-p1)
   (p2 :initarg :p2
       :initform (make-instance 'point)
       :layered-accessor line-p2)))

(define-layered-method move ((elm line) (dx integer) (dy integer))
  (move (line-p1 elm) dx dy)
  (move (line-p2 elm) dx dy))

(deflayer display-layer)

(declaim (type integer *update-count*))
(defparameter *update-count* 0)

(defmacro call-and-update (function object)
  (declare (ignore object))
  `(let ((result (with-inactive-layers (display-layer)
                   (,function))))
     (incf *update-count*)
     result))

(define-layered-method (setf point-x)
  :in display-layer :around (new-value (object point))
  (call-and-update call-next-method object))

(define-layered-method (setf point-y)
  :in display-layer :around (new-value (object point))
  (call-and-update call-next-method object))

(define-layered-method (setf line-p1)
  :in display-layer :around (new-value (object point))
  (call-and-update call-next-method object))

(define-layered-method (setf line-p2)
  :in display-layer :around (new-value (object point))
  (call-and-update call-next-method object))

(define-layered-method move
  :in display-layer :around (object dx dy)
  (call-and-update call-next-method object))

(defconstant +lines+ 100)

(defparameter *lines*
  (loop repeat +lines+
        collect (make-instance
                 'line
                 :p1 (make-instance
                      'point
                      :x (random 100)
                      :y (random 100))
                 :p2 (make-instance
                      'point
                      :x (random 100)
                      :y (random 100)))))

(defun move-lines/non-layered ()
  (loop for line in *lines*
        do (move line 5 -5))
  (loop for line in *lines*
        do (move line -5 5)))

(defun move-lines/layered ()
  (loop for line in *lines*
        do (with-active-layers (display-layer)
             (move line 5 -5)))
  (loop for line in *lines*
        do (with-active-layers (display-layer)
             (move line -5 5))))

(defconstant +runs+ 1000)

(defun run-test ()
  (loop repeat +runs+ do (move-lines/non-layered))
  (setf *update-count* 0)
  (time (loop repeat +runs+ do (move-lines/non-layered)))
  (assert (eql *update-count* 0))
  (time (loop repeat +runs+ do (move-lines/layered)))
  (assert (eql *update-count* (* +lines+ +runs+ 2))))
