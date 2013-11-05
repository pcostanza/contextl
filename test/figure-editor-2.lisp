(in-package :contextl-user)

(define-layered-class figure-element-2 ()
  ())

(define-layered-function move-2 (figure-element dx dy))

(define-layered-class point-2 (figure-element-2)
  ((x :initarg :x :initform 0 :layered t :accessor point-x-2)
   (y :initarg :y :initform 0 :layered t :accessor point-y-2)))

(define-layered-method move-2 ((elm point-2) (dx integer) (dy integer))
  (incf (point-x-2 elm) dx)
  (incf (point-y-2 elm) dy))

(define-layered-class line-2 (figure-element-2)
  ((p1 :initarg :p1 :initform (make-instance 'point-2) :layered t :accessor line-p1-2)
   (p2 :initarg :p2 :initform (make-instance 'point-2) :layered t :accessor line-p2-2)))

(define-layered-method move-2 ((elm line-2) (dx integer) (dy integer))
  (move-2 (line-p1-2 elm) dx dy)
  (move-2 (line-p2-2 elm) dx dy))

(deflayer display-layer-2)

(declaim (type integer *update-count-2*))
(defparameter *update-count-2* 0)

(defun call-and-update-2 (thunk)
  (let ((result (with-inactive-layers (display-layer-2)
                  (funcall thunk))))
    (incf *update-count-2*)
    result))

(define-layered-method (setf slot-value-using-layer)
  :in display-layer-2 :around
  (new-value class (object figure-element-2) slot writer)
  (call-and-update-2 (lambda () (funcall writer new-value))))

(define-layered-method move-2
  :in display-layer-2 :around
  ((elm figure-element-2) dx dy)
  (call-and-update-2 #'call-next-method))

(defconstant +lines-2+ 100)

(defparameter *lines-2*
  (loop repeat +lines-2+
        collect (make-instance
                 'line-2
                 :p1 (make-instance
                      'point-2
                      :x (random 100)
                      :y (random 100))
                 :p2 (make-instance
                      'point-2
                      :x (random 100)
                      :y (random 100)))))

(defun move-lines/non-layered-2 ()
  (loop for line in *lines-2*
        do (move-2 line 5 -5))
  (loop for line in *lines-2*
        do (move-2 line -5 5)))

(defun move-lines/layered-2 ()
  (loop for line in *lines-2*
        do (with-active-layers (display-layer-2)
             (move-2 line 5 -5)))
  (loop for line in *lines-2*
        do (with-active-layers (display-layer-2)
             (move-2 line -5 5))))

(defconstant +runs-2+ 1000)

(defun run-test-2 ()
  (setf *update-count-2* 0)
  (time (loop repeat +runs-2+ do (move-lines/non-layered-2)))
  (assert (eql *update-count-2* 0))
  (time (loop repeat +runs-2+ do (move-lines/layered-2)))
  (assert (eql *update-count-2* (* +lines-2+ +runs-2+ 2))))
