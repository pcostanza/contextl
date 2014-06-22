(asdf:oos 'asdf:load-op :contextl)

(in-package :contextl-user)

(defvar *adjoined*)
(defvar *removed*)
(defvar *default-context*)
(defvar *new-context*)

(defclass my-layer-class (standard-layer-class) ())

#-cx-disable-layer-gc
(loop repeat 2 do
      (clear-layer-caches)

  (defclass my-layer-class (standard-layer-class) ())

  (define-layered-method adjoin-layer-using-class :after
    ((class my-layer-class) (active-context t))
    (setf *adjoined* t))

  (define-layered-method remove-layer-using-class :after
    ((class my-layer-class) (active-context t))
    (setf *removed* t))

  (deflayer foo () () (:metaclass my-layer-class))
  (deflayer bar () () (:metaclass my-layer-class))
  (deflayer baz (bar) () (:metaclass my-layer-class))

  (setf *default-context* (current-layer-context))
  
;;;
  (print 1)
  
  (adjoin-layer 'foo *default-context*)
  (assert *adjoined*)

  (setf *adjoined* nil)
  (adjoin-layer 'foo *default-context*)
  (assert (not *adjoined*))

  (setf *adjoined* nil)
  (adjoin-layer 'foo *default-context*)
  (assert (not *adjoined*))

  (reinitialize-instance (find-layer-class 'foo))

  (makunbound '*adjoined*)
  (adjoin-layer 'foo *default-context*)
  (assert *adjoined*)

  (setf *adjoined* nil)
  (adjoin-layer 'foo *default-context*)
  (assert (not *adjoined*))

  (setf *adjoined* nil)
  (adjoin-layer 'foo *default-context*)
  (assert (not *adjoined*))

;;;
  (print 2)
  
  (remove-layer 'foo *default-context*)
  (assert *removed*)

  (setf *removed* nil)
  (remove-layer 'foo *default-context*)
  (assert (not *removed*))

  (setf *removed* nil)
  (remove-layer 'foo *default-context*)
  (assert (not *removed*))

  (reinitialize-instance (find-layer-class 'foo))

  (makunbound '*removed*)
  (remove-layer 'foo *default-context*)
  (assert *removed*)

  (setf *removed* nil)
  (remove-layer 'foo *default-context*)
  (assert (not *removed*))

  (setf *removed* nil)
  (remove-layer 'foo *default-context*)
  (assert (not *removed*))

;;;
  (print 3)
  
  (setf *new-context* (adjoin-layer 'foo *default-context*))

  (makunbound '*adjoined*)
  (adjoin-layer 'baz *new-context*)
  (assert *adjoined*)

  (setf *adjoined* nil)
  (adjoin-layer 'baz *new-context*)
  (assert (not *adjoined*))

  (setf *adjoined* nil)
  (adjoin-layer 'baz *new-context*)
  (assert (not *adjoined*))

  (reinitialize-instance (find-layer-class 'bar))

  (setf *adjoined* nil)
  (adjoin-layer 'baz *new-context*)
  (assert *adjoined*)

  (setf *adjoined* nil)
  (adjoin-layer 'baz *new-context*)
  (assert (not *adjoined*))

  (setf *adjoined* nil)
  (adjoin-layer 'baz *new-context*)
  (assert (not *adjoined*))

;;;
  (print 4)
  
  (setf *new-context* (remove-layer 'foo *default-context*))

  (makunbound '*removed*)
  (remove-layer 'baz *new-context*)
  (assert *removed*)

  (setf *removed* nil)
  (remove-layer 'baz *new-context*)
  (assert (not *removed*))

  (setf *removed* nil)
  (remove-layer 'baz *new-context*)
  (assert (not *removed*))

  (reinitialize-instance (find-layer-class 'bar))

  (setf *removed* nil)
  (remove-layer 'baz *new-context*)
  (assert *removed*)

  (setf *removed* nil)
  (remove-layer 'baz *new-context*)
  (assert (not *removed*))

  (setf *removed* nil)
  (remove-layer 'baz *new-context*)
  (assert (not *removed*))

;;;
  (print 5)

  (setf *new-context* (adjoin-layer 'foo *default-context*))

  (makunbound '*adjoined*)
  (adjoin-layer 'bar *new-context*)
  (assert *adjoined*)

  (setf *adjoined* nil)
  (adjoin-layer 'bar *new-context*)
  (assert (not *adjoined*))

  (define-layered-method adjoin-layer-using-class :before
    ((class my-layer-class) (active-context t))
    '())

  (setf *adjoined* nil)
  (adjoin-layer 'bar *new-context*)
  (assert *adjoined*)

  (setf *adjoined* nil)
  (adjoin-layer 'bar *new-context*)
  (assert (not *adjoined*))

;;;
  (print 6)

  (setf *new-context* (remove-layer 'foo *default-context*))

  (makunbound '*removed*)
  (remove-layer 'bar *new-context*)
  (assert *removed*)

  (setf *removed* nil)
  (remove-layer 'bar *new-context*)
  (assert (not *removed*))

  (define-layered-method remove-layer-using-class :before
    ((class my-layer-class) (active-context t))
    '())

  (setf *removed* nil)
  (remove-layer 'bar *new-context*)
  (assert *removed*)

  (setf *removed* nil)
  (remove-layer 'bar *new-context*)
  (assert (not *removed*))

;;;
  (print 7)

  (setf *new-context* (adjoin-layer 'foo *default-context*))

  (makunbound '*adjoined*)
  (adjoin-layer 'bar *new-context*)
  (assert *adjoined*)

  (setf *adjoined* nil)
  (adjoin-layer 'bar *new-context*)
  (assert (not *adjoined*))

  (define-layered-method adjoin-layer-using-class :before
    ((class (eql (find-layer-class 'bar))) (active-context t))
    '())

  (setf *adjoined* nil)
  (adjoin-layer 'bar *new-context*)
  (assert *adjoined*)

  (setf *adjoined* nil)
  (adjoin-layer 'bar *new-context*)
  (assert (not *adjoined*))

;;;
  (print 8)

  (clear-layer-caches)

  (setf *new-context* (remove-layer 'foo *default-context*))

  (makunbound '*removed*)
  (remove-layer 'bar *new-context*)
  (assert *removed*)

  (setf *removed* nil)
  (remove-layer 'bar *new-context*)
  (assert (not *removed*))

  (define-layered-method remove-layer-using-class :before
    ((class (eql (find-layer-class 'bar))) (active-context t))
    '())

  (setf *removed* nil)
  (remove-layer 'bar *new-context*)
  (assert *removed*)

  (setf *removed* nil)
  (remove-layer 'bar *new-context*)
  (assert (not *removed*))

  (print :done))

#+cx-disable-layer-gc
(print "Layer GC not supported.")
