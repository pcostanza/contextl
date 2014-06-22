(asdf:oos 'asdf:load-op :contextl)

(in-package :contextl-user)

(defclass person3 ()
  ((name3 :initarg :name
          :accessor person-name3))
  (:metaclass special-class))

(defparameter *p*
  (make-instance 'person3 :name "Dr. Jekyll"))

(assert (equal (person-name3 *p*) "Dr. Jekyll"))

(defparameter *error-count* 0)

(symbol-macrolet ((safe-special-symbol-progv t))
  (handler-bind
      ((error (lambda (error)
                (incf *error-count*)
                (eval '(defclass person3 ()
                         ((name3 :initarg :name
                                 :special t
                                 :accessor person-name3))
                         (:metaclass special-class)))
                (assert (equal (person-name3 *p*) "Dr. Jekyll"))
                (continue error))))
    (dletf (((person-name3 *p*) "Mr. Hide"))
      (assert (equal (person-name3 *p*) "Mr. Hide")))))

(assert (eql *error-count* 1))
(assert (equal (person-name3 *p*) "Dr. Jekyll"))

(print :done)
