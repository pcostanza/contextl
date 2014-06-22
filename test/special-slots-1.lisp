(asdf:oos 'asdf:load-op :contextl)

(in-package :contextl-user)

(define-layered-class person1 ()
  ((name1 :initarg :name
          :accessor person-name1)))

(defparameter *p*
  (make-instance 'person1 :name "Dr. Jekyll"))

(assert (equal (person-name1 *p*) "Dr. Jekyll"))

(symbol-macrolet ((safe-special-symbol-progv t))
  (handler-bind
      ((error (lambda (error)
                (eval '(define-layered-class person1 ()
                         ((name1 :initarg :name
                                 :special t
                                 :accessor person-name1))))
                (assert (equal (person-name1 *p*) "Dr. Jekyll"))
                (continue error))))
    (dletf (((person-name1 *p*) "Mr. Hide"))
      (assert (equal (person-name1 *p*) "Mr. Hide")))))

(assert (equal (person-name1 *p*) "Dr. Jekyll"))

(print :done)
