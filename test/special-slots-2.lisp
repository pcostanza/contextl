(asdf:oos 'asdf:load-op :contextl)

(in-package :contextl-user)

(define-layered-class person2 ()
  ((name2 :initarg :name
          :layered-accessor person-name2)))

(defparameter *p*
  (make-instance 'person2 :name "Dr. Jekyll"))

(assert (equal (person-name2 *p*) "Dr. Jekyll"))

(symbol-macrolet ((safe-special-symbol-progv t))
  (handler-bind
      ((error (lambda (error)
                (eval '(define-layered-class person2 ()
                         ((name2 :initarg :name
                                 :special t
                                 :layered-accessor person-name2))))
                (assert (equal (person-name2 *p*) "Dr. Jekyll"))
                (continue error))))
    (dletf (((person-name2 *p*) "Mr. Hide"))
      (assert (equal (person-name2 *p*) "Mr. Hide")))))

(assert (equal (person-name2 *p*) "Dr. Jekyll"))

(print :done)

#+abcl (extensions:quit)
#+allegro (excl:exit)
#+clozure (ccl:quit)
#+cmu (ext:quit)
#+ecl (si:quit)
#+sbcl (sb-ext:quit)
