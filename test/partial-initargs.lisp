(asdf:load-system :contextl)

(in-package :contextl-user)

(defclass serializable-class (standard-class)
  ((database :initarg :database)))

(defclass combined-class (layered-class serializable-class) 
  ())

(defmethod validate-superclass ((class combined-class) (superclass standard-class)) 
  t)

(defmethod partial-class-base-initargs append ((class combined-class))
  '(:database))

(defclass try ()
  ()
  (:metaclass combined-class)
  (:database . "mydb"))

(finalize-inheritance (find-class 'try))

(assert (string= (slot-value (find-class 'try) 'database) "mydb"))

(assert (loop for class in (rest (class-precedence-list (find-class 'try)))
              never (slot-exists-p class 'database)))

(print :done)
