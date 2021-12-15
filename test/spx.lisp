(asdf:oos 'asdf:load-op :contextl)

(in-package :contextl-user)

(setf (find-class 'test) nil)

(defclass test ()
  ((slot0 :initarg :slot0 :special t :reader tslot0)
   (slot1 :initarg :slot1 :initform 'foo :special t :allocation :class :reader tslot1))
  (:metaclass special-class))

(ensure-finalized (find-class 'test))

(assert (eq (tslot1 (class-prototype (find-class 'test))) 'foo))

(assert (eq (slot-value (class-prototype (find-class 'test)) 'slot1) 'foo))

(defparameter *t* (make-instance 'test :slot0 4711 :slot1 'bar))

(assert (eql (tslot0 *t*) 4711))

(assert (eql (slot-value *t* 'slot0) 4711))

(assert (eq (tslot1 *t*) 'bar))

(assert (eq (slot-value *t* 'slot1) 'bar)) 

(assert (eq (tslot1 (class-prototype (find-class 'test))) 'bar))

(assert (eq (slot-value (class-prototype (find-class 'test)) 'slot1) 'bar))

(reinitialize-instance *t* :slot0 42 :slot1 'baz)

(assert (eql (tslot0 *t*) 42))

(assert (eql (slot-value *t* 'slot0) 42))

(assert (eq (tslot1 *t*) 'baz))

(assert (eq (slot-value *t* 'slot1) 'baz))

(assert (eq (tslot1 (class-prototype (find-class 'test))) 'baz))

(assert (eq (slot-value (class-prototype (find-class 'test)) 'slot1) 'baz))


(deflayer test-layer ()
  ((slot0 :initarg :slot0 :initform 'foo :reader slot0 :special t)
   (slot1 :initarg :slot1 :initform 'bar :reader slot1 :special t)))

(assert (eq (slot0 (find-layer 'test-layer)) 'foo))

(assert (eq (slot1 (find-layer 'test-layer)) 'bar))

(with-active-layers ((test-layer :slot0 4711))
  (assert (eql (slot0 (find-layer 'test-layer)) 4711))
  (assert (eq (slot1 (find-layer 'test-layer)) 'bar))
  (setf (slot-value (find-layer 'test-layer) 'slot0) 111)
  (setf (slot-value (find-layer 'test-layer) 'slot1) 222)
  (assert (eql (slot0 (find-layer 'test-layer)) 111))
  (assert (eql (slot1 (find-layer 'test-layer)) 222)))

(assert (eq (slot0 (find-layer 'test-layer)) 'foo))
(assert (eql (slot1 (find-layer 'test-layer)) 222))

(defparameter *counter* 0)
(defparameter *check-counter* 0)

(defclass class1 ()
  ((some-slot :initform (incf *counter*) :reader some-slot))
  (:metaclass singleton-class))

(incf *check-counter*)

(ensure-finalized (find-class 'class1))

(assert (eql (some-slot (class-prototype (find-class 'class1))) *check-counter*))

(defclass class1 ()
  ((some-slot :initform (incf *counter*) :reader some-slot))
  (:metaclass singleton-class))

#+(or abcl cmu ecl)
(incf *check-counter*)

(assert (eql (some-slot (class-prototype (find-class 'class1))) *check-counter*))

(defclass class1 ()
  ((some-slot :initform (incf *counter*) :reader some-slot :reinitialize t))
  (:metaclass singleton-class))

#-(or lispworks6.1 lispworks7 lispworks8)
(incf *check-counter*)

(assert (eql (some-slot (class-prototype (find-class 'class1))) *check-counter*))

(defclass class2 ()
  ((some-slot :initform (incf *counter*) :reader some-slot :reinitialize t))
  (:metaclass singleton-class))

#-(or lispworks6.1 lispworks7 lispworks8)
(incf *check-counter*)
#+(or lispworks6.1 lispworks7 lispworks8)
(incf *check-counter* 3)

(ensure-finalized (find-class 'class2))

(assert (eql (some-slot (class-prototype (find-class 'class2))) *check-counter*))

(defclass class2 ()
  ((some-slot :initform (incf *counter*) :reader some-slot :reinitialize t))
  (:metaclass singleton-class))

#-(or lispworks6.1 lispworks7 lispworks8)
(incf *check-counter*)

(assert (eql (some-slot (class-prototype (find-class 'class2))) *check-counter*))

(defclass class2 ()
  ((some-slot :initform (incf *counter*) :reader some-slot))
  (:metaclass singleton-class))

#+(or abcl cmu ecl)
(incf *check-counter*)

(assert (eql (some-slot (class-prototype (find-class 'class2))) *check-counter*))

(defparameter *counter* 0)
(defparameter *check-counter* 0)

(deflayer layer1 ()
  ((some-slot :initform (incf *counter*) :reader some-slot)))

(incf *check-counter*)

(assert (eql (some-slot (find-layer 'layer1)) *check-counter*))

(deflayer layer1 ()
  ((some-slot :initform (incf *counter*) :reader some-slot)))

#+(or abcl cmu ecl)
(incf *check-counter*)

(assert (eql (some-slot (find-layer 'layer1)) *check-counter*))

(deflayer layer1 ()
  ((some-slot :initform (incf *counter*) :reader some-slot :reinitialize t)))

#-(or lispworks6.1 lispworks7 lispworks8)
(incf *check-counter*)

(assert (eql (some-slot (find-layer 'layer1)) *check-counter*))

(deflayer layer2 ()
  ((some-slot :initform (incf *counter*) :reader some-slot :reinitialize t)))

#-(or lispworks6.1 lispworks7 lispworks8)
(incf *check-counter*)
#+(or lispworks6.1 lispworks7 lispworks8)
(incf *check-counter* 3)

(assert (eql (some-slot (find-layer 'layer2)) *check-counter*))

(deflayer layer2 ()
  ((some-slot :initform (incf *counter*) :reader some-slot :reinitialize t)))

#-(or lispworks6.1 lispworks7 lispworks8)
(incf *check-counter*)

(assert (eql (some-slot (find-layer 'layer2)) *check-counter*))

(deflayer layer2 ()
  ((some-slot :initform (incf *counter*) :reader some-slot)))

#+(or abcl cmu ecl)
(incf *check-counter*)

(assert (eql (some-slot (find-layer 'layer2)) *check-counter*))

(deflayer layer3 ()
  ((some-slot :initform (incf *counter*) :reader some-slot :special t)))

#-(or lispworks6.1 lispworks7 lispworks8)
(incf *check-counter*)
#+(or lispworks6.1 lispworks7 lispworks8)
(incf *check-counter* 3)

(assert (eql (some-slot (find-layer 'layer3)) *check-counter*))

#-cmu
(progn
  (deflayer layer3 ()
    ((some-slot :initform (incf *counter*) :reader some-slot :special t :reinitialize t)))
  
  #-(or lispworks6.1 lispworks7 lispworks8)
  (incf *check-counter*)
  
  ;(assert (eql (some-slot (find-layer 'layer3)) *check-counter*))
  )

(dletf (((some-slot (find-layer 'layer3)) 'foo))
  (assert (eql (some-slot (find-layer 'layer3)) 'foo)))

(assert (eql (some-slot (find-layer 'layer3)) *check-counter*))

(deflayer layer4 ()
  ((some-slot :initform (incf *counter*) :reader some-slot :special t)))

#-(or lispworks6.1 lispworks7 lispworks8)
(incf *check-counter*)
#+(or lispworks6.1 lispworks7 lispworks8)
(incf *check-counter* 2)

(dletf (((some-slot (find-layer 'layer4)) 'bar))
  (assert (eql (some-slot (find-layer 'layer4)) 'bar)))

(assert (eql (some-slot (find-layer 'layer4)) *check-counter*))

(print :done)
