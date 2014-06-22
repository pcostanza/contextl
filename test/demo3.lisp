(asdf:oos 'asdf:load-op :contextl :force t)

(in-package :contextl-user)

(define-layered-class person ()
  ((name :initarg :name
         :layered-accessor person-name)))

(define-layered-function display-object (object))

(define-layered-method display-object ((object person))
  (print (list 'person :name (person-name object))))

(defparameter *pascal*
  (make-instance 'person :name 'pascal))

(assert (equal (display-object *pascal*)
               '(person :name pascal)))

(deflayer employment-layer)

(define-layered-class employer
  :in employment-layer ()
  ((name :initarg :name
         :layered-accessor employer-name)))

(define-layered-method display-object
  :in employment-layer ((object employer))
  (print (list 'employer :name (employer-name object))))

(defparameter *vub*
  (make-instance 'employer :name 'vub))

(assert (equal (with-active-layers (employment-layer)
                 (display-object *vub*))
               '(employer :name vub)))

(define-layered-class person
  :in employment-layer ()
  ((employer :initarg :employer
             :layered-accessor person-employer)))

(define-layered-method display-object
  :in employment-layer :around ((object person))
  (append (call-next-method)
          (print (list :employer
                       (display-object (person-employer object))))))

(with-active-layers (employment-layer)
  (setf (person-employer *pascal*) *vub*))

(assert (equal (display-object *pascal*)
               '(person :name pascal)))

(assert (equal (with-active-layers (employment-layer)
                 (display-object *pascal*))
               '(person :name pascal
                        :employer (employer :name vub))))

(deflayer info-layer)

(define-layered-class info-mixin
  :in info-layer ()
  ((city :initarg :city
         :layered-accessor city)))

(define-layered-method display-object
  :in info-layer :around ((object info-mixin))
  (append (call-next-method)
          (print (list :city (city object)))))

(define-layered-class person
  :in info-layer (info-mixin)
  ())

(define-layered-class employer
  :in info-layer (info-mixin)
  ())

(defparameter *docomo*
  (make-instance 'employer
                 :name 'docomo
                 :city 'munich))

(defparameter *robert*
  (make-instance 'person
                 :name 'robert
                 :employer *docomo*
                 :city 'ilmenau))

(assert (equal (display-object *robert*)
               '(person :name robert)))

(assert (equal (with-active-layers (employment-layer)
                 (display-object *robert*))
               '(person :name robert :employer (employer :name docomo))))

(assert (equal (with-active-layers (employment-layer info-layer)
                 (print (display-object *robert*)))
               '(person :name robert
                        :city ilmenau
                        :employer (employer :name docomo
                                            :city munich))))

(assert (equal (with-active-layers (info-layer employment-layer)
                 (display-object *robert*))
               '(person :name robert
                        :city ilmenau
                        :employer (employer :name docomo
                                            :city munich))))

(assert (equal (with-active-layers (info-layer employment-layer)
                 (with-inactive-layers (info-layer)
                   (display-object *robert*)))
               (with-active-layers (employment-layer)
                 (display-object *robert*))))

(assert (equal (with-active-layers (info-layer employment-layer info-layer)
                 (display-object *robert*))
               (with-active-layers (employment-layer info-layer)
                 (display-object *robert*))))

(deflayer generic-display-layer)

(define-layered-class displayed-slots-mixin
  :in generic-display-layer ()
  ((displayed-slots :special t
                    :initform '()
                    :accessor displayed-slots)))

(define-layered-class person
  :in generic-display-layer
  (displayed-slots-mixin)
  ())

(define-layered-class employer
  :in generic-display-layer
  (displayed-slots-mixin)
  ())

(defgeneric generic-display (object))

(defmethod generic-display (object) object)

(defmethod generic-display ((object displayed-slots-mixin))
  (let ((slots (displayed-slots object)))
    (if slots
        (loop for slot in slots
              collect slot
              collect (generic-display (slot-value object slot)))
      (format t "No slots for display selected.~%"))))

(assert (equal (with-active-layers (generic-display-layer)
                 (dletf (((displayed-slots *robert*) '(name employer))
                         ((displayed-slots *docomo*) '(name city)))
                   (generic-display *robert*)))
               '(name robert employer (name docomo city munich))))

(deflayer slot-access-layer)

(define-layered-method slot-value-using-layer
  :in slot-access-layer (class (object person) slot reader)
  (declare (ignorable class slot reader))
  (list* (call-next-method)
         (list :slot-access 'successful)))

(define-layered-class person
  :in slot-access-layer ()
  ((name :layered t)))

(assert (equal (with-active-layers (generic-display-layer slot-access-layer)
                 (dletf (((displayed-slots *robert*) '(name employer))
                         ((displayed-slots *docomo*) '(name city)))
                   (print (generic-display *robert*))))
               '(name (robert :slot-access successful)
                      employer (name docomo city munich))))

(define-layered-function test ())

(define-layered-method test :in t ()
  (list 'root-layer))

(define-layered-method test :in info-layer ()
  (list* 'info-layer (call-next-method)))

(define-layered-method test :in employment-layer ()
  (list* 'employment-layer (call-next-method)))

(assert (equal (test) '(root-layer)))

(assert (equal (with-active-layers (info-layer)
                 (test))
               '(info-layer root-layer)))

(assert (equal (with-active-layers (info-layer employment-layer)
                 (test))
               '(employment-layer info-layer root-layer)))

(assert (equal (with-active-layers (info-layer employment-layer info-layer)
                 (test))
               '(employment-layer info-layer root-layer)))

(assert (equal (with-active-layers (info-layer employment-layer)
                 (with-inactive-layers (info-layer)
                   (test)))
               '(employment-layer root-layer)))

(assert (equal (with-active-layers (employment-layer employment-layer)
                 (test))
               '(employment-layer root-layer)))

(assert (equal (with-active-layers (info-layer employment-layer)
                 (with-inactive-layers (employment-layer)
                   (test)))
               '(info-layer root-layer)))

(multiple-value-bind
    (required-parameters lambda-list-keyword)
    (contextl::parse-gf-lambda-list '(a b c &rest r))
  (assert (and (equal required-parameters '(a b c))
               lambda-list-keyword)))

(multiple-value-bind
    (required-parameters lambda-list-keyword)
    (contextl::parse-gf-lambda-list '(&key r))
  (assert (and (null required-parameters)
               lambda-list-keyword)))

(multiple-value-bind
    (required-parameters lambda-list-keyword)
    (contextl::parse-gf-lambda-list '(a b c))
  (assert (and (equal required-parameters '(a b c))
               (not lambda-list-keyword))))

(multiple-value-bind
    (required-parameters lambda-list-keyword)
    (contextl::parse-gf-lambda-list '())
  (assert (and (null required-parameters)
               (not lambda-list-keyword))))

(print :done)
