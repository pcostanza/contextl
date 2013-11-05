(in-package :contextl)

(defclass special-layered-access-class
          (layered-access-class
           #-cx-disable-special-class-in-layered-classes
           special-class
           standard-class-in-layer)
  ())

(defclass special-layered-direct-slot-definition
          (layered-direct-slot-definition
           #-cx-disable-special-class-in-layered-classes
           special-direct-slot-definition
           standard-direct-slot-definition-in-layer)
  ())

(defclass special-effective-slot-definition-in-layers
          (#-cx-disable-special-class-in-layered-classes
           special-effective-slot-definition
           standard-effective-slot-definition-in-layers)
  ())

(defclass layered-effective-slot-definition-in-layers
          (layered-effective-slot-definition
           standard-effective-slot-definition-in-layers)
  ())

(defclass special-layered-effective-slot-definition
          (layered-effective-slot-definition-in-layers
           special-effective-slot-definition-in-layers)
  ())

(defmethod direct-slot-definition-class
           ((class special-layered-access-class) &key &allow-other-keys)
  (find-class 'special-layered-direct-slot-definition))

(defvar *special-layered-effective-slot-definition-class*)

(defmethod effective-slot-definition-class
           ((class special-layered-access-class) &key &allow-other-keys)
  (if *special-layered-effective-slot-definition-class*
    *special-layered-effective-slot-definition-class*
    (call-next-method)))

(defmethod compute-effective-slot-definition
           ((class special-layered-access-class) name direct-slot-definitions)
  (declare (ignore name))
  (let ((*special-layered-effective-slot-definition-class*
         (if (some #'slot-definition-layeredp direct-slot-definitions)
           (if (some #'slot-definition-specialp direct-slot-definitions)
             (find-class 'special-layered-effective-slot-definition)
             (find-class 'layered-effective-slot-definition-in-layers))
           (when (some #'slot-definition-specialp direct-slot-definitions)
             (find-class 'special-effective-slot-definition-in-layers)))))
    (call-next-method)))

(defclass layered-class (partial-class special-layered-access-class)
  ()
  (:default-initargs :defining-metaclass 'special-layered-access-class))

#+sbcl
(defmethod shared-initialize :after
  ((class layered-class) slot-names &key defining-metaclass)
  (declare (ignore slot-names defining-metaclass)))

(defmacro define-layered-class (&whole form name &body options)
  (let* ((layer (if (member (car options) '(:in-layer :in) :test #'eq)
                  (cadr options)
                  t))
         (options (cond ((member (car options) '(:in-layer :in) :test #'eq)
                         (cddr options))
                        ((not (listp (car options)))
                         (error "Illegal option ~S in ~S."
                                (car options) form))
                        (t options)))
         (form `(defclass ,name ,(car options)
                  ,(mapcar #'process-layered-access-slot-specification (cadr options))
                  ,@(cddr options)
                  ,@(unless (assoc :metaclass options)
                      '((:metaclass layered-class)))
                  (:in-layer . ,layer))))
    #+allegro (if (eq (find-layer layer nil) 't) form
                `(excl:without-redefinition-warnings ,form))
    #+lispworks (if (eq (find-layer layer nil) 't) form
                  `(let ((dspec:*redefinition-action* :quiet)) ,form))
    #-(or allegro lispworks) form))
