(in-package :contextl)

(defclass layered-access-class (standard-class)
  ())

(defmethod validate-superclass
           ((class layered-access-class)
            (superclass standard-class))
  t)

(defgeneric slot-definition-layeredp (slot)
  (:method ((slot slot-definition)) nil))

(defclass layered-direct-slot-definition (standard-direct-slot-definition)
  ((layeredp :initarg :layered
             :initform nil
             :reader slot-definition-layeredp)
   (layered-readers :initarg :layered-readers
                    :initform ()
                    :reader slot-definition-layered-readers)
   (layered-writers :initarg :layered-writers
                    :initform ()
                    :reader slot-definition-layered-writers)
   (layered-accessor-methods :initform ()
                             :accessor layered-accessor-methods)))

(defclass layered-effective-slot-definition (standard-effective-slot-definition)
  ())

(defmethod slot-definition-layeredp ((slot layered-effective-slot-definition))
  t)

(defmethod direct-slot-definition-class
           ((class layered-access-class) &key &allow-other-keys)
  (find-class 'layered-direct-slot-definition))

(defvar *layered-effective-slot-definition-class*)

(defmethod effective-slot-definition-class
           ((class layered-access-class) &key &allow-other-keys)
  (if *layered-effective-slot-definition-class*
    *layered-effective-slot-definition-class*
    (call-next-method)))

(defmethod compute-effective-slot-definition
           ((class layered-access-class) name direct-slot-definitions)
  (declare (ignore name))
  (let ((*layered-effective-slot-definition-class*
         (when (some #'slot-definition-layeredp direct-slot-definitions)
           (find-class 'layered-effective-slot-definition))))
    (call-next-method)))

(define-layered-function slot-value-using-layer (class object slot reader)
  (:method (class object slot reader)
   (declare (ignore class object slot))
   (funcall reader)))

(defmethod slot-value-using-class :around
  ((class layered-access-class) object (slot layered-effective-slot-definition))
  (flet ((reader () (call-next-method)))
    (slot-value-using-layer class object slot #'reader)))

(define-layered-function (setf slot-value-using-layer) (new-value class object slot writer)
  (:method (new-value class object slot writer)
   (declare (ignore class object slot))
   (funcall writer new-value)))

(defmethod (setf slot-value-using-class) :around
  (new-value (class layered-access-class) object (slot layered-effective-slot-definition))
  (flet ((writer (new-value) (call-next-method new-value class object slot)))
    (setf (slot-value-using-layer class object slot #'writer)
          new-value)))

(define-layered-function slot-boundp-using-layer (class object slot reader)
  (:method (class object slot reader)
   (declare (ignore class object slot))
   (funcall reader)))

(defmethod slot-boundp-using-class :around
  ((class layered-access-class) object (slot layered-effective-slot-definition))
  (flet ((reader () (call-next-method)))
    (slot-boundp-using-layer class object slot #'reader)))

(define-layered-function slot-makunbound-using-layer (class object slot writer)
  (:method (class object slot writer)
   (declare (ignore class object slot))
   (funcall writer)))

(defmethod slot-makunbound-using-class :around
  ((class layered-access-class) object (slot layered-effective-slot-definition))
  (flet ((writer () (call-next-method)))
    (slot-makunbound-using-layer class object slot #'writer)))

(defgeneric process-layered-access-slot-specification (slot-spec)
  (:method ((slot-spec symbol)) slot-spec)
  (:method ((slot-spec cons))
   (let ((plist (cdr slot-spec)))
     (if (get-properties plist '(:layered-reader :layered-writer :layered-accessor))
       (loop for (key value) on plist by #'cddr
             if (eq key :layered-reader)
             collect value into layered-readers
             else if (eq key :layered-writer)
             collect value into layered-writers
             else if (eq key :layered-accessor)
             collect value into layered-readers
             and collect `(setf ,value) into layered-writers
             else nconc (list key value) into other-initargs
             finally (return (list* (car slot-spec)
                                    :layered-readers layered-readers
                                    :layered-writers layered-writers
                                    other-initargs)))
       slot-spec))))

(defgeneric add-layered-accessors (class)
  (:method ((class layered-access-class))
   (loop with reader-specializers = (list class)
         with writer-specializers = (list (find-class 't) class)
         for slot in (class-direct-slots class)
         for slot-name = (slot-definition-name slot)
         for layer = (find-layer-class (slot-definition-layer slot)) do
         (loop for layered-reader in (slot-definition-layered-readers slot)
               for gf = (ensure-layered-function layered-reader :lambda-list '(object))
               for method = (ensure-layered-method
                             layered-reader
                             `(lambda (object)
                                (declare (optimize (speed 3) (debug 0) (safety 0)
                                                   (compilation-speed 0)))
                                (slot-value object ',slot-name))
                             :in-layer layer
                             :specializers reader-specializers)
               do (push (cons gf method) (layered-accessor-methods slot)))
         (loop for layered-writer in (slot-definition-layered-writers slot)
               for gf = (ensure-layered-function layered-writer
                                                 :lambda-list '(new-value object)
                                                 :argument-precedence-order '(object new-value))
               for method = (ensure-layered-method
                             layered-writer
                             `(lambda (new-value object)
                                (declare (optimize (speed 3) (debug 0) (safety 0)
                                                   (compilation-speed 0)))
                                (setf (slot-value object ',slot-name)
                                      new-value))
                             :in-layer layer
                             :specializers writer-specializers)
               do (push (cons gf method) (layered-accessor-methods slot))))))

(defgeneric remove-layered-accessors (class)
  (:method ((class layered-access-class))
   (loop for slot in (class-direct-slots class)
         do (loop for method in (layered-accessor-methods slot)
                  do (remove-method (car method) (cdr method))))))

(defmethod initialize-instance :after
  ((class layered-access-class) &key)
  (add-layered-accessors class))

(defmethod reinitialize-instance :around
  ((class layered-access-class)
   &key (direct-slots () direct-slots-p))
  (declare (ignore direct-slots))
  (if direct-slots-p
    (progn
      (remove-layered-accessors class)
      (call-next-method)
      (add-layered-accessors class)
      class)
    (call-next-method)))
