(in-package :contextl)

(defgeneric class-layer (class)
  (:method ((class class)) 't))

(defclass standard-class-in-layer (standard-class)
  ((layer :initarg :in-layer
          :initarg :in
          :initform 't
          :reader class-layer)))

(defmethod validate-superclass
           ((class standard-class-in-layer)
            (superclass standard-class))
  t)

(defgeneric slot-definition-layer (slot)
  (:method ((slot direct-slot-definition)) 't))

(defclass standard-direct-slot-definition-in-layer (standard-direct-slot-definition)
  ((layer :initarg :in-layer
          :initarg :in
          :initform 't
          :reader slot-definition-layer)))

(defmethod direct-slot-definition-class
           ((class standard-class-in-layer) &key &allow-other-keys)
  (find-class 'standard-direct-slot-definition-in-layer))

(defgeneric slot-definition-layers (slot)
  (:method ((slot effective-slot-definition)) '(t)))

(defclass standard-effective-slot-definition-in-layers (standard-effective-slot-definition)
  ((layers :initform '(t)
           :reader slot-definition-layers)))

(defmethod effective-slot-definition-class
           ((class standard-class-in-layer) &key &allow-other-keys)
  (find-class 'standard-effective-slot-definition-in-layers))

(defmethod compute-effective-slot-definition
           ((class standard-class-in-layer) name direct-slot-definitions)
  (declare (ignore name))
  (let ((slot (call-next-method)))
    (setf (slot-value slot 'layers)
          (loop for direct-slot in direct-slot-definitions
                for layer = (slot-definition-layer direct-slot)
                for layer-name = (or (layer-name layer) layer)
                for layers = (list layer-name) then (adjoin layer-name layers :test #'eq)
                finally (return layers)))
    slot))

(defmethod initialize-instance :around
  ((class standard-class-in-layer) &rest initargs
   &key (direct-slots ()) (in-layer 't))
  (apply #'call-next-method class
         :direct-slots
         (loop for direct-slot in direct-slots
               if (get-properties direct-slot '(:in-layer :in)) collect direct-slot
               else collect (list* :in-layer in-layer direct-slot))
         initargs))

(defmethod reinitialize-instance :around
  ((class standard-class-in-layer) &rest initargs
   &key (direct-slots () direct-slots-p) (in-layer 't))
  (if direct-slots-p
    (apply #'call-next-method class
           :direct-slots
           (loop for direct-slot in direct-slots
                 if (get-properties direct-slot '(:in-layer :in)) collect direct-slot
                 else collect (list* :in-layer in-layer direct-slot))
           initargs)
    (call-next-method)))
