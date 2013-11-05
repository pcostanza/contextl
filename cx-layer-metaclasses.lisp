(in-package :contextl)

(defclass standard-layer-object (special-object)
  ())

(defgeneric layer-name (layer)
  (:method ((layer symbol)) layer)
  (:method ((layer (eql (find-class 't)))) 't)
  (:method ((layer standard-layer-object)) (layer-name (class-of layer))))

(defclass standard-layer-class (special-class singleton-class)
  ((layer-name :initarg original-name
               :initform nil
               :reader layer-name))
  (:default-initargs :direct-superclasses (list (find-class 'standard-layer-object))))

(defmethod validate-superclass
           ((class standard-layer-class)
            (superclass standard-class))
  t)

(defmethod print-object ((object standard-layer-object) stream)
  (print-unreadable-object (object stream :type nil :identity t)
    (format stream "LAYER ~A" (layer-name object))))

(defmethod print-object ((object standard-layer-class) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (layer-name object) stream)))

(defmethod initialize-instance :around
  ((class standard-layer-class) &rest initargs &key direct-superclasses)
  (if (loop for direct-superclass in direct-superclasses
            thereis (subclassp direct-superclass 'standard-layer-object))
    (call-next-method)
    (apply #'call-next-method
           class
           :direct-superclasses
           (append direct-superclasses
                   (list (find-class 'standard-layer-object)))
           initargs)))

(defmethod reinitialize-instance :around
  ((class standard-layer-class) &rest initargs &key (direct-superclasses '() direct-superclasses-p))
  (if (or (not direct-superclasses-p)
          (loop for direct-superclass in direct-superclasses
                thereis (subclassp direct-superclass 'standard-layer-object)))
    (call-next-method)
    (apply #'call-next-method
           class
           :direct-superclasses
           (append direct-superclasses
                   (list (find-class 'standard-layer-object)))
           initargs)))

(defclass layer-direct-slot-definition (singleton-direct-slot-definition
                                        special-direct-slot-definition)
  ())

(defmethod direct-slot-definition-class ((class standard-layer-class) &key)
  (find-class 'layer-direct-slot-definition))

(defmacro deflayer (name &optional superlayers &body options)
  (destructuring-bind (&optional slots &rest options) options
    `(defclass ,(defining-layer name) ,(mapcar #'defining-layer superlayers)
       ,(if slots slots '())
       ,@options
       ,@(unless (assoc :metaclass options)
           '((:metaclass standard-layer-class)))
       (original-name . ,name))))

(defun ensure-layer (layer-name
                     &rest initargs
                     &key (metaclass 'standard-layer-class)
                     &allow-other-keys)
  (apply #'ensure-class
         (defining-layer layer-name)
         :metaclass metaclass
         'original-name layer-name
         initargs))

(defgeneric find-layer-class (layer &optional errorp environment)
  (:method ((layer (eql 't)) &optional errorp environment)
   (declare (ignore errorp environment))
   (load-time-value (find-class 't)))
  (:method ((layer (eql (find-class 't))) &optional errorp environment)
   (declare (ignore errorp environment))
   (load-time-value (find-class 't)))
  (:method ((layer symbol) &optional (errorp t) environment)
   (or (find-class (defining-layer layer) nil environment)
       (when errorp
         (cerror "Retry finding the layer."
                 "There is no layer named ~S." layer)
         (find-layer-class layer errorp environment))))
  (:method ((layer standard-layer-object) &optional errorp environment)
   (declare (ignore errorp environment))
   (class-of layer))
  (:method ((layer standard-layer-class) &optional errorp environment)
   (declare (ignore errorp environment))
   layer))

(defgeneric find-layer (layer &optional errorp environment)
  (:method ((layer (eql 't)) &optional errorp environment)
   (declare (ignore errorp environment))
   't)
  (:method ((layer (eql (find-class 't))) &optional errorp environment)
   (declare (ignore errorp environment))
   't)
  (:method ((layer symbol) &optional (errorp t) environment)
   (let ((layer-class (find-layer-class layer errorp environment)))
     (when layer-class
       #-lispworks (ensure-finalized layer-class)
       (class-prototype layer-class))))
  (:method ((layer standard-layer-object) &optional errorp environment)
   (declare (ignore errorp environment))
   layer)
  (:method ((layer standard-layer-class) &optional errorp environment)
   (declare (ignore errorp environment))
   #-lispworks (ensure-finalized layer)
   (class-prototype layer)))

(defgeneric layer-makunbound (layer)
  (:method ((layer symbol))
   (let* ((defining-layer (defining-layer layer))
          (class (find-class defining-layer)))
     (setf (find-class defining-layer) nil
           (class-name class) nil)))
  (:method ((layer standard-layer-object))
   (let* ((class-name (class-name (class-of layer)))
          (class (find-class class-name)))
     (setf (find-class class-name) nil
           (class-name class) nil)))
  (:method ((layer standard-layer-class))
   (let* ((class-name (class-name layer))
          (class (find-class class-name)))
     (setf (find-class class-name) nil
           (class-name class) nil))))

(defstruct layer-context
  (prototype (error "No layer-context-prototype specified.")
             :type standard-object
             :read-only t)
  (specializer (error "No layer-context-specializer specified.")
               :type standard-layer-class
               :read-only t)
  (children/ensure-active '() :type list)
  (children/ensure-inactive '() :type list)
  (lock (make-lock :name "layer context") :read-only t))
