(in-package :contextl)

(defclass singleton-class (standard-class)
  ())

(defmethod validate-superclass
           ((class singleton-class)
            (superclass standard-class))
  t)

(defmethod make-instance ((class singleton-class) &rest initargs)
  (declare (ignore initargs))
  (error "The singleton class ~S cannot be instantiated." class))

(defvar *reinitialize-singleton-class* nil)

(defmethod reinitialize-instance :around
  ((class singleton-class) &key)
  (let ((*reinitialize-singleton-class* t))
    (call-next-method)))

(defclass singleton-direct-slot-definition (standard-direct-slot-definition)
  ((reinitializep :initarg :reinitialize :initform nil :accessor slot-definition-reinitializep)))

(defmethod direct-slot-definition-class ((class singleton-class) &key &allow-other-keys)
  (find-class 'singleton-direct-slot-definition))

(defmethod initialize-instance :around
  ((slotd singleton-direct-slot-definition)
   &rest initargs &key name (allocation :class) reinitialize)
  #+(or abcl cmu ecl lispworks6.1) (declare (ignore reinitialize))
  (restart-case
      (unless (eq allocation :class)
        (error "The allocation of the singleton class slot ~S must be :CLASS, but is defined as ~S."
               name allocation))
    (continue ()
      :report (lambda (stream) (format stream "Use allocation ~S anyway." allocation)))
    (allocation-class ()
      :report "Use allocation :CLASS instead."
      (setq allocation :class)))
  (apply #'call-next-method slotd
         :allocation allocation
         :reinitialize
         #-(or abcl cmu ecl lispworks6.1) (and reinitialize *reinitialize-singleton-class*)
         #+(or abcl cmu ecl lispworks6.1) nil
         initargs))

(defmethod reinitialize-instance :before
  ((class singleton-class) &rest initargs)
  (when (getf initargs
              #-lispworks4 :direct-default-initargs
              #+lispworks4 :default-initargs)
    (warn "Default initialization arguments do not make sense for singleton class ~S." class)))

(defmethod reinitialize-instance :after
  ((class singleton-class) &key)
  (when-let (prototype (ignore-errors (class-prototype class)))
    (loop for slot in (class-direct-slots class)
          when (slot-definition-reinitializep slot) do
          (setf (slot-definition-reinitializep slot) nil)
          (if (slot-definition-initfunction slot)
            (setf (slot-value prototype (slot-definition-name slot))
                  (funcall (slot-definition-initfunction slot)))
            (slot-makunbound prototype (slot-definition-name slot))))))

(defmethod finalize-inheritance :after ((class singleton-class))
  (let ((prototype (class-prototype class)))
    (loop for slot in (class-direct-slots class)
          when (slot-definition-reinitializep slot) do
          (setf (slot-definition-reinitializep slot) nil)
          (if (slot-definition-initfunction slot)
            (setf (slot-value prototype (slot-definition-name slot))
                  (funcall (slot-definition-initfunction slot)))
            (slot-makunbound prototype (slot-definition-name slot))))))

(declaim (inline find-singleton))

(defun find-singleton (name &optional (errorp t) environment)
  (class-prototype (find-class name errorp environment)))
