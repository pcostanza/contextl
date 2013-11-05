(in-package :contextl)

(defclass partial-object (standard-object)
  ()
  (:default-initargs :allow-other-keys t))

(defclass partial-class (standard-class)
  ((defining-classes :initarg defining-classes
                     :reader partial-class-defining-classes)
   (defining-metaclass :initarg :defining-metaclass
                       :reader partial-class-defining-metaclass))
  (:default-initargs :defining-metaclass 'standard-class))

(defmethod validate-superclass
           ((class partial-class)
            (superclass standard-class))
  t)

(defmethod validate-superclass
           ((class standard-class)
            (superclass partial-class))
  t)

(defgeneric partial-class-base-initargs (class)
  (:method-combination append)
  (:method append ((class partial-class))
   '(:name :defining-metaclass)))

(defmethod initialize-instance :around
  ((class partial-class) &rest initargs
   &key name defining-metaclass
   (in-layer 't in-layer-p) (in 't in-p))
  (assert (not (and in-layer-p in-p)))
  (loop for (key value) on initargs by #'cddr
        if (member key (partial-class-base-initargs class))
        nconc (list key value) into base-initargs
        else nconc (list key value) into partial-initargs
        finally (return
                 (let* ((in-layer (if in-layer-p in-layer in))
                        (in-layer-name (or (layer-name in-layer) (find-layer in-layer)))
                        (direct-superclasses (list (find-class 'partial-object)))
                        (defining-classes ()))
                   (let ((defined-class
                          (apply #'make-instance defining-metaclass partial-initargs)))
                     (push defined-class direct-superclasses)
                     (setf (getf defining-classes in-layer-name) defined-class))
                   (unless (eq in-layer-name 't)
                     (let ((defined-class (make-instance defining-metaclass)))
                       (push defined-class direct-superclasses)
                       (setf (getf defining-classes 't) defined-class)))
                   (apply #'call-next-method class
                          :direct-superclasses direct-superclasses
                          'defining-classes defining-classes
                          base-initargs)))))

(defmethod reinitialize-instance :around
  ((class partial-class) &rest initargs
   &key (in-layer 't in-layer-p) (in 't in-p)
   (defining-metaclass (partial-class-defining-metaclass class)))
  (assert (not (and in-layer-p in-p)))
  (loop for (key value) on initargs by #'cddr
        if (member key (partial-class-base-initargs class))
        nconc (list key value) into base-initargs
        else nconc (list key value) into partial-initargs
        finally (return
                 (let* ((in-layer (if in-layer-p in-layer in))
                        (in-layer-name (or (layer-name in-layer) (find-layer in-layer))))
                   (let ((defined-class (getf (partial-class-defining-classes class) in-layer-name)))
                     (if defined-class
                       (progn
                         (apply #'reinitialize-instance defined-class partial-initargs)
                         (apply #'call-next-method class base-initargs))
                       (let ((defined-class
                              (apply #'make-instance defining-metaclass partial-initargs)))
                         (apply #'call-next-method class
                                :direct-superclasses
                                (append (remove (find-class 'partial-object)
                                                (class-direct-superclasses class))
                                        (list defined-class)
                                        (list (find-class 'partial-object)))
                                'defining-classes
                                (list* in-layer-name defined-class
                                       (partial-class-defining-classes class))
                                base-initargs))))))))
