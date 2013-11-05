(in-package :contextl)

(defclass root-specializer () ()
  (:metaclass standard-layer-class)
  (original-name . t))
(ensure-finalized (find-class 'root-specializer))

#-allegro
(declaim (type layer-context *root-context* *active-context*))
#+allegro
(eval-when (:load-toplevel :execute)
  (proclaim '(type layer-context *root-context* *active-context*)))

(defvar *root-context*
  (make-layer-context
   :prototype (class-prototype (find-class 'root-specializer))
   :specializer (find-class 'root-specializer)))

(defvar *active-context* *root-context*)

(declaim (inline current-layer-context))
(defun current-layer-context () *active-context*)

(declaim (inline (setf current-layer-context)))
(defun (setf current-layer-context) (new-layer-context)
  (setf *active-context* new-layer-context))

(defun layer-active-p (layer &optional (context *active-context*))
  (subtypep (layer-context-specializer context)
            (find-layer-class layer)))

(defun active-layers (&optional (context *active-context*))
  (loop with result = '()
        for context-specializer = (layer-context-specializer context)
        then (second (class-direct-superclasses context-specializer))
        until (eq context-specializer (load-time-value (find-class 'root-specializer)))
        do (push (find-layer (first (class-direct-superclasses context-specializer))) result)
        finally (return (nreverse (cons 't result)))))

(define-layered-function adjoin-layer-using-class (layer-class active-context)
  (:method ((layer-class (eql (find-class 't))) active-context)
   (values active-context t))
  (:method ((layer-class standard-layer-class) active-context)
   (let ((active-context-specializer (layer-context-specializer active-context)))
     (values
      (if (subtypep active-context-specializer layer-class)
        active-context
        (let ((new-specializer
               (as-atomic-operation
                 (ensure-finalized
                  (make-instance 'standard-layer-class
                                 :direct-superclasses
                                 (list layer-class active-context-specializer))))))
          (make-layer-context
           :prototype (class-prototype new-specializer)
           :specializer new-specializer)))
      t))))

(defun safe-adjoin-layer (layer active-context)
  (with-lock ((layer-context-lock active-context))
    (or #-cx-threads (getf (layer-context-children/ensure-active active-context) layer)
        #-cx-threads (getf (layer-context-children/ensure-active active-context) (layer-name layer))
        (multiple-value-bind
            (new-layer-context cacheablep)
            (adjoin-layer-using-class (find-layer-class layer) active-context)
          (when cacheablep
            (setf (layer-context-children/ensure-active active-context)
                  (list* (or (layer-name layer) layer) new-layer-context
                         (layer-context-children/ensure-active active-context))))
          new-layer-context))))

(declaim (inline adjoin-layer))

(defun adjoin-layer (layer active-context)
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (or (getf (layer-context-children/ensure-active active-context) layer)
      (getf (layer-context-children/ensure-active active-context) (layer-name layer))
      (safe-adjoin-layer layer active-context)))

(defun ensure-active-layer (layer-name)
  (setf *active-context*
        (locally
          (declare (optimize (speed 3) (debug 0) (safety 0)
                             (compilation-speed 0)))
          (adjoin-layer layer-name *active-context*)))
  (values))

(define-layered-function remove-layer-using-class (layer-class active-context)
  (:method ((layer-class (eql (find-class 't))) active-context)
   (declare (ignore active-context))
   (error "The layer T may never be removed."))
  (:method ((layer-class standard-layer-class) active-context)
   (values
    (loop for context-specializer = (layer-context-specializer active-context)
          then (second (class-direct-superclasses context-specializer))
          for active-layers = (list (first (class-direct-superclasses context-specializer)))
          then (cons (first (class-direct-superclasses context-specializer)) active-layers)
          until (eq context-specializer (load-time-value (find-class 'root-specializer)))
          finally
          (return (loop for new-layer-context = *root-context*
                        then (if (subtypep active-layer layer-class)
                               new-layer-context
                               (adjoin-layer active-layer new-layer-context))
                        for active-layer in (cdr active-layers)
                        finally (return new-layer-context))))
    t)))

(defun safe-remove-layer (layer active-context)
  (with-lock ((layer-context-lock active-context))
    (or #-cx-threads (getf (layer-context-children/ensure-inactive active-context) layer)
        #-cx-threads (getf (layer-context-children/ensure-inactive active-context) (layer-name layer))
        (multiple-value-bind
            (new-layer-context cacheablep)
            (remove-layer-using-class (find-layer-class layer) active-context)
          (when cacheablep
            (setf (layer-context-children/ensure-inactive active-context)
                  (list* (or (layer-name layer) layer) new-layer-context
                         (layer-context-children/ensure-inactive active-context))))
          new-layer-context))))

(declaim (inline remove-layer))

(defun remove-layer (layer active-context)
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (or (getf (layer-context-children/ensure-inactive active-context) layer)
      (getf (layer-context-children/ensure-inactive active-context) (layer-name layer))
      (safe-remove-layer layer active-context)))

(defun ensure-inactive-layer (layer-name)
  (setf *active-context*
        (locally
          (declare (optimize (speed 3) (debug 0) (safety 0)
                             (compilation-speed 0)))
          (remove-layer layer-name *active-context*)))
  (values))

(defmacro %with-active-layers ((&rest layer-names) &body body)
  `(let ((*active-context*
          (locally
            (declare (optimize (speed 3) (debug 0) (safety 0)
                               (compilation-speed 0)))
            ,(loop for form = '*active-context*
                   then `(adjoin-layer ',layer-name ,form)
                   for layer-name in layer-names
                   finally (return form)))))
     ,@body))

(defmacro with-active-layers ((&rest layer-names) &body body)
  (cond ((null layer-names) `(progn ,@body))
        ((every #'atom layer-names)
         (with-unique-names (proceed)
           `(dynamic-wind :proceed ,proceed
              (%with-active-layers ,layer-names (,proceed ,@body)))))
        (t `(with-active-layers ,(loop for layer-spec in layer-names
                                       if (atom layer-spec)
                                       collect layer-spec
                                       else collect (car layer-spec))
              (with-special-initargs
                  ,(loop for layer-spec in layer-names
                         when (consp layer-spec)
                         collect `((find-layer ',(car layer-spec)) ,@(cdr layer-spec)))
                ,@body)))))

(defmacro with-active-layers* ((&rest layer-names) &body body)
  (cond ((null layer-names) `(progn ,@body))
        ((every #'atom layer-names)
         (with-unique-names (proceed)
           `(dynamic-wind :proceed ,proceed
              (%with-active-layers ,layer-names (,proceed ,@body)))))
        (t `(with-active-layers ,(loop for layer-spec in layer-names
                                       if (atom layer-spec)
                                       collect layer-spec
                                       else collect (car layer-spec))
              (with-special-initargs*
                  ,(loop for layer-spec in layer-names
                         when (consp layer-spec)
                         collect `((find-layer ',(car layer-spec)) ,@(cdr layer-spec)))
                ,@body)))))

(defmacro %with-inactive-layers ((&rest layer-names) &body body)
  `(let ((*active-context*
          (locally
            (declare (optimize (speed 3) (debug 0) (safety 0)
                               (compilation-speed 0)))
            ,(loop for form = '*active-context*
                   then `(remove-layer ',layer-name ,form)
                   for layer-name in layer-names
                   finally (return form)))))
     ,@body))

(defmacro with-inactive-layers ((&rest layer-names) &body body)
  (if layer-names
    (with-unique-names (proceed)
      `(dynamic-wind :proceed ,proceed
         (%with-inactive-layers ,layer-names (,proceed ,@body))))
    `(progn ,@body)))

(defun funcall-with-layer-context (layer-context function &rest args)
  (dynamic-wind
    (let ((*active-context* layer-context))
      (proceed (apply function args)))))

(defun apply-with-layer-context (layer-context function &rest args)
  (dynamic-wind
    (let ((*active-context* layer-context))
      (proceed (apply #'apply function args)))))
