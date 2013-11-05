(in-package :contextl)

(defun parse-method-body (form body)
  (let* ((in-layerp (member (car body) '(:in-layer :in) :test #'eq))
         (layer-spec (if in-layerp (cadr body) 't)))
    (when (consp layer-spec)
      (unless (null (cddr layer-spec))
        (error "Incorrect :in-layer specification in ~S." form)))
    (loop with layer = (if (atom layer-spec)
                         layer-spec
                         (cadr layer-spec))
          with layer-arg = (if (atom layer-spec)
                             (gensym "LAYER-ARG-")
                             (car layer-spec))
          for tail = (if in-layerp (cddr body) body) then (cdr tail)
          until (listp (car tail))
          collect (car tail) into qualifiers
          finally
          (loop for qualifier in qualifiers
                when (member qualifier '(:in-layer :in) :test #'eq)
                do (error "Incorrect occurrence of ~S in ~S. Must occur before qualifiers." qualifier form))
          (return (values layer-arg layer qualifiers (car tail) (cdr tail))))))

(defun prepare-layer (layer)
  (if (symbolp layer)
    (defining-layer layer)
    layer))

(defun prepare-layered-method-body (name form layer-arg body)
  (loop for tail = body then (cdr tail)
        for (first . rest) = tail
        while tail
        while (or (and rest (stringp first))
                  (and (consp first) (eq (car first) 'declare)))
        count (stringp first) into nof-seen-strings
        collect first into declarations
        finally
        (when (> nof-seen-strings 1)
          (warn "Too many documentation strings in ~S." form))
        (return `(,@declarations
                  (block ,(plain-function-name name)
                    (flet ((call-next-layered-method (&rest args)
                             (if args
                               (apply #'call-next-method ,layer-arg args)
                               (call-next-method))))
                      (declare (inline call-next-layered-method)
                               (ignorable (function call-next-layered-method)))
                      ,@tail))))))

(defun parse-gf-lambda-list (lambda-list)
  (loop for entry in lambda-list
        for lambda-list-keyword = (member entry lambda-list-keywords)
        until lambda-list-keyword
        collect entry into required-parameters
        finally (return (values required-parameters lambda-list-keyword))))

(defclass layered-function (standard-generic-function) ()
  (:metaclass funcallable-standard-class)
  (:default-initargs :method-class (find-class 'layered-method)))

(defmethod print-object ((object layered-function) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (lf-caller-name (generic-function-name object)) stream)))

(defun layered-function-definer (name)
  (fdefinition (lf-definer-name name)))

(defgeneric layered-function-argument-precedence-order (function)
  (:method ((function layered-function)) (butlast (generic-function-argument-precedence-order function))))

(defgeneric layered-function-lambda-list (function)
  (:method ((function layered-function)) (rest (generic-function-lambda-list function))))

(defun lfmakunbound (name)
  (fmakunbound (lf-definer-name name))
  (fmakunbound name))

(defclass layered-method (standard-method) ())

(defgeneric layered-method-lambda-list (method)
  (:method ((method layered-method)) (rest (method-lambda-list method))))

(defgeneric layered-method-specializers (method)
  (:method ((method layered-method)) (rest (method-specializers method))))

(defmacro define-layered-function (name (&rest args) &body options)
  (let ((definer (lf-definer-name name))
        (documentation (assoc :documentation options)))
    (with-unique-names (layer-arg rest-arg)
      `(progn
         (defgeneric ,definer (,layer-arg ,@args)
           ,@(unless (member :generic-function-class options :key #'car)
               '((:generic-function-class layered-function)))
           (:argument-precedence-order 
            ,@(let ((argument-precedence-order (assoc :argument-precedence-order options)))
                (if argument-precedence-order
                  (cdr argument-precedence-order)
                  (required-args args)))
            ,layer-arg)
           ,@(loop for option in (remove :argument-precedence-order options :key #'car)
                   if (eq (car option) :method)
                   collect (multiple-value-bind
                               (layer-arg layer qualifiers args method-body)
                               (parse-method-body option (cdr option))
                             `(:method ,@qualifiers ((,layer-arg ,(prepare-layer layer)) ,@args)
                               ,@(prepare-layered-method-body name option layer-arg method-body)))
                   else if (not (eq (car option) :documentation)) collect option))
         (declaim (inline ,name))
         ,(multiple-value-bind
              (required-parameters lambda-list-keyword)
              (parse-gf-lambda-list args)
            (if lambda-list-keyword
              `(defun ,name (,@required-parameters &rest ,rest-arg)
                 (declare (optimize (speed 3) (debug 0) (safety 0)
                                    (compilation-speed 0)))
                 ,@(when documentation (list (cadr documentation)))
                 (apply #',definer (layer-context-prototype *active-context*) ,@required-parameters ,rest-arg))
              `(defun ,name (,@required-parameters)
                 (declare (optimize (speed 3) (debug 0) (safety 0)
                                    (compilation-speed 0)))
                 ,@(when documentation (list (cadr documentation)))
                 (funcall #',definer (layer-context-prototype *active-context*) ,@required-parameters))))
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (bind-lf-names ',name))
         #',definer))))

(defmacro define-layered-method (&whole form name &body body)
  (multiple-value-bind
      (layer-arg layer qualifiers args method-body)
      (parse-method-body form body)
    `(defmethod ,(lf-definer-name name)
                ,@qualifiers ((,layer-arg ,(prepare-layer layer)) ,@args)
       ,@(prepare-layered-method-body name form layer-arg method-body))))
