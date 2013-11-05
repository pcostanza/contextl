(in-package :contextl)

#-cx-disable-dynamic-environments
(progn
  (defvar %unbound '%unbound)

  (defstruct (dbox (:constructor make-dbox (value)))
    value)

  (defmethod print-object ((object dbox) stream)
    (print-unreadable-object (object stream :type t :identity t)
      (princ (dbox-value object)))))

(defvar *dynamic-symbol*
  (make-symbol-mapper 'dynamic-symbol))

(defun make-dynamic-symbol (&optional (x "DYNAMIC-SYMBOL-"))
  #-cx-disable-dynamic-environments
  (let ((symbol (gensym x)))
    (setf (symbol-value symbol)
          (make-dbox %unbound))
    symbol)
  #+cx-disable-dynamic-environments
  (gensym x))

(defun dynamic-symbol (symbol)
  (map-symbol *dynamic-symbol* symbol
              #-cx-disable-dynamic-environments
              #'make-dynamic-symbol))

(declaim (inline dynamic-symbol-p))

(defun dynamic-symbol-p (symbol)
  #-cx-disable-dynamic-environments
  (and (symbolp symbol)
       (boundp symbol)
       (dbox-p (symbol-value symbol)))
  #+cx-disable-dynamic-environments
  (symbolp symbol))

(declaim (inline dynamic-symbol-value (setf dynamic-symbol-value)
                 dynamic-symbol-boundp dynamic-symbol-makunbound))

(defun dynamic-symbol-value (symbol)
  #-cx-disable-dynamic-environments
  (let ((value (dbox-value (symbol-value symbol))))
    (if (eq value %unbound)
      (error 'unbound-variable :name symbol)
      value))
  #+cx-disable-dynamic-environments
  (symbol-value symbol))

(defun (setf dynamic-symbol-value) (value symbol)
  #-cx-disable-dynamic-environments
  (setf (dbox-value (symbol-value symbol)) value)
  #+cx-disable-dynamic-environments
  (setf (symbol-value symbol) value))

(defun dynamic-symbol-boundp (symbol)
  #-cx-disable-dynamic-environments
  (not (eq (dbox-value (symbol-value symbol)) %unbound))
  #+cx-disable-dynamic-environments
  (boundp symbol))

(defun dynamic-symbol-makunbound (symbol)
  #-cx-disable-dynamic-environments
  (setf (dbox-value (symbol-value symbol)) %unbound)
  #+cx-disable-dynamic-environments
  (makunbound symbol))

#-cx-disable-dynamic-environments
(progn
  (declaim (inline compute-bindings))
  
  (defun compute-bindings (symbols values)
    (loop for nil in symbols
          if values collect (make-dbox (pop values))
          else collect (make-dbox %unbound))))

(defmacro dynamic-progv (symbols values &body body)
  #-cx-disable-dynamic-environments
  (with-unique-names (fixed-symbols fixed-bindings proceed)
    `(let* ((,fixed-symbols ,symbols)
            (,fixed-bindings (compute-bindings ,fixed-symbols ,values)))
       (dynamic-wind :proceed ,proceed
         (progv ,fixed-symbols ,fixed-bindings
           (,proceed ,@body)))))
  #+cx-disable-dynamic-environments
  `(progv ,symbols ,values ,@body))

(defmacro dynamic-reprogv (symbols values &body body)
  #-cx-disable-dynamic-environments
  (with-unique-names (computed-symbols computed-bindings proceed)
    `(dynamic-wind :proceed ,proceed
       (let* ((,computed-symbols ,symbols)
              (,computed-bindings (compute-bindings ,computed-symbols ,values)))
         (progv ,computed-symbols ,computed-bindings
           (,proceed ,@body)))))
  #+cx-disable-dynamic-environments
  `(progv ,symbols ,values ,@body))

(declaim (inline %dynamic-symbol))

(defun %dynamic-symbol (symbol)
  (map-symbol *dynamic-symbol* symbol))

(defmacro defdynamic (name &body form)
  (assert (and (consp form) (null (cdr form))))
  `(progn
     (defparameter ,(%dynamic-symbol name)
       #-cx-disable-dynamic-environments (make-dbox ,@form)
       #+cx-disable-dynamic-environments ,@form)
     ',name))

(defmacro dynamic (var)
  #-cx-disable-dynamic-environments
  `(dbox-value ,(%dynamic-symbol var))
  #+cx-disable-dynamic-environments
  (%dynamic-symbol var))

(defmacro set-dynamic (form var)
  `(setf (dynamic ,var) ,form))

(defmacro dynamic-let ((&rest bindings) &body body)
  (assert (and (every #'consp bindings)
               (notany #'cddr bindings)))
  #-cx-disable-dynamic-environments
  (loop with proceed = (gensym)
        for (var form) in bindings
        collect (copy-symbol var) into stores
        collect (%dynamic-symbol var) into symbols
        collect form into forms
        finally
        (return `(let ,(loop for store in stores
                             for form in forms
                             collect `(,store (make-dbox ,form)))
                   (dynamic-wind :proceed ,proceed
                     (let ,(loop for symbol in symbols
                                 for store in stores
                                 collect `(,symbol ,store))
                       (declare (special ,@symbols))
                       (,proceed ,@body))))))
  #+cx-disable-dynamic-environments
  `(let ,(loop for (var form) in bindings
               collect `(,(%dynamic-symbol var) ,form))
     ,@body))

(defmacro dlet ((&rest bindings) &body body)
  `(dynamic-let ,bindings ,@body))

(defmacro dynamic-let* ((&rest bindings) &body body)
  (if bindings
    `(dynamic-let (,(first bindings))
       (dynamic-let* ,(rest bindings)
         ,@body))
    `(progn ,@body)))

(defmacro dlet* ((&rest bindings) &body body)
  `(dynamic-let* ,bindings ,@body))

(defmacro dynamic-relet ((&rest bindings) &body body)
  (assert (and (every #'consp bindings)
               (notany #'cddr bindings)))
  #-cx-disable-dynamic-environments
  (with-unique-names (proceed)
    (loop for (var form) in bindings
          for symbol = (%dynamic-symbol var)
          collect symbol into symbols
          collect `(,symbol (make-dbox ,form)) into new-bindings
          finally (return
                   `(dynamic-wind :proceed ,proceed
                      (let ,new-bindings
                        (declare (special ,@symbols))
                        (,proceed ,@body))))))
  #+cx-disable-dynamic-environments
  `(let ,(loop for (var form) in bindings
               collect `(,(%dynamic-symbol var) ,form))
     ,@body))

(defmacro drelet ((&rest bindings) &body body)
  `(dynamic-relet ,bindings ,@body))

(defmacro dynamic-relet* ((&rest bindings) &body body)
  (if bindings
    `(dynamic-relet (,(first bindings))
       (dynamic-relet* ,(rest bindings)
         ,@body))
    `(progn ,@body)))

(defmacro drelet* ((&rest bindings) &body body)
  `(dynamic-relet* ,bindings ,@body))
