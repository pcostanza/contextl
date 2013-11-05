(in-package :contextl)

#-cx-disable-layer-gc
(progn
  (defun all-layer-contexts ()
    (let ((result '()))
      (labels ((collect (layer-context)
                 (declare (type layer-context layer-context))
                 (when (member layer-context result :test #'eq)
                   (return-from collect))
                 (push layer-context result)
                 (loop for (nil child) on (layer-context-children/ensure-active layer-context) by #'cddr do
                       (collect child))
                 (loop for (nil child) on (layer-context-children/ensure-inactive layer-context) by #'cddr do
                       (collect child))))
        (when (boundp '*root-context*)
          (collect (symbol-value '*root-context*))
          result))))

  (defun clear-layer-active-caches (test &optional (all-layer-contexts (all-layer-contexts)))
    (loop for layer-context in all-layer-contexts do
          (with-lock ((layer-context-lock layer-context))
            (setf (layer-context-children/ensure-active layer-context)
                  (loop for (key child) on (layer-context-children/ensure-active layer-context) by #'cddr
                        unless (funcall test key)
                        nconc (list key child))))))

  (defun clear-layer-inactive-caches (test &optional (all-layer-contexts (all-layer-contexts)))
    (loop for layer-context in all-layer-contexts do
          (with-lock ((layer-context-lock layer-context))
            (setf (layer-context-children/ensure-inactive layer-context)
                  (loop for (key child) on (layer-context-children/ensure-inactive layer-context) by #'cddr
                        unless (funcall test key)
                        nconc (list key child))))))

  (defgeneric clear-layer-context-caches (layer)
    (:method ((layer symbol)) (clear-layer-context-caches (find-layer-class layer)))
    (:method ((layer standard-layer-object)) (clear-layer-context-caches (find-layer-class layer)))
    (:method ((layer-class cl:class))
     (let ((all-layer-contexts (all-layer-contexts))
           (test (lambda (key) (subtypep (find-layer-class key) layer-class))))
       (clear-layer-active-caches test all-layer-contexts)
       (clear-layer-inactive-caches test all-layer-contexts))))

  (defun clear-layer-caches ()
    (let ((all-layer-contexts (all-layer-contexts)))
      (loop for layer-context in all-layer-contexts do
            (with-lock ((layer-context-lock layer-context))
              (setf (layer-context-children/ensure-active layer-context) '()
                    (layer-context-children/ensure-inactive layer-context) '())))))

  (defmethod reinitialize-instance :after
    ((class standard-layer-class) &rest initargs)
    (declare (ignore initargs))
    (clear-layer-context-caches class))

  (defgeneric clear-activation-method-caches (gf method)
    (:method (gf method) (declare (ignore gf method)) nil)
    (:method ((gf (eql (lf-definer-name 'adjoin-layer-using-class))) method)
     (let ((layer-specializer (first (layered-method-specializers method))))
       (if (typep layer-specializer 'eql-specializer)
         (let ((eql-specializer-object (eql-specializer-object layer-specializer)))
           (clear-layer-active-caches (lambda (key) (eql (find-layer-class key) eql-specializer-object))))
         (clear-layer-active-caches (lambda (key) (typep (find-layer-class key) layer-specializer))))))
    (:method ((gf (eql (lf-definer-name 'remove-layer-using-class))) method)
     (let ((layer-specializer (first (layered-method-specializers method))))
       (if (typep layer-specializer 'eql-specializer)
         (let ((eql-specializer-object (eql-specializer-object layer-specializer)))
           (clear-layer-inactive-caches (lambda (key) (eql (find-layer-class key) eql-specializer-object))))
         (clear-layer-inactive-caches (lambda (key) (typep (find-layer-class key) layer-specializer)))))))

  (defmethod add-method :after
    ((gf layered-function) (method layered-method))
    (clear-activation-method-caches (generic-function-name gf) method))

  (defmethod remove-method :after
    ((gf layered-function) (method layered-method))
    (clear-activation-method-caches (generic-function-name gf) method)))
