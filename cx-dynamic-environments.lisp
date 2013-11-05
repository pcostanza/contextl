(in-package :contextl)

#-cx-disable-dynamic-environments
(defvar *dynamic-wind-stack* '())

(defstruct (dynamic-mark (:constructor make-dynamic-mark (name)))
  (name nil :read-only t))

(defmacro with-dynamic-mark ((mark-variable) &body body)
  (let ((mark (gensym)))
    `(let* ((,mark (make-dynamic-mark ',mark-variable))
            #-cx-disable-dynamic-environments
            (*dynamic-wind-stack* (cons ,mark *dynamic-wind-stack*))
            (,mark-variable ,mark))
       ,@body)))

(defmacro dynamic-wind (&body body)
  (let ((proceed-name (cond ((eq (first body) :proceed)
                             (pop body) (pop body))
                            (t 'proceed))))
    (assert (symbolp proceed-name) (proceed-name))
    #-cx-disable-dynamic-environments
    (with-unique-names (dynamic-wind-thunk proceed-thunk proceed-body)
      `(flet ((,dynamic-wind-thunk (,proceed-thunk)
                (macrolet ((,proceed-name (&body ,proceed-body)
                             `(if ,',proceed-thunk
                                (funcall (the function ,',proceed-thunk))
                                (progn ,@,proceed-body))))
                  ,@body)))
         (declare (inline ,dynamic-wind-thunk))
         (let ((*dynamic-wind-stack* (cons #',dynamic-wind-thunk *dynamic-wind-stack*)))
           (,dynamic-wind-thunk nil))))
    #+cx-disable-dynamic-environments
    (with-unique-names (proceed-body)
      `(macrolet ((,proceed-name (&body ,proceed-body)
                    `(progn ,@,proceed-body)))
         ,@body))))

#-cx-disable-dynamic-environments
(progn
  (defclass dynamic-environment ()
    ((dynamic-winds :initarg :dynamic-winds :reader dynamic-winds)))

  (defun capture-dynamic-environment (&optional mark)
    (make-instance 'dynamic-environment
                   :dynamic-winds
                   (loop with dynamic-winds = '()
                         for entry in *dynamic-wind-stack*
                         if (functionp entry) do (push entry dynamic-winds)
                         else if (eq entry mark) return dynamic-winds
                         finally (return dynamic-winds))))

  (defgeneric call-with-dynamic-environment (environment thunk)
    (:method ((environment dynamic-environment) (thunk function))
     (declare (optimize (speed 3) (space 3) (debug 0) (safety 0)
                        (compilation-speed 0)))
     (labels ((perform-calls (environment thunk)
                (cond (environment
                       (assert (consp environment))
                       (let ((function (first environment)))
                         (assert (functionp function))
                         (let ((*dynamic-wind-stack* (cons function *dynamic-wind-stack*)))
                           (funcall function (lambda () (perform-calls (rest environment) thunk))))))
                      (t (funcall thunk)))))
       (perform-calls (dynamic-winds environment) thunk))))

  (defmacro with-dynamic-environment ((environment) &body body)
    `(call-with-dynamic-environment ,environment (lambda () ,@body))))
