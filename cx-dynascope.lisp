(in-package :contextl)

(defun make-special-symbol ()
  "creates a fresh unique special symbol"
  (let ((symbol (make-dynamic-symbol "SPECIAL-SYMBOL-")))
    (setf (get symbol 'specialp) t)
    symbol))

(declaim (inline special-symbol-p))

(defun special-symbol-p (symbol)
  "checks whether a symbol is special, as created by make-special-symbol"
  (and #-cx-fast-special-symbol-progv
       (dynamic-symbol-p symbol)
       #+cx-fast-special-symbol-progv
       (symbolp symbol)
       (get symbol 'specialp)))

(defvar *symbol-access* nil
  "set/get a place's special symbol instead of its symbol value
   when this is set to a non-nil value")

(defmacro with-symbol-access (&body body)
  "executes body in an environment with *symbol-access* set to t"
  `(let ((*symbol-access* t))
     ,@body))

(defmacro without-symbol-access (&body body)
  "executes body in an environment with *symbol-access* set to nil"
  `(let ((*symbol-access* nil))
     ,@body))

(defun prepare-binding (binding env)
  "ensure that a binding form is 'well-formed' to ease further processing"
  (when (symbolp binding)
    (setf binding (list binding nil)))
  (assert (null (cddr binding)) ()
    "Bad initialization form: ~S." binding)
  `(,(macroexpand (car binding) env) ,@(cdr binding)))

(define-symbol-macro safe-special-symbol-progv
  #-cx-fast-special-symbol-progv t
  #+cx-fast-special-symbol-progv nil)
;; redefine this to nil to get more efficient code,
;; either globally via define-symbol-macro,
;; or locally via symbol-macrolet

(defmacro special-symbol-progv (symbols values &body body &environment env)
  "like dynamic-progv, only that symbols must all be special symbols"
  (if (macroexpand 'safe-special-symbol-progv env)
    (with-unique-names (symbol-list retry)
      `(let (,symbol-list)
         (tagbody
          ,retry (setq ,symbol-list ,symbols)
          (unless (every #'special-symbol-p ,symbol-list)
            (cerror "Retry to rebind the place(s)."
                    "Attempt at rebinding one or more non-special places: ~S"
                    ',symbols)
            (go ,retry)))
         (dynamic-progv ,symbol-list ,values ,@body)))
    `(dynamic-progv ,symbols ,values ,@body)))

(defmacro special-symbol-reprogv (symbols values &body body &environment env)
  "like dynamic-reprogv, only that symbols must all be special symbols"
  (if (macroexpand 'safe-special-symbol-progv env)
    (with-unique-names (symbol-list retry)
      `(let (,symbol-list)
         (tagbody
          ,retry (setq ,symbol-list ,symbols)
          (unless (every #'special-symbol-p ,symbol-list)
            (cerror "Retry to rebind the place(s)."
                    "Attempt at rebinding one or more non-special places: ~S"
                    ',symbols)
            (go ,retry)))
         (dynamic-reprogv ,symbol-list ,values ,@body)))
    `(dynamic-reprogv ,symbols ,values ,@body)))

(defmacro dletf* (bindings &body body &environment env)
  "sequentially bind places to new values with dynamic scope,
   and execute body in that new dynamic environment"
  (loop for form = `(progn ,@body) then (etypecase (car binding)
                                          (symbol `(dlet (,binding) ,form))
                                          (cons `(special-symbol-progv
                                                     (list (with-symbol-access ,(car binding)))
                                                     (list ,(cadr binding))
                                                   ,form)))
        for binding in (reverse bindings)
        do (setf binding (prepare-binding binding env))
        finally (return form)))

(defmacro dreletf* (bindings &body body &environment env)
  "sequentially bind places to new values with dynamic scope,
   and execute body in that new dynamic environment"
  (loop for form = `(progn ,@body) then (etypecase (car binding)
                                          (symbol `(dreletf (,binding) ,form))
                                          (cons (with-unique-names (symbol-store)
                                                  `(let ((,symbol-store (list (with-symbol-access ,(car binding)))))
                                                     (special-symbol-reprogv
                                                         ,symbol-store
                                                         (list ,(cadr binding))
                                                       ,form)))))
        for binding in (reverse bindings)
        do (setf binding (prepare-binding binding env))
        finally (return form)))

(defmacro dletf (bindings &body body &environment env)
  "bind places to new values with dynamic scope in parallel,
   and execute body in that new dynamic environment"
  (loop for binding in bindings
        do (setf binding (prepare-binding binding env))
        collect (if (symbolp (car binding))
                  `',(%dynamic-symbol (car binding))
                  (car binding)) into symbol-forms
        when (symbolp (car binding)) collect (car binding) into variables
        collect (cadr binding) into value-forms
        finally (return `(special-symbol-progv
                             (with-symbol-access
                               (list ,@symbol-forms))
                             (list ,@value-forms)
                           (locally (declare (special ,@variables))
                             ,@body)))))

(defmacro dreletf (bindings &body body &environment env)
  "bind places to new values with dynamic scope in parallel,
   and execute body in that new dynamic environment"
  (loop for binding in bindings
        do (setf binding (prepare-binding binding env))
        collect (if (symbolp (car binding))
                  `',(%dynamic-symbol (car binding))
                  (car binding)) into symbol-forms
        when (symbolp (car binding)) collect (car binding) into variables
        collect (cadr binding) into value-forms
        finally (return (with-unique-names (symbol-store)
                          `(let ((,symbol-store (with-symbol-access
                                                  (list ,@symbol-forms))))
                             (special-symbol-reprogv
                                 ,symbol-store
                                 (list ,@value-forms)
                               (locally (declare (special ,@variables))
                                 ,@body)))))))
