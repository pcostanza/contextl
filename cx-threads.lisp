(in-package :contextl)

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :process))

#+(or abcl allegro clozure (and cmu mp) (and ecl threads) lispworks mcl (and sbcl sb-thread) scl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :cx-threads *features*))

(declaim (inline make-lock))

(defun make-lock (&key (name "contextl lock"))
  #-cx-threads name
  #+abcl (declare (ignore name))
  #+abcl (threads:make-thread-lock)
  #+allegro (mp:make-process-lock :name name)
  #+(or clozure mcl) (ccl:make-lock name)
  #+(and cmu mp) (mp:make-lock name)
  #+(and ecl threads) (mp:make-lock :name name)
  #+lispworks (mp:make-lock :name name)
  #+(and sbcl sb-thread) (sb-thread:make-mutex :name name)
  #+scl (thread:make-lock name))

(define-compiler-macro make-lock (&key (name "contextl lock"))
  #-cx-threads name
  #+abcl (declare (ignore name))
  #+abcl '(threads:make-thread-lock)
  #+allegro `(mp:make-process-lock :name ,name)
  #+(or clozure mcl) `(ccl:make-lock ,name)
  #+(and cmu mp) `(mp:make-lock ,name)
  #+(and ecl threads) `(mp:make-lock :name ,name)
  #+lispworks `(mp:make-lock :name ,name)
  #+(and sbcl sb-thread) `(sb-thread:make-mutex :name ,name)
  #+scl `(thread:make-lock ,name))

(defmacro with-lock ((lock) &body body)
  #-cx-threads (declare (ignore lock))
  #-cx-threads `(progn ,@body)
  #+abcl `(threads:with-thread-lock (,lock) ,@body)
  #+allegro `(mp:with-process-lock (,lock) ,@body)
  #+(or clozure mcl) `(ccl:with-lock-grabbed (,lock) ,@body)
  #+(and cmu mp) `(mp:with-lock-held (,lock) ,@body)
  #+(and ecl threads) `(mp:with-lock (,lock) ,@body)
  #+lispworks `(mp:with-lock (,lock) ,@body)
  #+(and sbcl sb-thread) `(sb-thread:with-recursive-lock (,lock) ,@body)
  #+scl `(thread:with-lock-held (,lock) ,@body))

#+cx-threads
(defvar *atomic-operation-lock* (make-lock :name "contextl atomic operation lock"))

(defmacro as-atomic-operation (&body body)
  #-cx-threads `(progn ,@body)
  #+cx-threads `(with-lock (*atomic-operation-lock*) ,@body))

(defstruct (symbol-mapper (:constructor make-symbol-mapper (name)))
  (name nil :read-only t)
  
  (map (make-hash-table
        :test #'eq
        
        #+allegro :weak-keys #+allegro t
        #+clisp :weak #+clisp :key
        #+(or clozure mcl) :weak #+(or clozure mcl) t
        #+cmu :weak-p #+cmu :key
        #+lispworks :weak-kind #+lispworks :key
        #+sbcl :weakness #+sbcl :key

        #+clozure :lock-free #+clozure t)
       
       :read-only t)
  
  #-(or clozure lispworks sbcl scl)
  (lock (make-lock :name "symbol mapper") :read-only t))

(declaim (inline atomic-ensure-symbol-mapping))

(defun atomic-ensure-symbol-mapping (symbol mapper generate)
  (macrolet ((locked-access (&body body)
               #+lispworks `(with-hash-table-locked (symbol-mapper-map mapper) ,@body)
               #+sbcl `(sb-ext:with-locked-hash-table ((symbol-mapper-map mapper)) ,@body)
               #-(or lispworks sbcl) `(with-lock ((symbol-mapper-lock mapper)) ,@body)))
    (or (gethash symbol (symbol-mapper-map mapper))
        #+(or clozure scl (not cx-threads))
        (setf (gethash symbol (symbol-mapper-map mapper)) (funcall generate))
        #+(and cx-threads (not clozure) (not scl))
        (locked-access
         (or (gethash symbol (symbol-mapper-map mapper))
             (setf (gethash symbol (symbol-mapper-map mapper)) (funcall generate)))))))

(defgeneric map-symbol (mapper symbol &optional generate)
  (:method ((mapper symbol-mapper) (symbol symbol) &optional (generate #'gensym))
   (if (symbol-package symbol)
     (intern (format nil "=~A-FOR-~A="
                     (symbol-mapper-name mapper)
                     (symbol-name symbol))
             (symbol-package symbol))
     (atomic-ensure-symbol-mapping symbol mapper generate))))
