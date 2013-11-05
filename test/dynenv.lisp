(asdf:oos 'asdf:load-op :contextl)

(in-package :contextl-user)

(let ((symbol (make-dynamic-symbol)))
  (assert (dynamic-symbol-p symbol))
  #-cx-disable-dynamic-environments
  (assert (not (dynamic-symbol-p (gensym))))
  (assert (not (special-symbol-p symbol)))
  (assert (not (dynamic-symbol-boundp symbol)))
  (setf (dynamic-symbol-value symbol) 42)
  (assert (dynamic-symbol-boundp symbol))
  (assert (eql (dynamic-symbol-value symbol) 42))
  (dynamic-symbol-makunbound symbol)
  (assert (not (dynamic-symbol-boundp symbol)))
  (assert (handler-case
              (progn (dynamic-symbol-value symbol) nil)
            (error () t))))

(let ((symbol (make-special-symbol)))
  (assert (dynamic-symbol-p symbol))
  (assert (special-symbol-p symbol))
  (assert (not (special-symbol-p (gensym))))
  (assert (not (dynamic-symbol-boundp symbol)))
  (setf (dynamic-symbol-value symbol) 42)
  (assert (dynamic-symbol-boundp symbol))
  (assert (eql (dynamic-symbol-value symbol) 42))
  (dynamic-symbol-makunbound symbol)
  (assert (not (dynamic-symbol-boundp symbol)))
  (assert (handler-case
              (progn (dynamic-symbol-value symbol) nil)
            (error () t))))

#-cx-disable-dynamic-environments
(progn
  (defdynamic x 0)
  (defdynamic y 0)
  (defdynamic z 0)

  (defdynamic env
    (dynamic-let ((x 1) (y 2) (z 3))
      (capture-dynamic-environment)))

  (assert (and (zerop (dynamic x))
               (zerop (dynamic y))
               (zerop (dynamic z))))

  (assert (equal (with-dynamic-environment ((dynamic env))
                   (list (dynamic x) (dynamic y) (dynamic z)))
                 '(1 2 3)))

  (assert (and (zerop (dynamic x))
               (zerop (dynamic y))
               (zerop (dynamic z))))
  
  (assert (equal (with-dynamic-environment ((dynamic env))
                   (list (incf (dynamic x)) (incf (dynamic y)) (incf (dynamic z))))
                 '(2 3 4)))

  (assert (equal (with-dynamic-environment ((dynamic env))
                   (list (dynamic x) (dynamic y) (dynamic z)))
                 '(2 3 4)))

  (setf (dynamic env)
        (dynamic-let ((x 1))
          (with-dynamic-mark (mark)
            (dynamic-let ((y 2))
              (capture-dynamic-environment mark)))))
  
  (assert (with-dynamic-environment ((dynamic env))
            (and (zerop (dynamic x))
                 (eql (dynamic y) 2))))

  (defvar *mark*)

  (defun bam ()
    (capture-dynamic-environment *mark*))

  (defun baz ()
    (dynamic-let ((y 4))
      (bam)))

  (defun bar ()
    (with-dynamic-mark (*mark*)
      (baz)))

  (defun foo ()
    (dynamic-let ((x 3))
      (bar)))

  (setf (dynamic env) (foo))

  (assert (with-dynamic-environment ((dynamic env))
            (and (zerop (dynamic x))
                 (eql (dynamic y) 4))))

  (setf (dynamic env)
        (dynamic-let ((x 10))
          (with-dynamic-mark (mark1)
            (dynamic-let ((y 11))
              (with-dynamic-mark (mark2)
                (dynamic-let ((z 12))
                  (list (capture-dynamic-environment mark1)
                        (capture-dynamic-environment mark2))))))))

  (assert (with-dynamic-environment ((first (dynamic env)))
            (and (zerop (dynamic x))
                 (eql (dynamic y) 11)
                 (eql (dynamic z) 12))))

  (assert (with-dynamic-environment ((second (dynamic env)))
            (and (zerop (dynamic x))
                 (zerop (dynamic y))
                 (eql (dynamic z) 12))))

  (setf (dynamic x) '(1 2 3))

  (setf (dynamic env)
        (dynamic-relet ((x (list* 'a 'b 'c (dynamic x))))
          (capture-dynamic-environment)))

  (assert (dynamic-let ((x '(d e f)))
            (with-dynamic-environment ((dynamic env))
              (equal (dynamic x) '(a b c d e f)))))

  (defclass dummy ()
    ((x :special t :accessor x)
     (y :special t :accessor y)
     (z :special t :accessor z))
    (:metaclass special-class))

  (defparameter obj (make-instance 'dummy))

  (setf (dynamic env)
        (dletf (((x obj) 1)
                ((y obj) 2)
                ((z obj) 3))
          (capture-dynamic-environment)))

  (assert (not (or (slot-boundp obj 'x)
                   (slot-boundp obj 'y)
                   (slot-boundp obj 'z))))

  (assert (equal (with-dynamic-environment ((dynamic env))
                   (list (x obj) (y obj) (z obj)))
                 '(1 2 3)))

  (assert (not (or (slot-boundp obj 'x)
                   (slot-boundp obj 'y)
                   (slot-boundp obj 'z))))

  (assert (equal (with-dynamic-environment ((dynamic env))
                   (list (incf (x obj)) (incf (y obj)) (incf (z obj))))
                 '(2 3 4)))

  (assert (equal (with-dynamic-environment ((dynamic env))
                   (list (x obj) (y obj) (z obj)))
                 '(2 3 4)))

  (setf (dynamic env)
        (dletf (((x obj) 1))
          (with-dynamic-mark (mark)
            (dletf (((y obj) 2))
              (capture-dynamic-environment mark)))))

  (assert (with-dynamic-environment ((dynamic env))
            (and (not (slot-boundp obj 'x))
                 (eql (y obj) 2))))

  (defun bam1 ()
    (capture-dynamic-environment *mark*))

  (defun baz1 ()
    (dletf (((y obj) 4))
      (bam1)))

  (defun bar1 ()
    (with-dynamic-mark (*mark*)
      (baz1)))

  (defun foo1 ()
    (dletf (((x obj) 3))
      (bar1)))

  (setf (dynamic env) (foo1))

  (assert (with-dynamic-environment ((dynamic env))
            (and (not (slot-boundp obj 'x))
                 (eql (y obj) 4))))

  (setf (dynamic env)
        (dletf (((x obj) 10))
          (with-dynamic-mark (mark1)
            (dletf (((y obj) 11))
              (with-dynamic-mark (mark2)
                (dletf (((z obj) 12))
                  (list (capture-dynamic-environment mark1)
                        (capture-dynamic-environment mark2))))))))

  (assert (with-dynamic-environment ((first (dynamic env)))
            (and (not (slot-boundp obj 'x))
                 (eql (y obj) 11)
                 (eql (z obj) 12))))

  (assert (with-dynamic-environment ((second (dynamic env)))
            (and (not (slot-boundp obj 'x))
                 (not (slot-boundp obj 'y))
                 (eql (z obj) 12))))

  (setf (dynamic env)
        (dletf* (((x obj) 1)
                 ((y obj) (+ (x obj) (x obj)))
                 ((z obj) (+ (y obj) (y obj))))
          (capture-dynamic-environment)))

  (assert (equal (with-dynamic-environment ((dynamic env))
                   (list (x obj) (y obj) (z obj)))
                 '(1 2 4)))

  (setf (x obj) '(1 2 3))

  (setf (dynamic env)
        (dreletf (((x obj) (list* 'a 'b 'c (x obj))))
          (capture-dynamic-environment)))

  (assert (dreletf (((x obj) '(d e f)))
            (with-dynamic-environment ((dynamic env))
              (equal (x obj) '(a b c d e f)))))

  (deflayer l1)
  (deflayer l2)
  (deflayer l3)

  (setf (dynamic env)
        (with-active-layers (l1 l2 l3)
          (assert (equal (mapcar #'layer-name (active-layers)) '(l3 l2 l1 t)))
          (capture-dynamic-environment)))

  (assert (equal (with-dynamic-environment ((dynamic env))
                   (mapcar #'layer-name (active-layers)))
                 '(l3 l2 l1 t)))

  (setf (dynamic env)
        (with-active-layers (l1 l2 l3)
          (with-dynamic-mark (mark)
            (with-inactive-layers (l1 l3)
              (list (capture-dynamic-environment)
                    (capture-dynamic-environment mark))))))

  (assert (equal (with-dynamic-environment ((first (dynamic env)))
                   (mapcar #'layer-name (active-layers)))
                 '(l2 t)))

  (assert (equal (with-dynamic-environment ((second (dynamic env)))
                   (mapcar #'layer-name (active-layers)))
                 '(t)))

  (assert (equal (with-active-layers (l3 l2 l1)
                   (with-dynamic-environment ((second (dynamic env)))
                     (mapcar #'layer-name (active-layers))))
                 '(l2 t)))

  (assert (equal (with-active-layers (l1 l2 l3 t)
                   (with-dynamic-environment ((second (dynamic env)))
                     (with-active-layers (l1)
                       (mapcar #'layer-name (active-layers)))))
                 '(l1 l2 t)))

  (setf (dynamic env)
        (dynamic-wind
          (handler-case
              (proceed (capture-dynamic-environment))
            (error () (print "error caught correctly") t))))

  (assert (with-dynamic-environment ((dynamic env))
            (error "This is an error.")))

  (defdynamic xxx nil)

  (defparameter *y*
    (dlet ((xxx 1))
      (capture-dynamic-environment)))

  (assert (eql (with-dynamic-environment (*y*)
                 (dynamic xxx))
               1))

  (defparameter *x*
    (with-dynamic-environment (*y*)
      (capture-dynamic-environment)))
  
  (assert (eql (with-dynamic-environment (*x*)
                 (dynamic xxx))
               1))

  #+lispworks5
  (print "This part of the test suite currently doesn't run on LispWorks 5.x.")

  #-lispworks5
  (progn
    (deflayer l5 ()
              ((x :initarg :x :accessor x)
               (y :initarg :y :accessor y)))
    
    (setf (dynamic env)
          (with-active-layers ((l5 :x 5 :y 8))
            (capture-dynamic-environment)))
    
    (assert (equal (with-dynamic-environment ((dynamic env))
                                             (list (mapcar #'layer-name (active-layers))
                                                   (x (find-layer 'l5))
                                                   (y (find-layer 'l5))))
                   '((l5 t) 5 8)))
    
    (setf (dynamic env) (with-active-layers* ((l5 :x 5) (l5 :y (* 2 (x (find-layer 'l5)))))
                                   (capture-dynamic-environment)))
    
    (assert (equal (with-dynamic-environment ((dynamic env))
                                             (list (mapcar #'layer-name (active-layers))
                                                   (x (find-layer 'l5))
                                                   (y (find-layer 'l5))))
                   '((l5 t) 5 10)))))

#+cx-disable-dynamic-environments
(print "Dynamic environments not supported.")

#+abcl (extensions:quit)
#+allegro (excl:exit)
#+clozure (ccl:quit)
#+cmu (ext:quit)
#+ecl (si:quit)
#+sbcl (sb-ext:quit)
