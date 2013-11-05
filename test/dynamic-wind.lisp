(asdf:oos 'asdf:load-op :dynamic-wind)

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
               1)))

#+cx-disable-dynamic-environments
(print "Dynamic environments not supported.")

#+abcl (extensions:quit)
#+allegro (excl:exit)
#+clozure (ccl:quit)
#+cmu (ext:quit)
#+ecl (si:quit)
#+sbcl (sb-ext:quit)
