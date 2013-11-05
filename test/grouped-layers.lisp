(asdf:oos 'asdf:load-op :contextl)

(in-package :contextl-user)

(defclass grouped-layer (standard-layer-class) ())

(defgeneric group-root (layer))
(defgeneric default-layer (layer))

(define-layered-method adjoin-layer-using-class
  ((to-add grouped-layer) active-layers)
  (call-next-layered-method
   to-add
   (remove-layer (group-root (find-layer to-add)) active-layers)))

(define-layered-method remove-layer-using-class
  ((to-remove grouped-layer) active-layers)
  (declare (ignore active-layers))
  (multiple-value-bind
      (new-layers cacheablep)
      (call-next-method)
    (values
     (adjoin-layer (default-layer (find-layer to-remove)) new-layers)
     cacheablep)))

(deflayer output ()
  ((group-root :initform 'output :reader group-root)
   (default-layer :initform 'standard-output :reader default-layer)))

(deflayer standard-output (output) ()
  (:metaclass grouped-layer))

(deflayer html-output (output) ()
  (:metaclass grouped-layer))

(deflayer xml-output (output) ()
  (:metaclass grouped-layer))

(deflayer json-output (output) ()
  (:metaclass grouped-layer))

(define-layered-function make-output ()
  (:method () '(output))
  (:method :in standard-output ()
   (list* 'standard-output (call-next-method)))
  (:method :in html-output ()
   (list* 'html-output (call-next-method)))
  (:method :in xml-output ()
   (list* 'xml-output (call-next-method)))
  (:method :in json-output ()
   (list* 'json-output (call-next-method))))

(assert (equal (make-output) '(output)))

(with-active-layers (standard-output)
  (assert (equal (make-output) '(standard-output output)))
  (with-active-layers (html-output)
    (assert (equal (make-output) '(html-output output)))
    (with-active-layers (xml-output)
      (assert (equal (make-output) '(xml-output output)))
      (with-inactive-layers (xml-output)
        (assert (equal (make-output) '(standard-output output))))
      (assert (equal (make-output) '(xml-output output))))
    (assert (equal (make-output) '(html-output output))))
  (assert (equal (make-output) '(standard-output output))))

(print :done)


#+abcl (extensions:quit)
#+allegro (excl:exit)
#+clozure (ccl:quit)
#+cmu (ext:quit)
#+ecl (si:quit)
#+sbcl (sb-ext:quit)
