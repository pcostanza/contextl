(asdf:oos 'asdf:load-op :contextl)
(in-package :contextl-user)

(deflayer test-layer)
(print (find-layer 'test-layer))
(print (find-layer-class 'test-layer))

(define-layered-class test-class () ())
(print (find-class 'test-class))

(define-layered-function test-function (a b c))
(print (layered-function-definer 'test-function))

(define-layered-method test-function ((a integer) (b cons) c)
  42)

(define-layered-method test-function :in test-layer :around ((a integer) (b cons) c)
  4711)

(pprint (generic-function-methods (layered-function-definer 'test-function)))

(print :done)
