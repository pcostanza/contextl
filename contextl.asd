#|
Configuration flags (can be added to *features* before compiling ContextL):

:cx-disable-dynamic-environments disables dynamic-wind / proceed functionality
(and avoids the incurred overhead)

:cx-fast-special-symbol-progv avoids the added check for special symbols
(not necessary for correct semantics, only for added safety during development)

:cx-disable-special-class-in-layered-classes removes the metaclass special-class
from the metaclass layered-class and especially avoids the overhead when
initializing instances of such classes

:cx-disable-layer-gc disables the garbage collector for layers
(only interesting if you redefine layers and related generic functions at runtime,
 should not have a serious effect on runtime performance)
|#

;(push :cx-disable-dynamic-environments cl:*features*)
;(push :cx-fast-special-symbol-progv cl:*features*)
;(push :cx-disable-special-class-in-layered-classes cl:*features*)
;(push :cx-disable-layer-gc cl:*features*)

#+scl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "ContextL is currently not supported in Scieneer Common Lisp."))

(asdf:defsystem #:contextl
  :name "ContextL"
  :description "ContextL is a CLOS extension for Context-oriented Programming (COP)."
  :author "Pascal Costanza"
  :version "1.0.0"
  :licence "MIT-style license"
  :depends-on (#:closer-mop #-lispworks #:lw-compat)
  :components ((:file "contextl-packages")
               (:file "cx-threads" :depends-on ("contextl-packages"))
               (:file "cx-util" :depends-on ("contextl-packages" "cx-threads"))
               (:file "cx-dynamic-environments" :depends-on ("contextl-packages"))
               (:file "cx-dynamic-variables" :depends-on ("contextl-packages" "cx-dynamic-environments" "cx-threads"))
               (:file "cx-dynascope" :depends-on ("contextl-packages" "cx-dynamic-variables"))
               (:file "cx-special-class" :depends-on ("cx-dynascope"))
               (:file "cx-singleton-class" :depends-on ("contextl-packages" "cx-util"))
               (:file "cx-layered-function-macros" :depends-on ("contextl-packages" "cx-util"))
               (:file "cx-layer-metaclasses" :depends-on ("cx-special-class" "cx-singleton-class" "cx-threads" "cx-util"))
               (:file "cx-gc" :depends-on ("cx-layer-metaclasses" "cx-layered-function-macros" "cx-threads"))
               (:file "cx-layer" :depends-on ("cx-layer-metaclasses" "cx-layered-function-macros" "cx-gc" "cx-util" "cx-threads"))
               (:file "cx-partial-class" :depends-on ("cx-layer"))
               (:file "cx-class-in-layer" :depends-on ("cx-layer"))
               (:file "cx-layered-function" :depends-on ("cx-layer" "cx-util"))
               (:file "cx-layered-access-class" :depends-on ("cx-layered-function"))
               (:file "cx-layered-class" :depends-on ("cx-layered-access-class" "cx-partial-class"))))
