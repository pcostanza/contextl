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
  :author "Pascal Costanza"
  :version "1.0.0"
  :licence "
Copyright (c) 2005 - 2013 Pascal Costanza

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the \"Software\"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or
sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
"
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
