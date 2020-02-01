(in-package :cl-user)

(defpackage #:contextl
  #+lispworks5
  (:import-from #:system #:with-hash-table-locked)
  #-(or lispworks4 lispworks5)
  (:import-from #:hcl #:with-hash-table-locked)
  (:use #:closer-common-lisp #:lispworks)
  (:export
   #:*symbol-access*
   #:active-layers
   #:adjoin-layer
   #:adjoin-layer-using-class
   #:apply-with-layer-context
   #:call-next-layered-method
   #:call-with-dynamic-environment
   #:capture-dynamic-environment
   #:class-layer
   #:clear-layer-caches
   #:current-layer-context
   #:defdynamic
   #:define-layered-class
   #:define-layered-function
   #:define-layered-method
   #:deflayer
   #:dlet #:dlet*
   #:dletf #:dletf*
   #:drelet #:drelet*
   #:dreletf #:dreletf*
   #:dynamic
   #:dynamic-environment
   #:dynamic-let
   #:dynamic-let*
   #:dynamic-mark
   #:dynamic-progv
   #:dynamic-relet
   #:dynamic-relet*
   #:dynamic-reprogv
   #:dynamic-symbol
   #:dynamic-symbol-boundp
   #:dynamic-symbol-makunbound
   #:dynamic-symbol-p
   #:dynamic-symbol-value
   #:dynamic-wind
   #:ensure-active-layer
   #:ensure-inactive-layer
   #:ensure-layer
   #:ensure-layered-function
   #:ensure-layered-method
   #:find-layer
   #:find-layer-class
   #:find-singleton
   #:funcall-with-layer-context
   #:layer-active-p
   #:layer-makunbound
   #:layer-name
   #:layered-access-class
   #:layered-class
   #:layered-direct-slot-definition
   #:layered-effective-slot-definition
   #:layered-effective-slot-definition-in-layers
   #:layered-function
   #:layered-function-argument-precedence-order
   #:layered-function-definer
   #:layered-function-lambda-list
   #:layered-method
   #:layered-method-lambda-list
   #:layered-method-layer
   #:layered-method-specializers
   #:lfmakunbound
   #:make-dynamic-symbol
   #:make-special-symbol
   #:partial-class
   #:partial-class-base-initargs
   #:partial-class-defining-classes
   #:partial-class-defining-metaclass
   #:partial-object
   #:proceed
   #:remove-layer
   #:remove-layer-using-class
   #:set-dynamic
   #:singleton-class
   #:slot-definition-layer
   #:slot-definition-layered-readers
   #:slot-definition-layered-writers
   #:slot-definition-layeredp
   #:slot-definition-layers
   #:slot-definition-specialp
   #:slot-boundp-using-layer
   #:slot-makunbound-using-layer
   #:slot-value-using-layer
   #:safe-special-symbol-progv
   #:special-class
   #:special-direct-slot-definition
   #:special-effective-slot-definition
   #:special-effective-slot-definition-in-layers
   #:special-layered-access-class
   #:special-layered-direct-slot-definition
   #:special-layered-effective-slot-definition
   #:special-object
   #:special-symbol-p
   #:special-symbol-progv
   #:special-symbol-reprogv
   #:standard-class-in-layer
   #:standard-direct-slot-definition-in-layer
   #:standard-effective-slot-definition-in-layers
   #:standard-layer-class
   #:standard-layer-object
   #:with-active-layers
   #:with-active-layers*
   #:with-dynamic-environment
   #:with-dynamic-mark
   #:with-inactive-layers
   #:with-special-initargs
   #:with-special-initargs*
   #:with-symbol-access
   #:without-symbol-access))

(in-package :contextl)

(defpackage #:contextl-common-lisp
  (:nicknames #:cxcl)
  (:use #:closer-common-lisp #:contextl)
  #.`(:export
      ,@(loop for sym being the external-symbols of :closer-common-lisp
              collect sym)
      ,@(loop for sym being the external-symbols of :contextl
              collect sym)))

(defpackage #:contextl-user
  (:use #:contextl-common-lisp)
  (:nicknames #:cx-user))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :dynamic-wind *features*)
  (pushnew :contextl *features*))
