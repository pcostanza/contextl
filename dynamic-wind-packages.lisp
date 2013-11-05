(in-package :cl-user)

(defpackage #:contextl
  (:use #:common-lisp #:lispworks)
  (:export
   #:*symbol-access*
   #:call-with-dynamic-environment
   #:capture-dynamic-environment
   #:defdynamic
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
   #:make-dynamic-symbol
   #:make-special-symbol
   #:proceed
   #:set-dynamic
   #:safe-special-symbol-progv
   #:special-symbol-p
   #:special-symbol-progv
   #:special-symbol-reprogv
   #:with-dynamic-environment
   #:with-dynamic-mark
   #:with-symbol-access
   #:without-symbol-access))

(defpackage #:contextl-common-lisp
  (:nicknames #:cxcl)
  (:use #:common-lisp #:contextl)
  #.`(:export
      ,@(loop for sym being the external-symbols of :common-lisp
              collect sym)
      ,@(loop for sym being the external-symbols of :contextl
              collect sym)))

(defpackage #:contextl-user
  (:use #:contextl-common-lisp)
  (:nicknames #:cx-user))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :dynamic-wind *features*))
