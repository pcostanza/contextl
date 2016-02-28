(asdf:defsystem #:dynamic-wind
  :name "dynamic-wind"
  :description "The dynamic-wind part of ContextL as a separate independent system definition."
  :author "Pascal Costanza"
  :version "1.0.0"
  :licence "MIT-style license"
  :depends-on (#-lispworks #:lw-compat)
  :components ((:file "dynamic-wind-packages")
               (:file "cx-threads" :depends-on ("dynamic-wind-packages"))
               (:file "cx-dynamic-environments" :depends-on ("dynamic-wind-packages"))
               (:file "cx-dynamic-variables" :depends-on ("dynamic-wind-packages" "cx-dynamic-environments" "cx-threads"))
               (:file "cx-dynascope" :depends-on ("dynamic-wind-packages" "cx-dynamic-variables"))))
