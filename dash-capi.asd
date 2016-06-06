;;;; dash-capi.asd

(asdf:defsystem #:dash-capi
  :description "Describe dash-capi here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:sqlite
               #:com.informatimago.common-lisp.html-parser
               #:alexandria
               #:cl-fad
               #:opticl)
  :serial t
  :components ((:file "package")
               (:file "dash-capi")))

