;;;; dash-capi.asd

(asdf:defsystem #:dash-capi
  :description "dash-capi is a tool to convert LW CAPI documentation to Dash docset"
  :author "Alexey Veretennikov <alexey.veretennikov@gmail.com>"
  :license "MIT"
  :depends-on (#:sqlite
               #:com.informatimago.common-lisp.html-parser
               #:alexandria
               #:cl-fad
               #:opticl)
  :serial t
  :components ((:file "package")
               (:file "dash-capi")))

