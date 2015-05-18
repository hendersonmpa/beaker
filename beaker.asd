;;;; beaker.asd

(asdf:defsystem #:beaker
  :description "Describe beaker here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:cl-csv
               #:clsql
               #:cl-ppcre
               #:cl-cwd)
  :components ((:file "package")
               (:file "beaker")))
