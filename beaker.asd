;;;; beaker.asd

(asdf:defsystem #:beaker
  :description "Describe beaker here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:cl-csv
               #:clsql
               #:clsql-mysql
               #:parse-number
               #:cl-ppcre
               #:split-sequence
               #:lparallel
               #:cl-fad) ;; TODO: change to UIOP
  :components ((:file "package")
               (:file "view-classes")
               (:file "utilities")
               (:file "instances")
               (:file "schema")
               (:file "load")))
