;;;; beaker.asd

(asdf:defsystem #:beaker
  :description "Describe beaker here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:cl-csv
               #:clsql
               #:clsql-sqlite3
               #:cl-ppcre
               #:cl-cwd
               #:cl-fad)
  :components ((:file "package")
               (:file "beaker")))
