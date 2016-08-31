;;; schema.lisp
;;; Functions for creating the database schema.

(in-package :beaker)
(require :sb-sprof)

;; Create tables from our view classes
;; Only the first time !!!!!
(defun create-schema (&optional (db-name "lab"))
  (let ((db-connection (list "localhost" db-name "root" "smjh13oo")))
    (flet ((handler-probe ()
             (handler-case
                 (clsql:probe-database db-connection :database-type :mysql)
               (clsql-sys:sql-connection-error () nil))))
      (cond ((handler-probe)
             (clsql:destroy-database db-connection :database-type :mysql)
             (clsql:create-database db-connection :database-type :mysql))
            (t (clsql:create-database db-connection :database-type :mysql)))
      (clsql:with-database (db db-connection :database-type :mysql)
        (clsql:create-view-from-class 'patient :database db)
        (clsql:create-view-from-class 'provider :database db)
        (clsql:create-view-from-class 'result :database db)
        (clsql:create-view-from-class 'sample :database db)))))

(defun add-table (table &optional (db-name "lab"))
  (let ((db-connection (list "localhost" db-name "root" "smjh13oo")))
    (clsql:with-database (db db-connection :database-type :mysql)
      (clsql:create-view-from-class table :database db))))
