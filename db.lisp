;;; db.lisp
;;; Functions for interaction with the database.

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


;; (clsql:with-database (db '("localhost" "lab" "root" "smjh13oo") :database-type :mysql)
;;   (clsql:create-view-from-class 'sample :database db))

(defun csv-upload (instance-maker file &optional (db-connection '("localhost" "lab" "root" "smjh13oo")))
  "Process row into object and upload.
 Tried to make parallel, but I think that MySQL locks tables during insertion.
 INSTANCE-MAKER is a definstance function."
  (clsql:with-database (db db-connection :database-type :mysql :pool T)
    (flet ((object-loader (row)
             (let ((object (funcall instance-maker row)))
               ;; TODO: I should handle-bind instead of ignoring errors
               ;;(clsql:update-records-from-instance object :database db)
               (ignore-errors (clsql:update-records-from-instance object :database db))
               )))
      (cl-csv:read-csv file
                       :row-fn #'object-loader
                       :skip-first-p t
                       :separator #\|
                       :quote nil ;; there are quotes in comment strings
                       :unquoted-empty-string-is-nil t))))
