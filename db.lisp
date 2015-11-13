;;; db.lisp
;;; Functions for interaction with the database.

(in-package :beaker)

;; Create tables from our view classes
;; Only the first time !!!!!
(defun create-schema ()
  (let ((db-connection '("localhost" "lab" "root" "smjh13oo")))
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

(defun csv-to-loo (file instance-maker)
  "Process csv into list of objects.
 INSTANCE-MAKER is a definstance function."
  (flet ((list-builder (row)
           (funcall instance-maker (list-to-array row))))
    (cl-csv:read-csv file
                     :map-fn #'list-builder
                     :skip-first-p t
                     :separator #\|
                     :unquoted-empty-string-is-nil t)))


(defun update-table (file instance-maker
                     &optional (db-connection '("localhost" "lab" "root" "smjh13oo")))
  "Update database with objects created from FILE using INSTANCE-MAKER"
  (let ((loo (csv-to-loo file instance-maker)))
    ;; creates a new connection for each object in case of errors
    (dolist (object loo)
      (handler-case        ; error when repeat entries are attempted
          (clsql:with-database (db db-connection :database-type :mysql)
            (clsql:update-records-from-instance object :database db))
        (clsql-sys:sql-database-data-error () nil)))))

(defun test-update ()
  (let ((file-path (merge-pathnames *data-repository* "beaker.sqlite")))
    (if (probe-file file-path) ;; only useful for sqlite
        (delete-file file-path)
        (print "Database doesn't exist"))
    (create-schema)
    (update-database)))


(defun update-database ()
  (update-table *patient-file* #'make-patient)
  (update-table *provider-file* #'make-provider)
  (update-table *result-file* #'make-result)
  (update-table *sample-file* #'make-sample))

(defun add-dir-db (&optional (location *data-repository*))
  (flet ((extract-file-p (pathname) (cl-ppcre:scan "CHEO Lab Quality Indicators" (namestring pathname))))
    (create-schema)
    (cl-fad:walk-directory location #'update-database :test #'extract-file-p )
                                        (remove-duplicate-rows)))

;;(clsql:connect '("localhost" "lab" "root" "smjh13oo") :database-type :mysql)
;;;(clsql:list-databases '("localhost" "lab" "root" "smjh13oo") :database-type :mysql)
;; (clsql:with-database (db '("localhost" "lab" "root" "smjh13oo")
;;                          :database-type :mysql)
;;   (clsql:create-view-from-class 'result :database db))
;;   (clsql:list-tables :owner :all :database db))
;; (clsql:create-database '("localhost" "lab" "root" "smjh13oo") :database-type :mysql)
;; (clsql:probe-database '("localhost" "lab" "root" "smjh13oo") :database-type :mysql)
