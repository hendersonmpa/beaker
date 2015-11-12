;;; db.lisp
;;; Interaction with the database

(defun update-tables (object)
  "Update database with entries"
  (handler-case ; error when repeat entries are attempted
      (clsql:with-database (db '("localhost" "lab" "root" "smjh13oo")
                               :database-type :mysql)
        (clsql:update-records-from-instance object :database db))
    (clsql-sys:sql-database-data-error () nil)))

(defun remove-duplicate-rows (&optional (db-connection *db-file*))
  (clsql:with-database (db (list db-connection)
                           :database-type :sqlite3)
    (clsql:execute-command "delete from physician where rowid not in (select max(rowid) from physician group by provider);"
                           :database db )
    (clsql:execute-command "delete from patient where rowid not in (select max(rowid) from patient group by mrn);"
                           :database db )
    (clsql:execute-command "delete from sample where rowid not in (select max(rowid) from sample group by specimen_number);"
                           :database db )))

(defun add-entry (row)
  (let ((tables-struct (create-tables-struct row)))
    (update-tables tables-struct)))

(defun update-database (file)
  (cl-csv:read-csv file
                   :map-fn #'add-entry
                   :skip-first-p t
                   :unquoted-empty-string-is-nil t))

(defun test-update ()
  (let ((file-path (merge-pathnames *data-repository* "beaker.sqlite")))
    (if (probe-file file-path) ;; only useful for sqlite
        (delete-file file-path)
        (print "Database doesn't exist"))
    (create-schema)
    (update-database)))

(defun add-dir-db (&optional (location *data-repository*))
  (flet ((extract-file-p (pathname) (cl-ppcre:scan "CHEO Lab Quality Indicators" (namestring pathname))))
    (create-schema)
    (cl-fad:walk-directory location #'update-database :test #'extract-file-p )
                                        (remove-duplicate-rows)))


;;(clsql:connect '("localhost" "lab" "root" "smjh13oo") :database-type :mysql)
;;;(clsql:list-databases '("localhost" "lab" "root" "smjh13oo") :database-type :mysql)
;; (clsql:with-database (db '("localhost" "lab" "root" "smjh13oo")
;;                          :database-type :mysql)
;;   (clsql:list-tables :owner :all :database db))
