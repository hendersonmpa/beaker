;; Provider instance closure
(let ((ht (create-index-hash *provider-file*)))
  (defun make-provider (row)
    (flet ((entry (key row)  ;ht closure
             (svref row (gethash key ht))))
      (make-instance 'provider
                     :name (entry 'prov_name row)
                     :id (parse-integer (entry 'prov_id row))
                     :line (parse-integer (entry 'line row))
                     :speciality (entry 'prov_specialty row)))))

(let ((ht (create-index-hash *patient-file*)))
  (defun make-patient (row)
    (flet ((entry (key row)  ;ht closure
             (svref row (gethash key ht))))
      (make-instance 'patient
                     :mrn (parse-integer (entry 'pat_mrn_id row))
                     :dob (entry 'pat_dob row)
                     :sex (entry 'pat_sex row)))))

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
                       :unquoted-empty-string-is-nil t))))o
