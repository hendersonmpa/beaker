Provider instance closure
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
