;;; file instances.lisp
;;; Functions and macros for creating instances from csv files.

(in-package #:beaker)
(defparameter *data-archive* "~/CHEO/LIS/data_mart/archive/")
(defparameter *provider-ht*
  (create-index-hash (merge-pathnames "DH_Physician_Extract.csv" *data-archive*)))
(defparameter *patient-ht*
  (create-index-hash (merge-pathnames "DH_Patient_Extract.csv" *data-archive*)))
(defparameter *result-ht*
  (create-index-hash (merge-pathnames "DH_Results_Extract_Yearly1.csv" *data-archive*)))
(defparameter *sample-ht*
  (create-index-hash (merge-pathnames "DH_Samples_Extract.csv" *data-archive*)))
(defparameter *price-ht*
  (create-index-hash (merge-pathnames "price.psv" *data-archive*)))

;;; Read in the csv files
(defun create-row-vector (file)
  "Read the csv into a list of vectors"
  (cl-csv:read-csv file
                   :map-fn #'(lambda (row)
                               (make-array (length row)
                                           :initial-contents row))
                   ;;:sample 10
                   :skip-first-p t
                   :separator #\|
                   :unquoted-empty-string-is-nil t))

(defmacro definstance (name class ht &body body)
  "Return functions to make instances using the hash-table"
  (let ((gen-row (gensym "ROW"))
        (gen-key (gensym "KEY")))
    `(defun ,name (row)
       (flet ((entry (,gen-key ,gen-row)
                (elt ,gen-row (gethash ,gen-key ,ht))))
         (make-instance ,class
                        ,@body)))))

;;; Provider table instance
(definstance make-provider 'provider *provider-ht*
  :name (entry 'prov_name row)
  :id (handler-parse-integer (entry 'prov_id row))
  :line (handler-parse-integer (entry 'line row))
  :specialty (entry 'prov_specialty row))

;;; Patient table instance
(definstance make-patient 'patient *patient-ht*
  :mrn (handler-parse-integer (entry 'pat_mrn_id row))
  :dob (entry 'pat_dob row)
  :sex (entry 'pat_sex row))

;;; Result table instance
(definstance make-result 'result *result-ht*
  :accession (entry 'accession row)
  :ordered-datetime (handler-parse-timestring (entry 'ordered_datetime row))
  :verified-datetime (handler-parse-timestring (entry 'verified_datetime row))
  :resulting-section-name (entry 'resulting_section_name row)
  :resulting-section-id (entry 'resulting_section_id row)
  :method-name (entry 'method_name row)
  :method-id (entry 'method_id row)
  :order-procedure-name (entry 'order_proc_name row)
  :order-procedure-code (entry 'order_proc_code row)
  :test-name (entry 'test_name row)
  :test-id (entry 'test_id row)
  :component-name (entry 'component_name row)
  :component-id (entry 'component_id row)
  :delta-yn (entry 'delta_yn row)
  :result (entry 'result row)
  :component-units (entry 'component_units row)
  :component-normal-low (entry 'component_nrml_lo row)
  :component-normal-high (entry 'component_nrml_hi row)
  :component-comment (entry 'component_cmt row)
  :test-internal-comment (entry 'tst_int_comm_string row)
  :test-external-comment (entry 'tst_ext_comm_string row)
  :resulting-user (entry 'resulting_user row)
  :verified-user (entry 'verified_user row)
  :collected-to-verified (entry 'coll_to_ver row)
  :received-to-verified (entry 'recv_to_ver row)
  :addon-to-verify (entry 'adon_to_ver row)
  :submitter-name (entry 'submitter_name row)
  :submitter-id (entry 'submitter_id row)
  :ordering-department-name (entry 'ordering_department_name row)
  :ordering-department-id (entry 'ordering_department_id row)
  :encounter-department-name (entry 'encounter_department_name row)
  :encounter-department-id (entry 'encounter_department_id row)
  :mrn (entry 'pat_mrn_id row)
  :age (handler-parse-number (entry 'age row))
  :visit-type (entry 'visit_type row)
  :result-status (entry 'result_status row)
  :authorizing-provider-name (entry 'authorizing_prov_name row)
  :authorizing-provider-id (entry 'authorizing_prov_id row))

;;; Sample table instance
(definstance make-sample 'sample *sample-ht*
  :accession (entry 'accession row)
  :location (entry 'location row)
  :client (entry 'client row)
  :ordered-procedure (entry 'ordered_procedure row)
  :encounter (entry 'encounter row)
  :mrn (entry 'mrn row)
  :non-patient-name (entry 'non-patient_name row)
  :original-ordered-datetime (handler-parse-timestring (entry 'orig_order_datetime row))
  :collection-datetime (handler-parse-timestring (entry 'collection_datetime row))
  :received-datetime (handler-parse-timestring (entry 'received_datetime row))
  :priority (entry 'priority row)
  :specimen-drawn-by  (entry 'specimen_drawn_by row)
  :specimen-type  (entry 'specimen_type row)
  :encounter-type  (entry 'encounter_type row))

;;; Price table instance
(definstance make-price 'price *price-ht*
  :test-name (entry 'test_name row)
  :test-id (entry 'test_id row)
  :price (handler-parse-number (entry 'price row)))
