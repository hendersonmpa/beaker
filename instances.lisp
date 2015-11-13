;;; file instances.lisp
;;; Functions and macros for creating instances from csv files.

(in-package #:beaker)

(defparameter *data-repository* "~/CHEO/LIS/data_mart/")
(defparameter *test-file*
  (merge-pathnames *data-repository*
                   "DH_Physician_Extract.csv"))
(defparameter *provider-file*
  (merge-pathnames *data-repository* "DH_Physician_Extract.csv"))
(defparameter *patient-file*
  (merge-pathnames *data-repository* "DH_Patient_Extract.csv"))
(defparameter *result-file*
  (merge-pathnames *data-repository* "DH_Results_Extract.csv"))
(defparameter *sample-file*
  (merge-pathnames *data-repository* "DH_Samples_Extract.csv"))

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

(defun test (iterations function file)
  (time (dotimes (i iterations)
          (funcall function file))))

;;; Provider instance closure
;; (let ((ht (create-index-hash *provider-file*)))
;;   (defun make-provider (row)
;;     (flet ((entry (key row)  ;ht closure
;;              (svref row (gethash key ht))))
;;       (make-instance 'provider
;;                      :name (entry 'prov_name row)
;;                      :id (parse-integer (entry 'prov_id row))
;;                      :line (parse-integer (entry 'line row))
;;                      :speciality (entry 'prov_specialty row)))))

;; (let ((ht (create-index-hash *patient-file*)))
;;   (defun make-patient (row)
;;     (flet ((entry (key row)  ;ht closure
;;              (svref row (gethash key ht))))
;;       (make-instance 'patient
;;                      :mrn (parse-integer (entry 'pat_mrn_id row))
;;                      :dob (entry 'pat_dob row)
;;                      :sex (entry 'pat_sex row)))))

(defmacro definstance (name class ht &body body)
  "Return functions to make instances using the hash-table"
  (let ((gen-row (gensym "ROW"))
        (gen-key (gensym "KEY")))
    `(defun ,name (row)
       (flet ((entry (,gen-key ,gen-row)  ;ht closure
                (svref ,gen-row (gethash ,gen-key ,ht))))
         (make-instance ,class
                        ,@body)))))

;;; Provider table instance
(definstance make-provider 'provider (create-index-hash *provider-file*)
  :name (entry 'prov_name row)
  :id (parse-integer (entry 'prov_id row))
  :line (parse-integer (entry 'line row))
  :speciality (entry 'prov_specialty row))

;;; Patient table instance
(definstance make-patient 'patient (create-index-hash *patient-file*)
  :mrn (parse-integer (entry 'pat_mrn_id row))
  :dob (entry 'pat_dob row)
  :sex (entry 'pat_sex row))

;;; Result table instance
(definstance make-result 'result (create-index-hash *result-file*)
  :specimen-number (entry 'specimen_number row)
  :ordered-datetime (entry 'ordered_datetime row)
  :verified-datetime (entry 'verified_datetime row)
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
  :component-normal-low (entry 'component_normal_lo row)
  :component-normal-high (entry 'component_normal_hi row)
  :component-comment (entry 'component_cmt row)
  :test-internal-comment (entry 'tst_int_comm_string row)
  :test-external-comment (entry 'test_ext_comm_string row)
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
  :age (entry 'age row)
  :visit-type (entry 'visit_type row)
  :result-status (entry 'result_status row)
  :authorizing-provider-name (entry 'authorizing_prov_name row)
  :authorizing-provider-id (entry 'authorizing_prov_id row))


;;; Sample table instance
(definstance make-sample 'sample (create-index-hash *sample-file*)
  :accession (entry 'accession row)
  :location (entry 'location row)
  :client (entry 'client row)
  :ordered-procedure (entry 'ordered_procedure row)
  :encounter (entry 'encounter row)
  :mrn (entry 'mrn row)
  :non-patient-name (entry 'non_patient_name row)
  :original-ordered-datetime (entry 'original_order_datetime row)
  :collection-datetime (entry 'collection_datetime row)
  :received-datetime (entry 'received_datetime row)
  :priority (entry 'priority row)
  :specimen-drawn-by  (entry 'specimen_drawn_by row)
  :specimen-type  (entry 'specimen_type row)
  :encounter-type  (entry 'encounter_type row)


  )
