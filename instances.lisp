;;;; make-instances.lisp

(in-package #:beaker)

(defparameter *provider-file*
  (merge-pathnames *data-repository* "DH_Physician_Extract.csv"))
(defparameter *patient-file*
  (merge-pathnames *data-repository* "DH_Patient_Extract.csv"))

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
