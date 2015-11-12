;;;; make-instances.lisp

(in-package #:beaker)

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

;;(test 250 #'create-row-vector *test-file*)

(defun make-provider (row)
  (declare (inline entry))
  (make-instance 'provider
                 :name (entry 'prov_name row)
                 :id (parse-integer (entry 'prov_id row))
                 :line (parse-integer (entry 'line row))
                 :speciality (entry 'prov_specialty row)))
