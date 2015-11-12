;;;; objects.lisp



(in-package #:beaker)
(defparameter *data-repository* "~/CHEO/LIS/data_mart/")
(defparameter *test-file*
  (merge-pathnames *data-repository*
                   "DH_Physician_Extract.csv"))

;;; Utilities for creating instances from csv rows
;;; Build a hashtable with keys: column names and entries: column index.
(defun create-index-hash (file)
  "Use the first row in the csv as the column-names.
 Intern the column-names and use as keys in an indexed hash table"
  (flet ((get-column-names (file)
           (cl-csv:read-csv-row file
                                :separator #\|
                                :unquoted-empty-string-is-nil t))
         (replace-spaces (str) (cl-ppcre:regex-replace-all "\\s+" str "-")))
    (let* ((column-names (get-column-names file))
           (clean-column-names (mapcar #'intern
                                       (mapcar #'replace-spaces column-names)))
           (ht (make-hash-table)) ; :test #'equal if not interned
          (index 0))
      (dolist (key clean-column-names ht)
        (setf (gethash key ht) index)
        (incf (the fixnum index))))))

(defun print-hash (hashtable)
  (maphash #'(lambda (k v) (format t "key ~S, value ~A~%" k v)) hashtable))

(defparameter *column-name-hash* (create-index-hash *test-file*)
  "A hashtable with column names as keys and the index as values")

;;; TODO: Remove global variables in entry, try a closure or macro
;;; to create local state for the hash-table
(defun entry (key row-vector &key (ht *column-name-hash*))
  (let ((index (gethash key ht)))
    (svref row-vector index)))

(defun list-to-array (list)
  (make-array (length list)
              :initial-contents list))


;;; Utilities for parsing

(defun make-datetime (datetime-list)
  "Convert ?m-?d-YYYY and ?h:?m:ss to ISO date-time format YYYY-MM-DD HH:MM:SS.SSS."
  (flet ((iso-date (date)
           (ppcre:register-groups-bind
               ((#'parse-integer month) (#'parse-integer day) (#'parse-integer year))
               ("(\\d{1,2})/(\\d{1,2})/(\\d{4})" date)
             (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))
         (iso-time (time)
           (ppcre:register-groups-bind
               ((#'parse-integer hours) (#'parse-integer minutes) (#'parse-integer seconds))
               ("(\\d{1,2}):(\\d{1,2}):(\\d{2})" time)
             (format nil "~2,'0d:~2,'0d:~2,'0d.000" hours minutes seconds))))
    (let ((date (iso-date (first datetime-list)))
          (time (iso-time (second datetime-list))))
      (concatenate 'string date " " time))))

(defun minutes-float (time-string)
  "convert HH:MM:SS to a minutes float"
  (ppcre:register-groups-bind
      ((#'parse-integer hours) (#'parse-integer minutes) (#'parse-integer seconds))
      ("(\\d+):(\\d{2}):(\\d{2})" time-string)
    (float (+ (* hours 60)
              minutes
              (/ seconds 60)))))

(defun age-string-num (age-string)
  "TODO Convert an age string to a years float"
  (ppcre:register-groups-bind
      ((#'parse-integer number) units)
      ("(\\d{1,3}) (\\w{5,6})" age-string)
    (float (cond ((equalp units "years") number)
                 ((equalp units "months")
                  (/ number 12))
                 (t 0)))))