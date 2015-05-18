;;;; beaker.lisp

(in-package #:beaker)

;;; "beaker" goes here. Hacks and glory await!

;;; Data Management
;;(defparameter *test-file* "/home/mpah/lisp/site/porph-screen/data/FPORS 2014-09-04.csv")
(defparameter *data-repository* "/Users/matthew/Documents/CHEO/LIS_data/")
(defparameter *test-file*
  (merge-pathnames *data-repository*
                   "CHEO Lab Quality Indicators - Biochemistry_2015-04-01-05-50-55.csv"))

(clsql:def-view-class entry ()
  ((specimen-number
    :initarg :specimen-number
    :type string
    :accessor specimen-number)
   (mrn
    :initarg :mrn
    :type string
    :accessor mrn)
   (encounter
    :initarg :encounter
    :type string
    :accessor encounter)
   (location
    :initarg :location
    :type string
    :accessor location)
   (order-datetime
    :initarg :order-datetime
    :type string
    :accessor order-datetime)
   (collection-datetime
    :initarg :collection-datetime
    :type string
    :accessor collection-datetime)
   (received-datetime
    :initarg :received-datetime
    :type string
    :accessor received-datetime)
   (verified-datetime
    :initarg :verified-datetime
    :type string
    :accessor verified-datetime)
   (component
    :initarg :component
    :type string
    :accessor component)
   (result
    :initarg :result
    :type string
    :accessor result)
   (comment
    :initarg :comment
    :type string
    :accessor comment)
   (transit
    :initarg :transit
    :type float
    :accessor transit)
   (tat
    :initarg :tat
    :type float
    :accessor tat)
   (test
    :initarg :test
    :type string
    :accessor test)
   (age
    :initarg :age
    :type float
    :accessor age)
   (sex
    :initarg :sex
    :type string
    :accessor sex)
   (resulting-user
    :initarg :user
    :type string
    :accessor user)
   (provider
    :initarg :provider
    :type string
    :accessor provider)
   (address
    :initarg :address
    :type string
    :accessor address)
   (city
    :initarg :city
    :type string
    :accessor city)
   (postal
    :initarg :postal
    :type string
    :accessor postal)))

(clsql:def-view-class patient ()
  ((mrn
    :db-kind :key
    :db-constraints :not-null
    :initarg :mrn
    :type string
    :accessor mrn)
   (dob
    :initarg :dob
    :type float
    :accessor dob)
   (sex
    :initarg :sex
    :type string
    :accessor sex)
   (address
    :initarg :address
    :type string
    :accessor address)
   (city
    :initarg :city
    :type string
    :accessor city)
   (postal
    :initarg :postal
    :type string
    :accessor postal)))

(clsql:def-view-class physician ()
  ((provider
    :initarg :provider
    :type string
    :accessor provider)
   (location ;; surrogate for specialty
    :initarg :location
    :type string
    :accessor location)))

(clsql:def-view-class biochemistry ()
  ((verified-datetime
    :initarg :verified-datetime
    :type string
    :accessor verified-datetime)
   (component
    :initarg :component
    :type string
    :accessor component)
   (result
    :initarg :result
    :type string
    :accessor result)
   (comment
    :initarg :comment
    :type string
    :accessor comment)
   (tat
    :initarg :tat
    :type float
    :accessor tat)
   (test
    :initarg :test
    :type string
    :accessor test)
   (age
    :initarg :age
    :type float
    :accessor age)
   (lower-ri
    :initarg :lower-ri
    :type string
    :accessor lower-ri)
   (upper-ri
    :initarg :upper-ri
    :type string
    :accessor upper-ri)
   (resulting-user
    :initarg :user
    :type string
    :accessor user)
   (rhlc-flags
    :initarg :rhlc-flags
    :type string
    :accessor rhlc-flags)))

;;(clsql:def-view-class hematology (biochemistry))

(clsql:def-view-class sample ()
  ((specimen-number
    :db-kind :key
    :db-constraints :not-null
    :initarg :specimen-number
    :type string
    :accessor specimen-number)
   (mrn
    :initarg :mrn
    :type string
    :accessor mrn)
   (encounter
    :initarg :encounter
    :type string
    :accessor encounter)
   (location
    :initarg :location
    :type string
    :accessor location)
   (order-datetime
    :initarg :order-datetime
    :type string
    :accessor order-datetime)
   (collection-datetime
    :initarg :collection-datetime
    :type string
    :accessor collection-datetime)
   (received-datetime
    :initarg :received-datetime
    :type string
    :accessor received-datetime)
   (transit
    :initarg :transit
    :type float
    :accessor transit)
   (provider
    :initarg :provider
    :type string
    :accessor provider)
   (priority
    :initarg :priority
    :type string
    :accessor priority)))

(clsql:def-view-class anaylte ()
  ((test
    :db-kind :key
    :db-constraints :not-null
    :initarg :test
    :type string
    :accessor test)
   (units
    :initarg :units
    :type string
    :accessor units)
   (amr-low
    :initarg :amr-low
    :type string
    :accessor amr-low)
   (amr-high
    :initarg :amr-high
    :type string
    :accessor amr-high)
   (imprescision
    :initarg :imprescision
    :type string
    :accessor imprescision)
   (allowable-error
    :initarg :allowable-error
    :type string
    :accessor allowable-error)
   (instrument
    :initarg :instrument
    :type string
    :accessor instrument)
   (l-code
    :initarg :l-code
    :type string
    :accessor l-code)
   (cost
    :initarg :cost
    :type string
    :accessor cost)
   (delta-rule
    :initarg :delta-rule
    :type string
    :accessor delta-rule)))

;; ("Specimen Number" "MRN" "Encounter ID" "Order Location" "Order Date"
;;  "Order Time" "Collection Date" "Collection Time" "Received Date"
;;  "Received Time" "Resulted Date" "Resulted Time" "Verified Date"
;;  "Verified Time" "Component" "Result" "Component Comm" "Transit TAT(Minutes)"
;;  "TAT(Minutes)" "Test Name" "Age" "Sex" "Resulting User" "Authorizing Provider"
;;  "Patient Address 1" "Patient Address 1" "City" "Postal")

(defun make-datetime (datetime-list)
  "TODO make and ISO date-time format"
  (flet ((iso-date (date)
           (ppcre:register-groups-bind
               (month day year)
               ("(\\d{1,2})/(\\d{1,2})/(\\d{4})" date)
             (concatenate 'string year "-" month "-" day))))
    (let ((date (iso-date (first datetime-list)))
          (time (second datetime-list)))
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

(defun create-entry-object (row)
  (make-instance 'entry
                 :specimen-number (first row)
                 :mrn (second row)
                 :encounter (third row)
                 :location (fourth row)
                 :order-datetime (make-datetime (subseq row 4 6))
                 :collection-datetime (make-datetime (subseq row 6 8))
                 :received-datetime (make-datetime (subseq row 8 10))
                 :resulted-datetime (make-datetime (subseq row 10 12))
                 :verified-datetime (make-datetime (subseq row 12 14))
                 :component (nth 14 row)
                 :result (nth 15 row)
                 :comment (nth 16 row)
                 :transit (minutes-float (nth 17 row))
                 :tat (minutes-float (nth 18 row))
                 :test (nth 19 row)
                 :age (age-string-num (nth 20 row))
                 :sex (nth 21 row)
                 :resulting-user (nth 22 row)
                 :provider (nth 23 row)
                 :address (nth 24 row)
                 :city (nth 26 row)
                 :postal (nth 27 row)))

(defun create-objects (row)
  "Create objects from row of Beaker extract file"
  (make-instance 'patient
                 :mrn (second row)
                 :dob nil
                 :sex (nth 21 row)
                 :address (nth 24 row)
                 :city (nth 26 row)
                 :postal (nth 27 row))
  (make-instance 'physician
                 :provider (nth 23 row)
                 :specialty (fourth row)) ;; this is currently the location
  (make-instance 'biochemistry
                 :verified-datetime (make-datetime (subseq row 12 14))
                 :component (nth 14 row)
                 :result (nth 15 row)
                 :comment (nth 16 row)
                 :tat (minutes-float (nth 18 row))
                 :test (nth 19 row)
                 :age (age-string-num (nth 20 row))
                 :upper-ri nil
                 :lower-ri nil
                 :resulting-user (nth 22 row)
                 :rhlc-flags nil) ;; need this data
  (make-instance 'sample
                 :specimen-number (first row)
                 :mrn (second row)
                 :encounter (third row)
                 :location (fourth row)
                 :order-datetime (make-datetime (subseq row 4 6))
                 :collection-datetime (make-datetime (subseq row 6 8))
                 :received-datetime (make-datetime (subseq row 8 10))
                 :transit (minutes-float (nth 17 row))
                 :provider (nth 23 row)
                 :priority nil) ;; need this data
  )

;; Create tables from our view classes
;; Only the first time !!!!!
(defun create-tables (&optional (location *data-repository*)
                        (name "beaker.sqlite"))
  (let ((db-connection (list (concatenate 'string location name))))
    (if (probe-file (merge-pathnames location name)) ;; only useful for sqlite
        (print "Database file exists")
        (clsql:with-database (db db-connection
                                 :database-type :sqlite3)
          (clsql:create-view-from-class 'entry :database db)))))

(defun update-tables (entry-object &optional (location *data-repository*)
                                     (name "beaker.sqlite"))
  "Update database with entries"
  (let ((db-connection (list (concatenate 'string location name))))
    (clsql:with-database (db db-connection
                             :database-type :sqlite3)
      (clsql:update-records-from-instance entry-object :database db))))

(defun add-entry (row)
  (let ((entry-object (create-entry-object row)))
    (update-tables entry-object)))

(defun update-database ()
  (cl-csv:read-csv *test-file*
                   :map-fn #'add-entry
                   :skip-first-p t
                   :unquoted-empty-string-is-nil t))
