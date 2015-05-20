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
   (imprecision
    :initarg :imprecision
    :type string
    :accessor imprecision)
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

(defun analyte-object (row)
  "Make analyte table from currently fictional data-set"
  (make-instance 'analyte
               :test (first row)
               :units (second row)
               :amr-low (third row)
               :amr-hi (fourth row)
               :imprecision (make-datetime (subseq row 4 6))
               :allowable-error (make-datetime (subseq row 6 8))
               :instrument (make-datetime (subseq row 8 10))
               :l-code (minutes-float (nth 17 row))
               :cost (nth 23 row)))


(defun create-analyte-table (&optional (location *data-repository*)
                        (name "beaker.sqlite"))
  (let ((db-connection (list (concatenate 'string location name))))
    (if (probe-file (merge-pathnames location name)) ;; only useful for sqlite
        (print "Database file exists")
        (clsql:with-database (db db-connection
                                 :database-type :sqlite3)
          (clsql:create-view-from-class 'analyte :database db)))))
