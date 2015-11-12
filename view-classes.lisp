;;;; beaker.lisp

(in-package #:beaker)

(clsql:def-view-class patient ()
  ((idpatient :db-kind :key :db-constraints :not-null :initarg :idpatient
    :type int :accessor idpatient)
   (mrn :initarg :mrn :type '(string 45) :accessor mrn)
   (dob :initarg :dob :nulls-ok t :type string :accessor dob)
   (sex :initarg :sex :type (string 45) :accessor sex)))

(clsql:def-view-class provider ()
  ((id :db-kind :key :db-constraints :not-null :initarg :id
       :type integer :accessor name)
   (name :initarg :name :type (string 45) :accessor name)

   (line :initarg :line :type integer :accessor line)
   ;; use location as a surrogate for specialty
   (specialty :initarg :specialty :type (string 45) :accessor specialty))
  (:base-table |provider|))

(clsql:def-view-class result ()
  ((idresult :db-kind :key :db-constraints :not-null :initarg :idresult
             :type int :accessor idresult)
   (specimen_number :initarg :specimen-number :type (string 45)
                    :accessor specimen_number)
   (ordered_datetime :initarg :ordered-datetime :type datetime
                     :accessor ordered-datetime )
   (specimen-collected-datetime :initarg :specimen-collected-datetime
                                :type (string 45) :accessor specimen-collected-datetime)
   (specimen-received-datetime :initarg :specimen-received-datetime
                               :type (string 45) :accessor specimen-received-datetime)
   (last-received-datetime :initarg :last-received-datetime
                               :type (string 45) :accessor last-received-datetime)
   (verified_datetime :initarg :verified-datetime :type datetime
                     :accessor verified-datetime )
   (resulting-section-name :initarg :resulting-section-name
                           :type (string 45) :accessor resulting-section-name)
   (resulting-section-id :initarg :resulting-section-id
                           :type (string 45) :accessor resulting-section-id)
   (method-name :initarg :method-name :type (string 45) :accessor method-name)
   (method-id :initarg :method-id :type (string 45) :accessor method-id)
   (order-procedure-name :initarg :order-procedure-name :type (string 45)
                         :accessor order-procedure-name)
   (order-procedure-code :initarg :order-procedure-code :type (string 45)
                         :accessor order-procedure-code)
   (test-name :initarg :test-name :type (string 45) :accessor test-name)
   (test-id :initarg :test-id :type (string 45) :accessor test-id)
   (component-name :initarg :component-name :type (string 45) :accessor component-name)
   (component-id :initarg :component-id :type (string 45) :accessor component-id)
   (delta-yn :initarg :delta-yn :type (string 45) :accessor delta-yn)
   (result :initarg :result :type (string 45) :accessor result)
   (component_units :initarg :component_units :type (string 45) :accessor component_units)
   (component_normal_low :initarg :component_normal_low :type (string 45)
                         :accessor component_normal_low)
   (component_normal_high :initarg :component_normal_high :type (string 45)
                          :accessor component_normal_high)
   (component_comment :initarg :component_comment :type (string 45)
                      :accessor component_comment)
   (test_internal_comment :initarg :test_internal_comment:type (string 45)
                          :accessor test_internal_comment)
   (test_external_comment :initarg :test_external_comment:type (string 45)
                          :accessor test_external_comment)
   (resulting_user :initarg :resulting_user :type (string 45) :accessor resulting_user)
   (verified_user :initarg :verified_user :type (string 45) :accessor verified_user)


   (specimen-number :initarg :specimen-number
    :type (string 45)
    :accessor specimen-number)
   (verified-datetime
    :initarg :verified-datetime
    :type (string 45)
    :accessor verified-datetime)
   (comment
    :initarg :comment
    :type (string 45)
    :accessor comment)
   (tat
    :initarg :tat
    :type float
    :accessor tat)
   (age
    :initarg :age
    :type float
    :accessor age)
   (lower-ri
    :initarg :lower-ri
    :nulls-ok t
    :type (string 45)
    :accessor lower-ri)
   (upper-ri
    :initarg :upper-ri
    :nulls-ok t
    :type (string 45)
    :accessor upper-ri)
   (resulting-user
    :initarg :user
    :type (string 45)
    :accessor user)
   (rhlc-flags
    :initarg :rhlc-flags
    :type (string 45)
    :accessor rhlc-flags)))

;;(clsql:def-view-class hematology (biochemistry))

(clsql:def-view-class sample ()
  ((specimen-number
    ;; :db-kind :key
    ;; :db-constraints :not-null
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
