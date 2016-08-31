;;; file view-classes.lisp
;;; Definitions for the lab data-mart view-classes.

(in-package #:beaker)

;;; Patient class
(clsql:def-view-class patient ()
  ((mrn :db-kind :key :db-constraints :not-null :initarg :mrn
        :type integer :accessor mrn)
   (dob :initarg :dob :type string :accessor dob)
   (sex :initarg :sex :type string :accessor sex))
  (:base-table |patient|))

;;; Provider class
(clsql:def-view-class provider ()
  ((id :db-kind :key :db-constraints :not-null :initarg :id
       :type integer :accessor name)
   (name :initarg :name :type string :accessor name)
   (line :db-kind :key :db-constraints :not-null :initarg :line
         :type integer :accessor line)
   ;; use location as a surrogate for specialty
   (specialty :initarg :specialty :type string :accessor specialty))
  (:base-table |provider|))

;;; Result super-class
(clsql:def-view-class result ()
  ((accession :db-kind :key :db-constraints :not-null :initarg :accession
                    :type string :accessor accession)
   (ordered-datetime :initarg :ordered-datetime :type clsql:wall-time
                     :accessor ordered-datetime)
   (verified-datetime :initarg :verified-datetime :type clsql:wall-time
                      :accessor verified-datetime)
   (resulting-section-name :initarg :resulting-section-name :type string
                           :accessor resulting-section-name)
   (resulting-section-id :initarg :resulting-section-id :type string
                       :accessor resulting-section-id)
   (method-name :initarg :method-name :type string :accessor method-name)
   (method-id :initarg :method-id :type string :accessor method-id)
   (order-procedure-name :initarg :order-procedure-name :type string
                         :accessor order-procedure-name)
   (order-procedure-code :initarg :order-procedure-code :type string
                         :accessor order-procedure-code)
   (test-name :initarg :test-name :type string :accessor test-name)
   (test-id :initarg :test-id :type string :accessor test-id)
   (component-name :initarg :component-name :type string
                   :accessor component-name)
   (component-id :db-kind :key :db-constraints :not-null :initarg :component-id
                 :type string :accessor component-id)
   (delta-yn :initarg :delta-yn :type string :accessor delta-yn)
   (result :initarg :result :type string :accessor result)
   (component-units :initarg :component-units :type string
                    :accessor component-units)
   (component-normal-low :initarg :component-normal-low :type string
                         :accessor component-normal-low)
   (component-normal-high :initarg :component-normal-high :type string
                          :accessor component-normal-high)
   (component-comment :initarg :component-comment :type string
                      :accessor component-comment)
   (test-internal-comment :initarg :test-internal-comment :type string
                          :accessor test-internal-comment)
   (test-external-comment :initarg :test-external-comment :type string
                          :accessor test-external-comment)
   (resulting-user :initarg :resulting-user :type string
                   :accessor resulting-user)
   (verified-user :initarg :verified-user :type string
                  :accessor verified-user)
   (collected-to-verified :initarg :collect-to-verify :type string
                          :accessor collect-to-verify)
   (received-to-verified :initarg :received-to-verified :type string
                         :accessor received-to-verified)
   (addon-to-verify :initarg :addon-to-verify :type string
                    :accessor addon-to-verify)
   (submitter-name :initarg :submitter-name :type string
                   :accessor submitter-name)
   (submitter-id :initarg :submitter-id :type string
                 :accessor submitter-id)
   (ordering-department-name :initarg :ordering-department-name :type string
                             :accessor ordering-department-name)
   (ordering-department-id :initarg :ordering-department-id :type string
                           :accessor ordering-department-id)
   (encounter-department-name :initarg :encounter-department-name :type string
                              :accessor encounter-department-name)
   (encounter-department-id :initarg :encounter-department-id :type string
                            :accessor encounter-department-id)
   (mrn :initarg :mrn :type string :accessor mrn)
   (age :initarg :age :type float :accessor age)
   (visit-type :initarg :visit-type :type string :accessor visit-type)
   (result-status :initarg :result-status :type string
                  :accessor result-status)
   (authorizing-provider-name :initarg :authorizing-provider-name :type string
                              :accessor authorizing-provider-name)
   (authorizing-provider-id :initarg :authorizing-provider-id :type string
                            :accessor authorizing-provider-id))
  (:base-table |result|))

;;; Sample class
(clsql:def-view-class sample ()
  ((accession
    :db-kind :key :db-constraints :not-null :initarg :accession
    :type string :accessor accession)
   (location :initarg :location :type string :accessor location)
   (client :initarg :client :type string :accessor client)
   (ordered-procedure :initarg :ordered-procedure :type string
                      :accessor ordered-procedure)
   (encounter :initarg :encounter :type string :accessor encounter)
   (mrn :initarg :mrn :type string :accessor mrn)
   (non-patient-name :initarg :non-patient-name :type string
                     :accessor non-patient-name)
   (original-order-datetime :initarg :original-ordered-datetime
                            :type clsql:wall-time :accessor ordered-procedure)
   (collection-datetime :initarg :collection-datetime :type clsql:wall-time
                        :accessor collection-datetime)
   (received-datetime :initarg :received-datetime :type clsql:wall-time
                      :accessor received-datetime)
   (priority :initarg :priority :type string :accessor priority)
   (specimen-drawn-by :initarg :specimen-drawn-by :type string
                      :accessor specimen-drawn-by)
   (specimen-type :initarg :specimen-type :type string
                  :accessor specimen-type)
   (encounter-type :initarg :encounter :type string
                   :accessor encounter-type))
  (:base-table |sample|))

;;; Price class
(clsql:def-view-class price ()
  ((test-name :initarg :test-name :type string :accessor test-name)
   (test-id :db-kind :key :db-constraints :not-null :initarg :test-id
            :type string :accessor test-id)
   (price :initarg :price :type float :accessor price))
  (:base-table |price|))
