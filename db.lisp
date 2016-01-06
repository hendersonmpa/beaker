;;; db.lisp
;;; Functions for interaction with the database.

(in-package :beaker)
(require :sb-sprof)
(setf lparallel:*kernel* (lparallel:make-kernel 8))

;; Create tables from our view classes
;; Only the first time !!!!!
(defun create-schema (&optional (db-name "lab"))
  (let ((db-connection (list "localhost" db-name "root" "smjh13oo")))
    (flet ((handler-probe ()
               (handler-case
                   (clsql:probe-database db-connection :database-type :mysql)
                 (clsql-sys:sql-connection-error () nil))))
      (cond ((handler-probe)
             (clsql:destroy-database db-connection :database-type :mysql)
             (clsql:create-database db-connection :database-type :mysql))
            (t (clsql:create-database db-connection :database-type :mysql)))
      (clsql:with-database (db db-connection :database-type :mysql)
        (clsql:create-view-from-class 'patient :database db)
        (clsql:create-view-from-class 'provider :database db)
        (clsql:create-view-from-class 'result :database db)
        (clsql:create-view-from-class 'sample :database db)))))


;; (clsql:with-database (db '("localhost" "lab" "root" "smjh13oo") :database-type :mysql)
;;   (clsql:create-view-from-class 'sample :database db))

(defun csv-upload (instance-maker file &optional (db-connection '("localhost" "lab" "root" "smjh13oo")))
  "Process row into object and upload.
 Tried to make parallel, but I think that MySQL locks tables during insertion.
 INSTANCE-MAKER is a definstance function."
  (clsql:with-database (db db-connection :database-type :mysql :pool T)
    (flet ((object-loader (row)
             (let ((object (funcall instance-maker row)))
               ;; TODO: I should handle-bind instead of ignoring errors
               ;;(clsql:update-records-from-instance object :database db)
               (ignore-errors (clsql:update-records-from-instance object :database db))
               )))
      (cl-csv:read-csv file
                       :row-fn #'object-loader
                       :skip-first-p t
                       :separator #\|
                       :quote nil ;; there are quotes in comment strings
                       :unquoted-empty-string-is-nil t))))


;; (make-sample (car (cl-csv:read-csv *sample-file*
;;                                    :skip-first-p t
;;                                    :separator #\|
;;                                    :quote nil ;; there are quotes in comment strings
;;                                    :unquoted-empty-string-is-nil t)))


(defun file-chunker (file)
  (let* ((complete (cl-csv:read-csv file
                                    :skip-first-p t
                                    :separator #\|
                                    :quote nil ;; there are quotes in comment strings
                                    :unquoted-empty-string-is-nil t))
         (len (length complete))
         (chunk (floor (/ len 8)))
         (first (subseq complete 0 chunk))
         (second (subseq complete chunk (* 2 chunk)))
         (third (subseq complete (* 2 chunk) (* 3 chunk)))
         (fourth (subseq complete (* 3 chunk) (* 4 chunk))))
    (values first second third fourth)))


(defun chunk-upload (instance-maker chunk &optional (db-connection '("localhost" "lab" "root" "smjh13oo")))
  "Process row into object and upload.
  INSTANCE-MAKER is a definstance function."
  (clsql:with-database (db db-connection :database-type :mysql :pool T)
    (dolist (row chunk)
      (ignore-errors (clsql:update-records-from-instance
                      (funcall instance-maker row) :database db)))))

;;TODO: Change this to use "split -l 100000 ./split"
(defun parallel-upload (instance-maker file)
  (multiple-value-bind (a b c d) (file-chunker file)
    (let ((channel (lparallel:make-channel)))
      (lparallel:submit-task channel
                             (lambda ()
                               (chunk-upload instance-maker a)))
      (lparallel:submit-task channel
                             (lambda ()
                               (chunk-upload instance-maker b)))
      (lparallel:submit-task channel
                             (lambda ()
                               (chunk-upload instance-maker c)))
      (lparallel:submit-task channel
                             (lambda ()
                               (chunk-upload instance-maker d)))

      (list (lparallel:receive-result channel)
            (lparallel:receive-result channel)
            (lparallel:receive-result channel)
            (lparallel:receive-result channel)))))

;; (handler-bind ((clsql-sys:sql-database-data-error
;;                 #'(lambda (c)
;;                     (declare (ignore c))
;;                     (invoke-restart 'continue))))
;;   (clsql:update-records-from-instance object :database db))

;; (handler-case; error when repeat entries are attempted
;;     (clsql:update-records-from-instance object :database db)
;;   (clsql-sys:sql-database-data-error () nil))

;; (defun object-loader (row &optional (db-connection '("localhost" "lab" "root" "smjh13oo")))
;;   (let* ((row-vector (list-to-array row))
;;         (object (make-patient row-vector)))
;;     (handler-case        ; error when repeat entries are attempted
;;         (clsql:with-database (db db-connection :database-type :mysql)
;;           (clsql:update-records-from-instance object :database db))
;;       (clsql-sys:sql-database-data-error () nil))))


(defun update-database ()
  (let ((channel (lparallel:make-channel)))
    (lparallel:submit-task channel
                           (lambda ()
                             (parallel-upload #'make-provider *provider-file*)))
    (lparallel:submit-task channel
                           (lambda ()
                             (parallel-upload #'make-patient *patient-file*)))
    (lparallel:submit-task channel
                           (lambda ()
                             (parallel-upload #'make-result *result-file*)))
    (lparallel:submit-task channel
                           (lambda ()
                             (parallel-upload #'make-sample *sample-file*)))
    (list (lparallel:receive-result channel)
          (lparallel:receive-result channel)
          (lparallel:receive-result channel)
          (lparallel:receive-result channel))))

;;(clsql:connect '("localhost" "lab" "root" "smjh13oo") :database-type :mysql)
;;;(clsql:list-databases '("localhost" "lab" "root" "smjh13oo") :database-type :mysql)
;; (clsql:with-database (db '("localhost" "lab" "root" "smjh13oo")
;;                          :database-type :mysql)
;;   (clsql:create-view-from-class 'result :database db))
;;   (clsql:list-tables :owner :all :database db))
;; (clsql:create-database '("localhost" "lab" "root" "smjh13oo") :database-type :mysql)
;; (clsql:probe-database '("localhost" "lab" "root" "smjh13oo") :database-type :mysql)

;; (let ((db-connection '("localhost" "lab" "root" "smjh13oo")))
;;   (clsql:with-database (db db-connection :database-type :mysql)
;;     (clsql:select 'result :where [ = [slot-value 'result 'resulting-section-name]
;;                   "BIOCHEMISTRY"] :database db)))


;; From http://paste.lisp.org/display/25003

;; TODO: not working !!!!
(defun parallel-stream-upload (instance-maker file-path  &optional (db-connection '("localhost" "lab" "root" "smjh13oo")))
  (flet ((object-upload (str db)
           (let* ((row (mapcar (lambda (str) (string-trim '(#\Space #\Tab #\Newline #\Return) str))
                               (split-sequence:split-sequence #\| (read-line str))))
                 ;(object (funcall instance-maker row))
                  )
                                        ;(ignore-errors (clsql:update-records-from-instance object :database db))
             (format t "~S~%" row))))
    (let ((channel (lparallel:make-channel)))
      (with-open-file (str file-path :direction :input)
        (clsql:with-database (db db-connection :database-type :mysql :pool T)
          (loop
             while str
             do (handler-case
                    (progn
                      (lparallel:submit-task channel
                                             (lambda ()
                                               (object-upload str db)))
                      (lparallel:submit-task channel
                                             (lambda ()
                                               (object-upload str db)))
                      (lparallel:submit-task channel
                                             (lambda ()
                                               (object-upload str db)))
                      (lparallel:submit-task channel
                                             (lambda ()
                                               (object-upload str db)))
                      (list (lparallel:receive-result channel)
                            (lparallel:receive-result channel)
                            (lparallel:receive-result channel)
                            (lparallel:receive-result channel)))
                  (end-of-file () (return)))) :database db)))))
