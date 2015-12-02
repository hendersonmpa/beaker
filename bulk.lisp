;;; bulk.lisp
;;; Bulk upload from csv files.

(in-package #:beaker)
(defparameter *absolute-namestring* "/home/mpah/CHEO/LIS/data_mart/")

(defun split-file (filename dir)
  "create dir, split files, return dir path (split-file /home/mpah/CHEO/LIS/data_mart/DH_Samples_Extract_Yearly.csv sample/)"
  ;; (split-file "DH_Results_Extract_Yearly1.csv" "results/"
  (let ((split-dir (concatenate 'string *absolute-namestring* dir))
        (file-path (concatenate 'string *absolute-namestring* filename)))
    (ensure-directories-exist split-dir)
    (uiop:run-program (list "/usr/bin/split" "-l 100000" file-path split-dir) :output t)))

(defun load-split-files (instance-maker dir &optional (db-connection '("localhost" "lab" "root" "smjh13oo")))
  "Load the split files"
  ;; (load-split-files  #'make-sample (merge-pathnames "sample/" *data-repository* ))
  (labels ((collectp  (dir) (declare (ignore dir)) t)
           (recursep  (dir) (declare (ignore dir)) t)
           (object-loader (row )
             (clsql:with-database (db db-connection :database-type :mysql :pool T)
               (let ((object (funcall instance-maker row)))
                 ;; TODO: I should handle-bind instead of ignoring errors
                 (ignore-errors (clsql:update-records-from-instance object :database db)))))
           (process-csv (pathname)
             (cl-csv:read-csv pathname
                              :row-fn #'object-loader
                              :skip-first-p nil
                              :separator #\|
                              :quote nil ;; there are quotes in comment strings
                              :unquoted-empty-string-is-nil t))
           (collector (dir)
             (let ((channel (lparallel:make-channel)))
               (loop :for (a b c d) :on (uiop:directory-files dir)
                  :by #'cddddr ;; move along 4 spots
                  :do (progn
                        (format t "~@{~A~%~}" a b c d)
                        (when a (lparallel:submit-task channel
                                                     (lambda ()
                                                       (process-csv a))))
                        (when b (lparallel:submit-task channel
                                                     (lambda ()
                                                       (process-csv b))))
                        (when c (lparallel:submit-task channel
                                                     (lambda ()
                                                       (process-csv c))))
                        (when d (lparallel:submit-task channel
                                                     (lambda ()
                                                       (process-csv d))))
                        (when a (lparallel:receive-result channel))
                        (when b (lparallel:receive-result channel))
                        (when c (lparallel:receive-result channel))
                        (when d (lparallel:receive-result channel)))))))
    (uiop:collect-sub*directories dir #'collectp #'recursep #'collector)))

;; (defun upload-provider ()
;;   (clsql:with-database (db'("localhost" "lab" "root" "smjh13oo") :database-type :mysql)
;;     (let ((statement "load data local infile '/home/mpah/CHEO/LIS/data_mart/DH_Physician_Extract.csv' into table provider
;; fields terminated by '|'
;; lines terminated by '\n'
;; ignore 1 lines"))
;;       (clsql:query statement :database db))))

(loop :for (a b c d) :on (uiop:directory-files "/home/mpah/CHEO/LIS/data_mart/result/")
   :by #'cddddr
   :do (format t "~@{~A~%~}" a b c d))
