;;; load.lisp
;;; upload csv file to the database.

(in-package #:beaker)

(setf lparallel:*kernel* (lparallel:make-kernel 8))

(let ((computer-name (machine-instance)))
  (cond ((string-equal computer-name "hex")
         (defparameter *absolute-data-repository* "/media/mpah/data/Beaker_extracts/"))
        ((string-equal computer-name "detritus")
         (defparameter *absolute-data-repository* "/home/mpah/CHEO/LIS/data_mart/"))))

;;; TODO: DON'T FORGET TO CHANGE BACK TO LAB
(defparameter *db-connection* '("localhost" "lab" "root" "smjh13oo"))
(defun split-file (input-file-string  output-dir-string)
  "create dir, split files"
  ;; (split-file "DH_Results_Extract_Yearly1.csv" "results/")
  (uiop:run-program
   (list "/usr/bin/split" "-l 10000" input-file-string output-dir-string)
   :output t))

(defun load-split-files (instance-maker dir
                         &optional (db-connection *db-connection*))
  "Load the split files"
  ;; (load-split-files  #'make-sample (merge-pathnames "sample/" *data-repository*))
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

(defun load-file (file-pathname data-type
                    &optional (db-connection *db-connection*)
                       (data-repository *absolute-data-repository*))
  ;; (split-file "DH_Results_Extract_Yearly1.csv" "results/")
  ;; (load-split-files  #'make-sample (merge-pathnames "sample/" *data-repository*))
  (labels ((process-file (dir-string instance-maker)
             (let ((output-dir-path (merge-pathnames dir-string data-repository)))
               (ensure-directories-exist output-dir-path)
               (split-file (namestring file-pathname) (namestring output-dir-path))
               (remove-lines (namestring (merge-pathnames "aa" output-dir-path)) 0 1)
               (load-split-files instance-maker output-dir-path db-connection)
               (uiop:delete-directory-tree output-dir-path :validate t))))
    ;; remove split-dir and contents
    (ccase data-type
      ((patient) (process-file "patient/" #'make-patient))
      ((result) (process-file "result/"  #'make-result))
      ((sample) (process-file "sample/" #'make-sample))
      ((provider) (process-file "provider/" #'make-provider))
      ((price) (process-file "price/" #'make-price)))))

;;(load-file #P"/home/mpah/CHEO/LIS/data_mart/archive/price.psv" 'price)


(defun archive-file (old-pathname &optional (to-subdir "archive"))
  "Archive csv file"
  (let ((new-pathname
         (make-pathname :defaults old-pathname
                        :directory (append
                                    (butlast (pathname-directory old-pathname))
                                    (list to-subdir)))))
    (rename-file old-pathname new-pathname)))


(defun load-dir (&optional (dir-name "/media/mpah/data/Beaker_extracts/pending/"))
  "Upload files to db table based on regex match"
  (let ((file-list (uiop:directory-files dir-name)))
    (dolist (file-name file-list)
      (cond ((cl-ppcre:scan "DH_Results_Extract.+" (namestring file-name))
             (load-file file-name 'result))
            ((cl-ppcre:scan "DH_Patient_Extract.+" (namestring file-name))
             (load-file file-name 'patient))
            ((cl-ppcre:scan "DH_Physician_Extract.+" (namestring file-name))
             (load-file file-name 'provider))
            ((cl-ppcre:scan "DH_Samples_Extract.+" (namestring file-name))
             (load-file file-name 'sample)))
      (archive-file file-name))))
