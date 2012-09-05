(in-package #:minifileman.config)

(defparameter *config-print-case* :downcase)

(defparameter *default-config-path*
  #+unix
   #P"~/.minifileman/minifileman.list"
  #+win32 ; 64?
   #P"C:\\minifileman.list"
  #-(or unix win32)
   (error "Not implemented"))

(defvar *config* nil)

(defun read-list (stream)
  (iter
    (for el := (handler-case (read stream)
                 (end-of-file () (return list))))
    (collect el :into list)))

(defun read-config (&optional (path *default-config-path*))
  (when (and path (not (equal path "")))
    (with-open-file (file path :if-does-not-exist nil)
      (when file
        (with-package #:minifileman
          (read-list file))))))

(defun write-config (&key (config *config*) (path *default-config-path*))
  (with-open-file (file path
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :supersede)
    (let ((*print-case* *config-print-case*))
      (with-package #:minifileman
        (format file "~{~s~%~^~%~}" config)))))

(defun config (path &optional (config *config*))
  (iter
    (initially (setf result config))
    (for v :in (mklist path))
    (for result := (safe-get-alist v result))
    (finally (return result))))

(defmacro configq (path &optional (config '*config*))
  `(config ',path ,config))

(defun path-nest (path val)
  (if (= (length path) 1)
    (cons (first path) val)
    (list (first path) (path-nest (rest path) val))))

(defun (setf config) (new-val path &optional (config *config*))
  (let ((path (mklist path)))
    (cond
      ((null path) config)
      ((= (length path) 1)
       (setf (safe-get-alist (first path) config) new-val))
      (t
       (bind (((first &rest rest) path))
         (symbol-macrolet ((first-config (config first config))
                           (rest-config (config rest first-config)))
           (setf first-config
                 (if (null first-config)
                   (list (path-nest rest new-val))
                   (progn
                     (setf rest-config new-val)
                     first-config)))
           new-val))))))

(defun delete-config (path &optional (config *config*))
  "Remove config variable. Can be destructive"
  (let ((path (mklist path)))
    (cond
      ((> (length path) 1)
       (let ((base (butlast path))
             (last (last1 path)))
         (_f2 safe-remove-alist
              last
              (config base config))
         config))
      (path (safe-remove-alist (first path) config))
      (t config))))

(defmacro delete-config! (path &optional (config '*config*))
  `(setf ,config (delete-config ,path ,config)))
