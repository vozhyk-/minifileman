(in-package #:minifileman.config)

(defparameter *default-config-path*
  #+unix
   (expand-pathname #P"~/.minifileman/minifileman.conf")
  #+win32 ; 64?
   #P"C:\\minifileman-conf.txt"
  #-(or unix win32)
   (error "Not implemented"))

;(defparameter *default-config-hash* (make-hash-table :test 'equal))

(defparameter *config-defaults*
  '())

(defparameter *config-dependencies* nil)

(defparameter *silent-set-to-default* nil)

;(defvar *config-key-unsets* nil)

;(defun current-config-key-unset ()
;  (first *config-key-unsets*))

(defvar *config*)

(defclass config ()
  ((hash
    :initarg :defaults-alist
    :initform (make-hash-table :test 'equal) ;*config-defaults*
    :accessor hash
    :documentation "The hash to hold the configuration")
   (path
    :initarg :path
    :initform *default-config-path*
    :accessor path
    :documentation "Path to the config file"))
;   (config-file-stream
;    :initform nil
;    :accessor config-file-stream
;    :documentation "The stream of the config file"))
  (:documentation
"Config class. Reads from file on initialization. Cannot hold non-string keys
 or values.")) ;!!

(defmacro define-config ((&rest keys) &body args)
  `(progn
     ,@(iter
         (for key-spec :in keys)
         (destructuring-bind (name &key depends-on default) key-spec
           (when default
             (collect (cons name default) :into defaults))
           (when depends-on
             (collect `(cons ,name ,depends-on) :into deps)))
         (finally
          (return
            `((defparameter *config-defaults* ',defaults)
              (defparameter *config-dependencies* (list ,@deps))))))
     ,(destructure-define-args (default-path) args
	(when default-path `(defparameter *default-config-path* ,@default-path)))
     nil))

(defmethod initialize-instance :after ((config config) &key)
  ;(setf (hash config) (alist->hash (hash config)))
  (read-config :config config))

(defun config (key &key (config *config*) raw-value with-unset)
  "Gets the value of a config parameter"
  (multiple-value-bind (value present) (gethash key (hash config))
    (values (cond
	      (raw-value value)
	      ((and with-unset (not present)) :unset)
	      ((string= value "no") nil)
	      ((string= value "yes") t)
	      (t value))
	    present)))

(defun set-p (key &optional (config *config*))
"Returns T if the config parameter exists and its value is not an empty string"
  (let ((value (gethash key (hash config))))
    (and
     value
     (not (string= value "")))))

(defgeneric (setf config) (value key &key config raw-value)
  (:documentation "Sets the value of a config parameter"))

(defmethod (setf config) (value
			  key &key (config *config*) raw-value)
  (declare (ignore raw-value))
  (setf (gethash key (hash config)) value))

(defmethod (setf config) ((value (eql t))
			  key &key (config *config*) raw-value)
  (if (not raw-value)
      (call-next-method "yes" key :config config)
      (call-next-method)))

(defmethod (setf config) ((value (eql nil))
			  key &key (config *config*) raw-value)
  (if (not raw-value)
      (call-next-method "no" key :config config)
      (call-next-method)))

(defun default (key)
  (get-alist key *config-defaults* :test #'equal))

(defun set-to-default (key &optional (config *config*))
  (setf (config key :config config) (default key)))

(defun remconfig (key &optional (config *config*))
  "Remove a parameter from config"
  (remhash key (hash config)))

(defun clear-config (&optional (config *config*))
  "Remove all parameters from the config"
  (clrhash (hash config)))
(defun add-line (line &optional (config *config*))
  "Add a config entry in the form
  \"<variable> <value>\"
 or
  \"<comment>\"
to the config"
  (let-if (space-pos (position #\Space line))
    (setf (config (subseq line 0 space-pos) :config config)
          (subseq line (1+ space-pos)))
    (add-comment line config)))

(defun add-comment (comment &optional (config *config*))
  "Add a comment to the config"
  (setf (config comment :config config :raw-value t) nil))

(defun read-config (&key (config *config*) (path (path config)))
  "Read the config from file, replacing existing entries"
  (when (and path (not (equal path "")))
    (with-open-file (file path :if-does-not-exist nil)
      (when file
	(loop for line = (read-line file nil nil) while line
	   do (add-line line config))))))

(defmacro doconfig ((key value &optional (config '*config*)) &body body)
  `(iter
     (for (,key ,value) :in-hashtable (hash ,config))
     ,@body))

#|
(defun print-missing-parameter (condition stream)
  (bind (((:struct missing-parameter- config config-name key) condition))
    (format stream "``~a'' is unset in ~a(~a)"
	    key config config-name)))
|#

(define-condition missing-parameter (error)
  ((config-name :initarg :config-name :accessor missing-parameter-config-name)
   (config :initarg :config :accessor missing-parameter-config)
   (key :initarg :key :accessor missing-parameter-key)
   (value :initarg :value :accessor missing-parameter-value))
  (:report (lambda (condition stream)
	     (bind (((:struct missing-parameter- config config-name key) condition))
	       (format stream "``~a'' is unset in ~a(~a)~%~
                               *silent-set-to-default* => ~a"
		       key config config-name *silent-set-to-default*)
	       (format stream "~&~a" (find-restart 'store-default))))))

(defun missing-parameter (&key
			 (config-name '*config*)
			 (config (symbol-value config-name))
			 key
			 (value (config key :config config :with-unset t)))
  (labels ((store-default ()
	     (store-value (default key))))
    (restart-bind ((store-default #'store-default
		     :report-function (formatter "Set the config variable to default"))
		   (store-value #'(lambda (value)
				    (setf (config key :config config) value)
				    (continue))
		     :interactive-function #'(lambda () (list (ask-read-line "Enter the new value for the config variable")))
		     :report-function (formatter "Set the config variable to a given value."))
		   #|(continue #'(lambda () nil))|#)
      (error 'missing-parameter
	     :config-name config-name
	     :config config
	     :key key
	     :value value))))

(defun use-to-check ()
  (mapcar #'first *config-dependencies*))

(defun check-key (key &optional (config *config*))
  (when (not (set-p key config))
    (missing-parameter :key key
		       :config config
		       :value (config key :config config :with-unset t))))

(defun keys-to-check (use &optional (config *config*))
  (let ((dep-fun (get-alist use *config-dependencies* :test #'equal)))
    (mklist (funcall dep-fun (config use :config config)))))

(defun check-use (use &optional (config *config*))
  (let ((set-p (set-p use config)))
    (check-key use config)
    (let ((*silent-set-to-default*
	   (if (not set-p) t *silent-set-to-default*)))
      (labels ((check (list &aux (key (first list)))
		 (when list
		   (restart-case (check-key key config)
		     (continue () (check (rest list)))))))
	(check (keys-to-check use config))))))
;      (dolist (key (keys-to-check use config))
;	(check-key key config)))))

(defun check-config (&optional (config *config*))
  #|(dolist (key *keys-to-check*)
    (check-key key config))|#
  (handler-bind ((missing-parameter #'(lambda (c)
				       (if *silent-set-to-default*
					 (store-default)
					 (error c)))))
    (dolist (use (use-to-check))
      (check-use use config))))

(defun print-config (&key (config *config*)
		          (stream *standard-output*)
		          (pretty t))
  "Print the config to stream"
  (doconfig (key value config)
    (if value
      (format stream "~a~2@*~:[ ~;~35t~]~1@*~a~%" key value pretty)
      (write-line key stream)))
  (force-output stream))

(defun write-config (&key (config *config*) (path (path config)))
  "Write the config to file"
  (with-open-file (file path
			:direction :output
			:if-does-not-exist :create
			:if-exists :supersede)
    (print-config :config config :stream file :pretty nil)))
