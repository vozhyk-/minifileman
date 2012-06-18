(in-package #:minifileman.utils)

(defun last1 (list)
  (first (last list)))

;(defun copy-hash (hash &key (test 'equal))
;  (let ((new-hash (make-hash-table :test test)))
;    (dohash (key value hash)
;      (setf (gethash key new-hash) value))
;    new-hash))

(defun ask-read-line (question &optional (stream *query-io*))
  (format stream "~a: " question)
  (read-line stream nil ""))

#|
(defmacro with-restarts ((&rest names) &body body)
  `(restart-bind ,(loop for n in names collect `(,n #',n))
     ,@body))
|#

(defmacro with-condition-slots ((&rest slots) condition type &body body)
  `(let ,(loop for s in slots
	    collect `(,s (,(intern (concatenate 'string
						(symbol-name type)
						"-"
						(symbol-name s)))
			  ,condition)))
     ,@body))

#|
(defun alist->hash (alist &key (test 'equal))
  (let ((hash (make-hash-table :test test)))
    (loop for (key . value) in alist
       do (setf (gethash key hash) value)
       finally (return hash))))
|#

(defmacro dohash ((key value hash) &body body)
  `(loop for ,key being the hash-keys in ,hash using (hash-value ,value)
      do ,@body))

#|
(defun skip-keyword-args (args)
  (loop
     for result = args then (cddr result)
     while (and result (keywordp (first result)))
     finally (return result)))
|#

(defun remove-keyword-args (args &rest keywords)
  (let ((result (copy-tree args)))
    (dolist (kw keywords result)
      (remf result kw))))

(defun add-replace-keyword-args (args &rest keys-values)
  (loop
     with result = (copy-tree args)
     for (key value . rest) on keys-values by #'cddr
     do (setf (getf result key) value)
     finally (return result)))

(defun starts-with (beginning string)
  (and
   (>= (length string) (length beginning))
   (string=
    (subseq string 0 (length beginning))
    beginning)))

(defun positions (item sequence)
  (loop
     for pos = (position item sequence)
       then (position item sequence :start (1+ pos))
     while pos
     collect pos))

(defun nth-comp (n fun arg)
  (loop
     for result = arg then (funcall fun result)
     for i from 1 to n
     finally (return result)))

(defun class-from (name-or-class)
  (if (not (typep name-or-class 'standard-class))
      (find-class name-or-class)
      name-or-class))

(defun class-precedence-list (pcl-class)
  (closer-mop:finalize-inheritance pcl-class)
  (closer-mop:class-precedence-list pcl-class))

(defun superclass-p (superclass &optional (class nil class-supplied-p))
  (if class-supplied-p
	(find (class-from superclass)
		  (class-precedence-list (class-from class)))
	(lambda (class) (superclass-p superclass class))))

(defun append-new-superclasses (orig &rest new)
  (dolist (new-sup new orig)
    (when (not (find-if (superclass-p new-sup)
			orig))
      (pushnew new-sup orig))))

(defmacro acase (keyform &body clauses)
  `(let ((it ,keyform))
     (case it
       ,@clauses)))
