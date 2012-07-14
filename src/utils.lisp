(in-package #:minifileman.utils)

(defun last1 (list)
  (first (last list)))

(defun less-if (pred)
  #l(and (funcall pred !1)
         (not (funcall pred !2))))

(defun ask-read-line (question &optional (stream *query-io*))
  (format stream "~a: " question)
  (force-output stream)
  (read-line stream nil ""))

#|
(defmacro with-restarts ((&rest names) &body body)
  `(restart-bind ,(loop for n in names collect `(,n #',n))
     ,@body))
|#

#|
(defun alist->hash (alist &key (test 'equal))
  (let ((hash (make-hash-table :test test)))
    (loop for (key . value) in alist
       do (setf (gethash key hash) value)
       finally (return hash))))
|#

#|
(defun skip-keyword-args (args)
  (loop
     for result = args then (cddr result)
     while (and result (keywordp (first result)))
     finally (return result)))
|#

(defun multi-level-sort (seq levels &key (key #'identity))
  (iter
    (initially (setf result seq))
    (for p :in (reverse levels))
    (for result := (stable-sort result p :key key))
    (finally (return result))))

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

(defun ordinary-lambda-list-vars (lambda-list)
  (multiple-value-bind (req opt rest key) (parse-ordinary-lambda-list lambda-list)
    (append req (mapcar #'first opt) (mklist rest) (mapcar #'cadar key))))

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

(defmacro _f2 (op arg place &rest args)
  (with-gensyms (place-var arg1 args-var)
    `(_f (lambda (,place-var ,arg1 &rest ,args-var)
           (apply #',op ,arg1 ,place-var ,args-var))
         ,place ,arg ,@args)))
