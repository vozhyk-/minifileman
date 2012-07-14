;;;; ==== Safe alist functions (ignoring non-lists) ====

(in-package #:minifileman.config)

(defun safe-assoc-test (key test)
  #'(lambda (x)
      (and (listp x)
           (let ((f (first x)))
             (funcall test
                      f
                      key)))))

(defun safe-assoc (key alist &key (test #'equal))
  (find-if (safe-assoc-test key test)
           alist))

;;; from kmrcl/lists.lisp
(defun safe-get-alist (key alist &key (test #'equal))
  (cdr (safe-assoc key alist :test test)))

;;; from kmrcl/lists.lisp
(defmacro safe-update-alist (akey value alist &key (test '#'eql)) ; change
  "Macro to support below (setf safe-get-alist)"
  (let ((elem (gensym "ELEM-"))
        (val (gensym "VAL-")))
    `(let ((,elem (safe-assoc ,akey ,alist :test ,test)) ; change
           (,val ,value))
       (cond
        (,elem
         (setf (cdr ,elem) ,val))
        (,alist
         (setf (cdr (last ,alist)) (list (cons ,akey ,val))))
        (t
         (setf ,alist (list (cons ,akey ,val)))))
       ,alist)))

;;; from kmrcl/lists.lisp
(defun (setf safe-get-alist) (new-val key alist &key (test #'equal))
  (safe-update-alist key new-val alist :test test)
  new-val)

(defun safe-remove-alist (key alist &key (test #'equal))
  (remove-if (safe-assoc-test key test) alist))
