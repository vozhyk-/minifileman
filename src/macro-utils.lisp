;;;; macro-utils.lisp

(in-package #:minifileman.macro-utils)

(defun as-keyword (symbol)
  (intern (symbol-name symbol) '#:keyword))

(defmacro destructure-define-args ((&rest names) args &body body)
  `(let ,(loop for n in names collect `(,n (second (assoc (as-keyword ',n) ,args :test #'equal))))
     ,@body))
