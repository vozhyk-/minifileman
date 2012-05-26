;;;; macro-helpers.lisp -- Helpers for macros

(in-package #:org.programmingforchildren.minifileman.macro-helpers)

(defun as-keyword (symbol)
  (intern (symbol-name symbol) '#:keyword))

(defmacro with-gensyms ((&rest vars) &body body)
  `(let ,(loop for var in vars collect `(,var (gensym)))
     ,@body))

(defmacro destructure-define-args ((&rest names) args &body body)
  `(let ,(loop for n in names collect `(,n (second (assoc (as-keyword ',n) ,args :test #'equal))))
     ,@body))
