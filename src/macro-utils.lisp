;;;; macro-utils.lisp

(in-package #:minifileman.macro-utils)

(defun as-keyword (symbol)
  (intern (symbol-name symbol) '#:keyword))

(defmacro destructure-define-args ((&rest names) args &body body)
  `(bind (((:alist ,@(loop for n in names
                        collect `(,n ,(as-keyword n))))
           ,args))
     ,@body))
