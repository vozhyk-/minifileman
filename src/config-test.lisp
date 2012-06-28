(load "load.lisp")
(in-package #:minifileman.config)
(define-config
  (("default-dir:use" :default t
		      :depends-on
                      #'(lambda (value)
                          (when (eql value t) "default_dir")))
   ("default-dir" :default "/")))
(defparameter *config* (make-instance 'config))
