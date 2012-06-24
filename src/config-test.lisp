(load "load.lisp")
(in-package #:minifileman.config)
(define-config
  (("default_dir:use" :default t
		      :depends-on
                      #'(lambda (value)
                          (when (eql value t) "default_dir")))
   ("default_dir" :default "/"))
  (:default-path #P"/home/vozhyk/.minifileman/minifileman.conf"))
(defparameter *config* (make-instance 'config))
