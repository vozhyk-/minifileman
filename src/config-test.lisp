(in-package #:cl-user)
(load "load.lisp")
(define-config
  (("default_dir:use" :default t
		      :depends-on
		      (((eql value t) "default_dir")))
   ("default_dir" :default "/"))
  (:default-path #P"/home/vozhyk/.minifileman/minifileman.conf"))
(defparameter *config* (make-instance 'config))
(clear-config)