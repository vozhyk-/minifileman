;;;; load.lisp - loads src

(in-package #:cl-user)

(load "minifileman.asd")

(defun :load-pr ()
  (asdf:oos 'asdf:load-op '#:minifileman)
  (when (eql *package* (find-package '#:cl-user))
    (setf *package* (find-package '#:minifileman-user))))

(:load-pr)
