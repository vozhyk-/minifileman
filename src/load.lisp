;;;; load.lisp - loads src

(in-package #:cl-user)

(defun :load-pr ()
  (asdf:oos 'asdf:load-op '#:minifileman)
  (let ((*package* (find-package '#:cl-user)))
    (use-package '(#:minifileman.gui-lib
                   #:minifileman.config
                   #:minifileman.pathnames
                   #:minifileman.filesystem
                   #:minifileman))))

(:load-pr)
