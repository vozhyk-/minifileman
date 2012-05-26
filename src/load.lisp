;;;; load.lisp - loads src

(in-package #:cl-user)

(require '#:closer-mop)
(require '#:kmrcl)
(require '#:ltk)
;(require '#:ltk-mw)

(defparameter *ordered-file-list*
  '("packages.lisp"
    "helpers.lisp"
    "macro-helpers.lisp"
    "ltk-ext.lisp"
    "gui-lib.lisp"
    "pathnames.lisp"
    "filesystem.lisp"
    "config.lisp"
    "minifileman.lisp"))

(defun :load-pr ()
  (let ((*package* (find-package '#:cl-user)))
    (dolist (f *ordered-file-list*)
      (load f))
    (use-package '(#:org.programmingforchildren.minifileman.gui-lib
		   #:org.programmingforchildren.minifileman.config
		   #:org.programmingforchildren.minifileman.pathnames
		   #:org.programmingforchildren.minifileman.filesystem
		   #:org.programmingforchildren.minifileman.minifileman))))

(:load-pr)
