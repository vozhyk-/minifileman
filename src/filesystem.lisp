(in-package #:minifileman.filesystem)

(defun list-directory (dirname)
  (mapcar #'basename
     (list-directory-absolute dirname)))
