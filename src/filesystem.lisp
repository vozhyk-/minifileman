(in-package #:org.programmingforchildren.minifileman.filesystem)

(defun list-directory (dirname)
  (mapcar #'basename
     (list-directory-absolute dirname)))
