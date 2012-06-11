(in-package #:minifileman.filesystem)

(defun list-directory-relative (dirname)
  (mapcar #'basename
          (list-directory dirname)))
