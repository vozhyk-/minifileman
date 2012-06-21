;;;; src/pathnames.lisp -- the pathname library for minifileman

(in-package #:minifileman.pathnames)

;; For supporting '\' in pathnames
#+(and openmcl unix) ; digitool?
(setf ccl::*pathname-escape-character* #\null)

(defun topathname (name)
  #+(and (or sbcl cmu) unix)
  (pathname (if (stringp name)
                (escape-backslashes name)
                name))
  #-(and (or sbcl cmu) unix) (pathname name))

(defun nonempty-pathname-p (pathname)
  (and
   (find-if #'(lambda (x) (cl-fad::component-present-p (funcall x pathname)))
            (list
             #'pathname-host
             #'pathname-device
             #'pathname-directory
             #'pathname-name
             #'pathname-type
             #'pathname-version))
   pathname))

(defun absolute-pathname-p (pathname)
  (eql (first (pathname-directory pathname))
       :absolute))

#+(and (or sbcl cmu) unix)
;;; Redefine cl-fad:pathname-as-file (use topathname)
(defun pathname-as-file (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to file form."
  (let ((pathname (topathname pathspec))) ; changed
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (cond ((directory-pathname-p pathspec)
           (let* ((directory (pathname-directory pathname))
                  (name-and-type (topathname (first (last directory))))) ; changed
             (make-pathname :directory (butlast directory)
                            :name (pathname-name name-and-type)
                            :type (pathname-type name-and-type)
                            :defaults pathname)))
          (t pathname))))

(defun preserve-pathname-directory-form (pathname original)
  (if (directory-pathname-p original)
      (pathname-as-directory pathname)
      (pathname-as-file pathname)))

(defun append-pathnames (&rest names)
  (let ((names (remove nil names)))
    (cond
      ((= (length names) 0) (make-pathname))
      ((= (length names) 1) (first names))
      (t
       (let* ((first (pathname-as-directory (topathname (first names))))
	      (second (topathname (second names)))
	      (first-directory (pathname-directory first))
	      (second-directory (pathname-directory second)))
	 (when (or (wild-pathname-p first)
		   (wild-pathname-p second))
	   (cerror "Try anyway"
		   "Can't reliably convert wild pathnames."))
	 (when (absolute-pathname-p second)
	   (warn "Second argument(~a) is an absolute pathname." second))
	 (apply #'append-pathnames
		(make-pathname
		 :defaults first
		 :directory (append first-directory (rest second-directory))
		 :name (pathname-name second)
		 :type (pathname-type second)
		 :version (pathname-version second))
		(cddr names)))))))

(defun basename (name)
  (let ((pathname (topathname name)))
    (if (not (equal pathname #P"/")) ; unportable?
      (preserve-pathname-directory-form (make-pathname
					 :directory nil
					 :defaults (pathname-as-file pathname))
					pathname)
      pathname)))

(defun dirname (name)
  (let ((pathname (topathname name)))
    (pathname-as-directory
     (make-pathname
      :name nil
      :type nil
      :defaults (pathname-as-file pathname)))))

(defun expand-pathname (name)
  (let* ((pathname (topathname name))
	 (directory (pathname-directory pathname)))
    (cond
      ((string= (second directory) "~")
       (append-pathnames
	(user-homedir-pathname)
	(make-pathname
	 :defaults pathname
	 :directory (apply #'list :relative (cddr directory)))))
      ((and (null directory) (string= (file-namestring pathname) "~"))
       (user-homedir-pathname))
      (t pathname))))

(defun directory-p (pathspec)
  (directory-exists-p pathspec))

(defun file-p (pathspec)
  (not (directory-p pathspec)))

(defun true-pathname-form (name)
  (if (directory-p name)
      (pathname-as-directory name)
      (pathname-as-file name)))
