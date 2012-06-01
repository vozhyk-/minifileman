;;;; src/pathnames.lisp -- the pathname library for minifileman

(in-package #:minifileman.pathnames)

(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

(defun absolute-pathname-p (pathname)
  (eql (first (pathname-directory pathname))
       :absolute))

(defun directory-pathname-p (p)
  (and
   p
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))))

(defun pathname-as-file (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (let ((directory (pathname-directory pathname)))
      (if (and (directory-pathname-p pathname)
	       (/= (length directory) 1)) ; not tested under non-Linux systems
	(let ((name-and-type (pathname (first (last directory)))))
	  (make-pathname
	   :defaults pathname
	   :directory (butlast directory)
	   :name (pathname-name name-and-type)
	   :type (pathname-type name-and-type)))
	pathname))))

(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
      (make-pathname
       :defaults pathname
       :directory (append (or (pathname-directory pathname) (list :relative))
			  (list (file-namestring pathname)))
       :name nil
       :type nil)
      pathname)))

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
       (let* ((first (pathname-as-directory (pathname (first names))))
	      (second (pathname (second names)))
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
  (let ((pathname (pathname name)))
    (if (not (equal pathname #P"/"))
      (preserve-pathname-directory-form (make-pathname
					 :directory nil
					 :defaults (pathname-as-file pathname))
					pathname)
      pathname)))

(defun dirname (name)
  (let ((pathname (pathname name)))
    (pathname-as-directory
     (make-pathname
      :name nil
      :type nil
      :defaults (pathname-as-file pathname)))))

(defun expand-pathname (name)
  (let* ((pathname (pathname name))
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

(defun directory-wildcard (dirname)
  (make-pathname
   :defaults (pathname-as-directory dirname)
   :name :wild
   :type #-clisp :wild #+clisp nil))

#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
  (make-pathname
   :directory (append (pathname-directory wildcard) (list :wild))
   :name nil
   :type nil
   :defaults wildcard))

(defun list-directory-absolute (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (let ((wildcard (directory-wildcard dirname)))
    
    #+(or sbcl cmu lispworks)
    (directory wildcard)
    
    #+openmcl
    (directory wildcard :directories t)
    
    #+allegro
    (directory wildcard :directories-are-files nil)
    
    #+clisp
    (nconc
     (directory wildcard)
     (directory (clisp-subdirectories-wildcard wildcard)))
    
    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory-absolute not implemented")))

(defun file-exists-p (pathname)
  #+(or sbcl lispworks openmcl)
  (probe-file pathname)
  
  #+(or allegro cmu)
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))
  
  #+clisp
  (or (ignore-errors
        (probe-file (pathname-as-file pathname)))
      (ignore-errors
        (let ((directory-form (pathname-as-directory pathname)))
          (when (ext:probe-directory directory-form)
            directory-form))))
  
  #-(or sbcl cmu lispworks openmcl allegro clisp)
  (error "file-exists-p not implemented"))

(defun true-pathname-form (name)
  (file-exists-p (pathname name)))

(defun directory-p (name)
  (let ((pathname (pathname name)))
    (directory-pathname-p (true-pathname-form pathname))))

(defun file-p (name)
  (not (directory-p name)))

(defun walk-directory (dirname fn &key directories (test (constantly t)))
  (labels
      ((walk (name)
	 (cond
           ((directory-pathname-p name)
	    (when (and directories (funcall test name))
	      (funcall fn name))
	    (dolist (x (list-directory-absolute name)) (walk x)))
           ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))
