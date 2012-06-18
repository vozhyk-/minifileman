(in-package #:minifileman.filesystem)

;;; Redefine cl-fad:list-directory (add :resolve-symlinks option)
(defun list-directory (dirname &rest args &key (resolve-symlinks t))
  "Returns a fresh list of pathnames corresponding to the truenames of
all files within the directory named by the non-wild pathname
designator DIRNAME.  The pathnames of sub-directories are returned in
directory form - see PATHNAME-AS-DIRECTORY."
  (declare (ignorable args resolve-symlinks))
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  #+:ecl
  (let ((dir (pathname-as-directory dirname)))
    (concatenate 'list
                 (apply #'directory (merge-pathnames (pathname "*/") dir args))
                 (apply #'directory (merge-pathnames (pathname "*.*") dir args))))
  #-:ecl
  (let ((wildcard (cl-fad::directory-wildcard dirname)))
    #+:abcl (system::list-directory dirname resolve-symlinks)
    #+:sbcl (apply #'directory wildcard args)
    #+(or :cmu :scl) (directory wildcard :truenamep resolve-symlinks)
    #+:lispworks (directory wildcard :link-transparency resolve-symlinks)
    #+(or :openmcl :digitool)
    (directory wildcard :directories t :follow-links resolve-symlinks)
    ;; No resolve-symlinks (no such option)
    #+:allegro (directory wildcard :directories-are-files nil)
    #+:clisp
    (let ((list (nconc (directory wildcard :if-does-not-exist :keep :full t)
                       (directory (cl-fad::clisp-subdirectories-wildcard wildcard)
                                  :full t))))
      (mapcar (if resolve-symlinks #'second #'first) list))
    ;; No resolve-symlinks (couldn't find Corman Lisp's documentation)
    #+:cormanlisp (nconc (directory wildcard)
                         (cl::directory-subdirs dirname)))
  #-(or :sbcl :cmu :scl :lispworks :openmcl :allegro :clisp :cormanlisp :ecl :abcl :digitool)
  (error "LIST-DIRECTORY not implemented"))

(defun list-directory-relative (dirname)
  (mapcar #'basename
          (list-directory dirname :resolve-symlinks nil)))
