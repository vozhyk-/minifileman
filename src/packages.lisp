(in-package #:cl-user)

(defpackage #:minifileman.utils
  (:nicknames #:org.programmingforchildren.minifileman.utils)
  (:use #:common-lisp)
  (:export #:ask-read-line
	   #:with-condition-slots
	   ;#:alist->hash
	   #:dohash
	   #:as-list
	   ;#:skip-keyword-args
	   #:remove-keyword-args
	   #:add-replace-keyword-args
           #:starts-with
           #:positions
           #:nth-comp
	   #:class-from
	   #:class-precedence-list
	   #:superclass-p
	   #:append-new-superclasses))

(defpackage #:minifileman.macro-utils
  (:nicknames #:org.programmingforchildren.minifileman.macro-utils)
  (:use #:common-lisp)
  (:export #:as-keyword
	   #:with-gensyms
	   #:destructure-define-args))

(defpackage #:minifileman.pathnames
  (:nicknames #:org.programmingforchildren.minifileman.pathnames)
  (:use #:common-lisp)
  (:export #:list-directory-absolute
	   #:file-exists-p
	   #:append-pathnames
	   #:expand-pathname
	   #:directory-pathname-p
	   #:file-pathname-p
	   #:absolute-pathname-p
	   #:pathname-as-directory
	   #:pathname-as-file
	   #:true-pathname-form
	   #:preserve-pathname-directory-form
	   #:basename
	   #:dirname
	   #:walk-directory
	   #:directory-p
	   #:file-p))

(defpackage #:minifileman.filesystem
  (:nicknames #:org.programmingforchildren.minifileman.filesystem)
  (:use #:minifileman.pathnames
	#:common-lisp)
  (:export #:list-directory))

(defpackage #:minifileman.config
  (:nicknames #:minifileman-config
              #:org.programmingforchildren.minifileman.config)
  (:use #:minifileman.pathnames
	#:minifileman.macro-utils
	#:common-lisp)
  (:import-from #:minifileman.utils
    #:as-list
    #:ask-read-line
    #:dohash
    #:with-condition-slots)
  (:import-from #:kmrcl
    #:get-alist)
  (:export #:define-config
           #:*default-config-path*
	   #:*config-defaults*
	   #:*config-dependencies*
	   #:*config*
	   #:config
	   #:default
	   #:set-to-default
	   #:set-p
	   #:path
	   #:hash
	   #:remconfig
	   #:clear-config
	   #:add-line
	   #:add-comment
	   #:read-config
	   #:doconfig
	   #:print-config
	   #:write-config
	   #:check-config
	   #:mising-parameter
	   ;#:*config-key-unsets*
	   ))

(defpackage #:minifileman.gui-lib
  (:nicknames #:org.programmingforchildren.minifileman.gui-lib)
  (:use #:minifileman.macro-utils
	#:ltk
	#:common-lisp)
  (:import-from #:minifileman.utils
    #:remove-keyword-args
    #:add-replace-keyword-args
    #:superclass-p
    #:append-new-superclasses)
  (:import-from #:kmrcl
    #:get-alist
    #:awhen
    #:it)
  (:export #:gui-class
	   #:define-gui-class
	   #:self-autoresize
	   #:callback))

(defpackage #:minifileman
  (:nicknames #:org.programmingforchildren.minifileman
              #:org.programmingforchildren.minifileman.minifileman)
  (:use #:minifileman.pathnames
	#:minifileman.filesystem
	#:minifileman.config
	#:minifileman.gui-lib
	#:ltk
	#:common-lisp)
  (:export #:main
	   #:minifileman-list-dir
	   #:quit-minifileman))

(defpackage #:minifileman-user
  (:use #:minifileman
        #:minifileman.utils
        #:kmrcl
        #:minifileman.pathnames
	#:minifileman.filesystem
	#:minifileman.config
	#:minifileman.gui-lib
	#:ltk
        #:common-lisp
	#:common-lisp-user))
