(in-package #:cl-user)

(defpackage #:minifileman.utils
  (:nicknames #:org.programmingforchildren.minifileman.utils)
  (:use #:metabang-bind
        #:common-lisp)
  (:import-from #:kmrcl
    #:with-gensyms
    #:it)
  (:export #:last1
           #:ask-read-line
	   ;#:alist->hash
	   #:dohash
	   ;#:skip-keyword-args
	   #:remove-keyword-args
	   #:add-replace-keyword-args
           #:acase
           #:it
           #:positions
           #:nth-comp
	   #:class-from
	   #:class-precedence-list
	   #:superclass-p
	   #:append-new-superclasses))

(defpackage #:minifileman.macro-utils
  (:nicknames #:org.programmingforchildren.minifileman.macro-utils)
  (:use #:metabang-bind
        #:common-lisp)
  (:export #:as-keyword
	   #:destructure-define-args))

(defpackage #:minifileman.pathnames
  (:nicknames #:org.programmingforchildren.minifileman.pathnames)
  (:use #:cl-fad
        #:metabang-bind
        #:common-lisp)
  (:import-from #:kmrcl
    #:escape-backslashes
    #:mklist)
  (:export #:topathname
           #:nonempty-pathname-p
           #:append-pathnames
	   #:expand-pathname
	   #:directory-pathname-p
	   #:absolute-pathname-p
           #:root-p
	   #:true-pathname-form
	   #:preserve-pathname-directory-form
	   #:basename
	   #:dirname
	   #:directory-p
	   #:file-p))

(defpackage #:minifileman.filesystem
  (:nicknames #:org.programmingforchildren.minifileman.filesystem)
  (:use #:minifileman.pathnames
        #:cl-fad
	#:common-lisp)
  (:export #:list-directory-relative))

(defpackage #:minifileman.config
  (:nicknames #:minifileman-config
              #:org.programmingforchildren.minifileman.config)
  (:use #:minifileman.pathnames
        #:cl-fad
	#:minifileman.macro-utils
        #:metabang-bind
	#:common-lisp)
  (:import-from #:minifileman.utils
    #:ask-read-line
    #:dohash)
  (:import-from #:kmrcl
    #:get-alist
    #:let-if
    #:mklist)
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
    #:acase
    #:last1
    #:superclass-p
    #:append-new-superclasses)
  (:import-from #:kmrcl
    #:get-alist
    #:with-gensyms
    #:awhen
    #:it
    #:let-when)
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
        #:cl-fad
	#:ltk
	#:common-lisp)
  (:import-from #:kmrcl
    #:string-starts-with)
  (:export #:minifileman
           #:*panel*
           #:go-to-dir
	   #:minifileman-list-dir
	   #:quit-minifileman))

(defpackage #:minifileman-user
  (:use #:minifileman
        #:minifileman.utils
        #:kmrcl
        #:cl-fad
        #:minifileman.pathnames
	#:minifileman.filesystem
	#:minifileman.config
	#:minifileman.gui-lib
	#:ltk
        #:common-lisp
	#:common-lisp-user)
  (:shadowing-import-from #:kmrcl
    #:copy-file
    #:delete-directory-and-files))
