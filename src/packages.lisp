(in-package #:cl-user)

(defpackage #:minifileman.utils
  (:nicknames #:org.programmingforchildren.minifileman.utils)
  (:use #:metabang-bind
        #:iterate
        #:common-lisp)
  (:import-from #:kmrcl
    #:mklist
    #:with-gensyms
    #:_f
    #:it)
  (:import-from #:alexandria
    #:parse-ordinary-lambda-list)
  (:export #:last1
           #:less-if
           #:setfun
           #:ask-read-line
           #:multi-level-sort
           #:_f2
	   ;#:alist->hash
	   ;#:skip-keyword-args
	   #:remove-keyword-args
	   #:add-replace-keyword-args
           #:acase
           #:it
           #:positions
           #:nth-comp
           #:ordinary-lambda-list-vars
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
           #:append-pathnames
	   #:expand-pathname
	   #:directory-pathname-p
	   #:absolute-pathname-p
           #:home-pathname-p
           #:root-p
           #:home-p
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
	#:metabang-bind
        #:iterate
	#:common-lisp)
  (:import-from #:minifileman.utils
    #:_f2
    #:last1)
  (:import-from #:kmrcl
    #:get-alist
    #:remove-alist
    #:mklist)
  (:export #:*default-config-path*
	   #:*config*
	   #:config
           #:configq
	   #:delete-config
	   #:delete-config!
	   #:read-config
	   #:write-config))

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
    #:ordinary-lambda-list-vars
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
           #:mult-bind
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
        #:iterate
	#:common-lisp)
  (:import-from #:minifileman.utils
    #:multi-level-sort
    #:less-if
    #:setfun)
  (:import-from #:kmrcl
    #:string-starts-with)
  (:export #:minifileman
           #:panel
           #:*panel*
           #:go-to-dir
	   #:minifileman-list-dir
	   #:quit-minifileman))

(defpackage #:minifileman-user
  (:use #:minifileman
        #:iterate
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
  (:shadowing-import-from #:iterate
    #:while
    #:for
    #:until
    #:in)
  (:shadowing-import-from #:kmrcl
    #:copy-file
    #:delete-directory-and-files))
