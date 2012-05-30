(in-package #:cl-user)
(defpackage #:minifileman-system
  (:use #:cl #:asdf))
(in-package #:minifileman-system)

(defsystem #:minifileman
  :version "0.1.0"
  :author "Vitaut Bajaryn <vitaut.bayaryn@gmail.com>"
  :license "BSD-2-Clause"
  :depends-on (#:ltk
               #:closer-mop
               #:kmrcl)
  :components
  ((:file "packages")
   (:file "helpers" :depends-on ("packages"))
   (:file "macro-helpers" :depends-on ("packages"))
   (:file "pathnames" :depends-on ("helpers"))
   (:file "filesystem" :depends-on ("pathnames"))
   (:file "config" :depends-on ("pathnames" "macro-helpers"))
   (:file "ltk-ext")
   (:file "gui-lib" :depends-on ("ltk-ext" "helpers" "macro-helpers"))
   (:file "minifileman"
          :depends-on ("filesystem"
                       "config"
                       "gui-lib"))
   (:file "path-entry" :depends-on ("minifileman"))))
