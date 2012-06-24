(in-package #:cl-user)
(defpackage #:minifileman-system
  (:use #:cl #:asdf))
(in-package #:minifileman-system)

(defsystem #:minifileman
  :version "0.1.0"
  :author "Vitaut Bajaryn <vitaut.bayaryn@gmail.com>"
  :license "BSD-2-Clause"
  :depends-on (#:ltk
               #:cl-fad
               #:closer-mop
               ;; with :flet and :labels
               ;; on Gentoo: >metabang-bind-0.7.4
               #:metabang-bind
               #:iterate
               #:alexandria
               #:kmrcl)
  :components
  ((:file "packages")
   (:file "utils" :depends-on ("packages"))
   (:file "macro-utils" :depends-on ("packages"))
   (:file "pathnames" :depends-on ("utils"))
   (:file "filesystem" :depends-on ("pathnames"))
   (:file "config" :depends-on ("pathnames" "macro-utils"))
   (:file "ltk-ext")
   (:file "gui-lib" :depends-on ("ltk-ext" "utils" "macro-utils"))
   (:file "minifileman"
          :depends-on ("filesystem"
                       "config"
                       "gui-lib"))
   (:file "path-entry" :depends-on ("minifileman"))))
