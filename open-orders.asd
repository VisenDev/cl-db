(in-package #:asdf-user)

(defsystem "open-orders" 
  :author "Robert Burnett"
  :license "GPL-3.0"
  :depends-on ("clog"
               "alexandria"
               "closer-mop"
               "defclass-std"
               "marshal"
               "cl-dbi"
               "sqlite"
               "uiop"
               "cl-pass"
               "parse-float"
               "introspect-environment"
               "anaphora")
  :serial t
  :components ((:module "static-files"
                :components ((:static-file "open-orders.css")))
               (:module "src"
                :serial t
                :components ((:file "utils")
                             (:file "paths")
                             (:file "sql-table")
                             (:file "tables")
                             (:file "admin")
                             (:file "random")
                             (:file "url-parser")
                             (:file "specific")))))
#+nil
(defsystem "open-orders/executable"
  :build-operation program-op
  :build-pathname "open-orders"
  :entry-point "open-orders.main:main"
  :depends-on ("open-orders"))
