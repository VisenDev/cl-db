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
               "introspect-environment")
  :serial t
  :components ((:module "static-files"
                :components ((:static-file "pico.min.css")))
               (:module "src"
                :serial t
                :components ((:file "utils")
                             ;; (:file "tab-bar")
                             (:file "paths")
                             (:file "sql-table")
                             (:file "tables")
                             (:file "admin")
                             ;; (:file "class-ui")
                             ;; (:file "auth")
                             (:file "random")
                             (:file "specific")
                             ;; (:file "pages")
                             ;; (:file "ui")
                             ;; (:file "main")
                             ))))

(defsystem "open-orders/executable"
  :build-operation program-op
  :build-pathname "open-orders"
  :entry-point "open-orders.main:main"
  :depends-on ("open-orders")
  )
