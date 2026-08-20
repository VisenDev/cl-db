(in-package #:asdf-user)

(defsystem "open-orders.main"
  :serial t
  :depends-on ("open-orders.sql-table" "open-orders.pagen"
               "defclass-std" "uiop" "closer-mop" "hunchentoot")
  :components ((:file "tables")
               (:file "main")))
