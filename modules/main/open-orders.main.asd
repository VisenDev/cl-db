(in-package #:asdf-user)

(defsystem "open-orders.main"
  :serial t
  :depends-on ("dbd-sqlite3" "open-orders.sql-table" "open-orders.pagen"
               "defclass-std" "uiop" "closer-mop" "hunchentoot"
               "ironclad" "cl-pass" "asdf" "url-rewrite")
  :components ((:file "templates")
               (:file "auth")
               (:file "tables")
               (:file "main")))
