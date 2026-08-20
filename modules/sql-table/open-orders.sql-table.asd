(in-package #:asdf-user)

(defsystem "open-orders.sql-table/base"
    :author "Robert Burnett"
    :license "MIT"
    :depends-on ("closer-mop")
    :components ((:file "sql-table")))

(defsystem "open-orders.sql-table"
    :depends-on ("open-orders.sql-table/base" "dbi" "marshal"))
