(in-package #:asdf-user)

(defsystem "sql-table/base"
    :author "Robert Burnett"
    :license "MIT"
    :depends-on ("closer-mop")
    :components ((:file "sql-table")))

(defsystem "sql-table"
    :depends-on ("sql-table/base dbi marshal"))
