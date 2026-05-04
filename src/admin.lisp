(open-orders.utils:defpackage* #:open-orders.admin
  (:use #:cl)
  (:nicknames #:ooa)
  (:local-nicknames (#:a #:alexandria)
                    (#:sql #:open-orders.sql-table)
                    (#:tbl #:open-orders.tables)
                    (#:paths #:open-orders.paths)
                    (#:mop #:closer-mop))
  (:export
   #:user-create-new
   #:user-update-password))
(in-package #:open-orders.admin)

(defun user-create-new (name password)
  (let ((db (tbl:database-connect)))
    (sql:exec-insert
     (make-instance 'tbl:user
                    :name name
                    :hash (cl-pass:hash password))
     db)
    (tbl:database-disconnect db)))

(defun user-update-password (name password)
  (let ((db (tbl:database-connect)))
    (sql:exec-update
     (make-instance 'tbl:user
                    :name name
                    :hash (cl-pass:hash password))
     db)
    (tbl:database-disconnect db)))
