(uiop:define-package #:open-orders.home
    (:use #:cl)
  (:import-from #:defclass-std
                #:defclass/std
                #:class/std)
  (:import-from #:open-orders.utils
                #:fn)
  (:local-nicknames (#:a #:alexandria)
                    (#:tbl #:open-orders.tables)
                    (#:sql #:open-orders.sql-table)
                    (#:class-ui #:open-orders.class-ui)
                    (#:auth #:open-orders.auth))
  (:export #:on-home))
(in-package #:open-orders.home)

(defun on-home (body &aux conn)
  (setf conn (clog:connection-data-item body "conn"))
  (clog:destroy-children body)
  
  (clog:set-on-click (clog:create-button body :content "Logout")
                     (fn (obj) (auth:logout body)))
  (clog:create-p body :content "Home")
  )
