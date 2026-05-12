(uiop:define-package #:open-orders.main
  (:use #:cl)
  (:import-from #:defclass-std
                #:defclass/std
                #:class/std)
  (:local-nicknames (#:a #:alexandria)
                    (#:tbl #:open-orders.tables)
                    (#:sql #:open-orders.sql-table))
  (:export #:main))
(in-package #:open-orders.main)

(defparameter *css*
  "<style>

:root {color-scheme: light dark;}
* {pad:2px;margin:2px;}
label {min-width: 100px;display:block;}
form {display:grid;max-width:300px;grid-template-columns:100px 1fr;}

  </style>")

(defun on-new-window (body)

  (let ((conn (make-instance 'tbl:connection
                             :db (tbl:database-connect))))

    (setf (clog:connection-data-item body "conn") conn)

    ;;setup page
    (clog:create-child (clog:head-element (clog:html-document body)) *css*)
    (clog:set-html-on-close body "<script>close();</script>")
    (setf (clog:title (clog:html-document body)) "Open Orders")
    (clog:enable-clog-popup)            ; To allow browser popups

    (open-orders.auth:on-login body)

    ;; Block until body has been closed
    (clog:run body)
    (when (tbl:db conn)
p      (tbl:database-disconnect (tbl:db conn)))))

(defun main ()
  (clog:initialize #'on-new-window)
  (clog:set-on-new-window
   (lambda (body)
     (clog:url-replace (clog:location body) "/"))
   :path "/home")
  (clog:open-browser)
  )
