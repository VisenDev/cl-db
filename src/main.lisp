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
* {pad:2px;margin:2px;font-size:13pt;font-family:sans-serif;}
label {min-width: 120px;display:block;}
form {display:grid;max-width:360px;grid-template-columns:120px 1fr;}

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

    (open-orders.pages:setup-popstate-handler body)
    (open-orders.pages:on-login body)

    ;; Block until body has been closed
    (clog:run body)
    (when (tbl:db conn)
      (tbl:database-disconnect (tbl:db conn)))))

(defun main ()
  (clog:initialize #'on-new-window
                   :static-root (asdf:system-relative-pathname "open-orders"
                                                               "./static-files/"))
  (clog:set-on-new-window
   (lambda (body)
     (clog:url-replace (clog:location body) "/"))
   :path "/home")
  (clog:open-browser)
  )
