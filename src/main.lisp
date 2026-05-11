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


(defun on-new-window (body)
  
  (let ((conn (make-instance 'connection)))

    (setf (clog:connection-data-item body "conn") conn)
    
    ;; Load css
    (when *use-css*
      (if *use-external-css*
          (clog:load-css (clog:html-document body) *pico-css-url*)

          ;; otherwise use local cached version
          (clog:create-child (clog:head-element (clog:html-document body))
                             *pico-css*)))

    (clog:set-html-on-close body "<script>close();</script>")
    (setf (clog:title (clog:html-document body)) "Open Orders")
    (clog:enable-clog-popup)            ; To allow browser popups

    ;; loading bar
    (clog:create-child body "<div aria-busy=\"true\"/>")

    ;; load database
    (setf (db conn) (tbl:database-connect))

    (setf (clog:url (clog:location body)))
    (on-login-screen body)

    ;; Block until body has been closed
    (clog:run body)
    (when (db conn)
      (tbl:database-disconnect (db conn)))))

(defun main ()
  (clog:n)
  )
