(defpackage #:open-orders.templates
  (:use #:cl
        #:open-orders.pagen
        #:open-orders.sql-table
        #:open-orders.tables)
  (:export
   #:with-page))
(in-package #:open-orders.templates)

;; CSS loader handler
(hunchentoot:define-easy-handler (css :uri "/orders.css") ()
  (setf (hunchentoot:content-type*) "text/css")
  #.(uiop:read-file-string (asdf:system-relative-pathname "open-orders.main"
                                                          "orders.css")))

(defmacro with-page (&body body)
  `(progn
     (setf (hunchentoot:content-type*) "text/html")
     (doctype ()
       (html ()
         (head ()
           (title () "Open Orders")
           (meta (:charset "utf-8"))
           (link (:href "/orders.css" :rel "stylesheet")))
         (body ()
           ,@body)))))


