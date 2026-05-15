(uiop:define-package #:open-orders.specific
  (:use #:cl #:clog)
  (:import-from #:open-orders.utils
                #:fn)
  (:import-from #:defclass-std
                #:defclass/std
                #:class/std)
  (:local-nicknames (#:a #:alexandria)
                    (#:tbl #:open-orders.tables)
                    (#:sql #:open-orders.sql-table)
                    (#:class-ui #:open-orders.class-ui)
                    (#:auth #:open-orders.auth)))
(in-package #:open-orders.specific)

(declaim (optimize (debug 3)))


(defparameter *menu-buttons*
  '("Customers" "Open Orders" "Material" "History"))

(defun create-menu-button (obj content &key magnifier-icon-p)
  (let ((b (create-button obj :class "menu-button"
                              :content
                          (if magnifier-icon-p
                              (concatenate 'string content " 🔍")
                              content))))
    b))

(defun on-new-window (body)
  (load-css (html-document body) "/open-orders.css")

  ;; Menu bar
  (let ((div (create-div body :class "menu-bar")))
    (dolist (b *menu-buttons*)
      (create-menu-button div b :magnifier-icon-p t))
    (create-menu-button div "Forms")
    (create-p body :content "Exit" :class "exit-button"))

  ;; content
  (let* ((tbl (create-table body))
         (header (create-table-heading tbl :class "table-header-bar")))
    (dolist (h '("Date" "Code" "Part Number" "P.O.#"
                 "Line" "Status" "File#" "QTY"))
      (create-table-heading header :content h
                                   :class "table-header-bar-item")))
  )

(defun test ()
  (clog:initialize
   #'on-new-window
   :static-root (asdf:system-relative-pathname "open-orders"
                                               "./static-files/"))
  (clog:open-browser)
  )
