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
                    (#:rand #:open-orders.random)
                    (#:url #:open-orders.url-parser)))
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

(class/std open-order-table-item
  date code part-number po-number line status
  file-number qty)

(defun generate-random-open-order-table-item ()
  (make-instance 'open-order-table-item
                 :date (rand:date)
                 :code (format nil "~a/~a" (rand:capital-letter)
                               (rand:capital-letter))
                 :part-number (rand:n-digit-number (+ 4 (random 2)))
                 :line (random 15)
                 :status (rand:random-value '("In Stock" "Waiting"
                                              "Running"))
                 :po-number (rand:n-digit-number (+ 4 (random 2)))
                 :file-number ""
                 :qty (max 100 (* 100 (random 100)))))

(defun on-edit-open-order (body &optional open-order-id)
  ;; (let* ((tab-bar (create-div ))))
  (let* ((params (url:parse-parameters (url (location body))))
         (id (getf params :id)))
    (unless id
      (create-p body :content "No id provided"))
    (create-section body :h1
                    :content (format nil "Editing Open Order ~a" id))))

(defun on-new-window (body)
  (load-css (html-document body) "/open-orders.css")

  ;; Menu bar
  (let ((div (create-div body :class "menu-bar")))
    (dolist (b *menu-buttons*)
      (create-menu-button div b :magnifier-icon-p t))
    (create-menu-button div "Forms")
    (create-p body :content "Exit" :class "exit-button"))

  ;; content
  (let* ((tbl (create-table body :class "table"))
         (header (create-table-head tbl :class "table-header-bar"))
         (content (loop :repeat 20
                        :collect (generate-random-open-order-table-item))))
    (loop :for h :in '("Date" "Code" "Part Number" "P.O.#"
                       "Line" "Status" "File#" "QTY")
          :for i :from 0
          :do
             (let ((heading (create-table-heading
                             header :content h
                             :class "table-header-bar-item")))
               (when (> i 2)
                 (add-class heading "hidden-on-mobile"))
               
               (setf (attribute heading "title")
                     (format nil "Sort by ~a" h))))
    
    (loop
      :for item :in content
      :for i :from 0
      :do
         (let ((i i)
               (row (clog:create-table-row tbl :class "table-row"))
               (slots '(date code part-number po-number
                        line status file-number qty)))
           (set-on-click row  (lambda (obj)
                                (declare (ignore obj))
                                (clog:url-assign
                                 (location body)
                                 (format nil "/edit-open-order?id=~a"
                                         i))))
           (dolist (slot slots)
             (let ((i i))
               (create-table-column
                row :content (slot-value item slot)
                :class                  
                (when (> i 2)
                  "hidden-on-mobile"))
               ))))))

(defun test ()
  (clog:initialize
   #'on-new-window
   :static-root (asdf:system-relative-pathname "open-orders"
                                               "./static-files/"))
  (clog:set-on-new-window #'on-edit-open-order :path "/edit-open-order")
  (clog:open-browser)
  )
