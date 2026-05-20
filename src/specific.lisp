(uiop:define-package #:open-orders.specific
  (:use #:cl #:clog) ;; I know use is bad style but in the interest
                     ;; of brevity I am allowing it here
  (:import-from #:open-orders.utils
                #:fn)
  (:import-from #:defclass-std
                #:defclass/std
                #:class/std)
  (:local-nicknames (#:a #:alexandria)
                    (#:an #:anaphora)
                    (#:tbl #:open-orders.tables)
                    (#:sql #:open-orders.sql-table)
                    (#:rand #:open-orders.random)
                    (#:url #:open-orders.url-parser)))
(in-package #:open-orders.specific)

(declaim (optimize (debug 3)))


(defparameter *menu-buttons*
    ;; label      url         magnifier-icon-p
  '(("Customers" "/customers" t)
    ("Open Orders" "/" t)
    ("Material" "/material" t)
    ("History" "/history" t)
    ("Forms" "/forms" nil)))

(defun create-menu-button (obj content &key magnifier-icon-p)
  (let ((b (create-button obj :class "menu-button"
                              :content
                          (if magnifier-icon-p
                              (concatenate 'string content " 🔍")
                              content))))
    b))

(defun load-css-and-menu-buttons (body)
  (load-css (html-document body) "/open-orders.css")

  ;; Menu bar
  (let ((div (create-div body :class "menu-bar")))
    (loop :for (label url magnifier) :in *menu-buttons* :do
      (let ((url url))
        (set-on-click
         (create-menu-button div label :magnifier-icon-p magnifier)
         (lambda (obj)
           (declare (ignore obj))
           (url-assign (location body) url)))))
    (create-p body :content "Exit" :class "exit-button")))

(defun create-copyright-bar (body)
  (let ((div (create-div body :class "copyright-bar")))
    (dolist (msg '("Created By Wess Burnett" "Record ID #" "Work Order #"))
      (create-p div :content msg))
    (create-a div :content "Copyright Wess Burnett, 2026"
              :link "https://github.com/VisenDev/open-orders"
              :target "_blank" )))


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

(defun on-edit-open-order (body)
  ;;menu
  (load-css-and-menu-buttons body)

  (flet ((create-tab (tab-button-parent tab-content-parent label)
           "returns (label button div)"
           (list label
                 (create-p tab-button-parent
                           :class "tab-bar-button"
                           :content label)
                 (an:aprog1
                     (create-div
                      tab-content-parent
                      :class "tab-bar-content")
                   (setf (visiblep an:it) nil)))))

    (let* ((div (create-div body :class "window-content"))
           (params (url:parse-parameters (url (location body))))
           (id (getf params :id))
           (header-bar (create-div div :class "header-bar"))
           (tab-bar (create-div div :class "tab-bar"))
           (tab-labels '("P.O. Details" "Job Ticket" "Shipping Details"
                         "Certificate of Conformance" "Part Labels"
                         "Packing List" "Additional Documents"))
           (tab-content-container (create-div
                                   div :class "tab-bar-content-container"))
           (tabs
             (mapcar
              (a:curry #'create-tab tab-bar tab-content-container)
              tab-labels)))
      (declare (ignorable id header-bar))
      (assert (not (null id))) ;; ensure id url parameter was passed

      ;; temporary page content for demo
      (dolist (tab tabs)
        (create-p (third tab) :content (first tab)))

      (create-copyright-bar div)

      ;; local functions
      (labels ((find-tab (label)
                 (an:aprog1
                     (assoc label tabs :test #'string-equal)
                   (assert (not (null an:it))
                           (label) "tab '~a' not found" label)))

               (unselect-all-tabs (&optional skip-tab)
                 (dolist (tab tabs)
                   (unless (equalp tab skip-tab)
                     (setf (visiblep (third tab)) nil)
                     (clog:remove-class (second tab)
                                        "tab-bar-button-selected"))))
               
               (select-tab (label-button-div-list)
                 (assert (not (null label-button-div-list)))
                 (add-class (second label-button-div-list)
                            "tab-bar-button-selected")
                 (setf (visiblep (third label-button-div-list)) t)
                 (unselect-all-tabs label-button-div-list)))

        ;; select first tab by default
        (select-tab (find-tab "P.O. Details"))

        ;; add on-click behavior to all tabs
        (loop :for tab :in tabs
              :do (let ((tab tab))
                    (set-on-click
                     (second tab) (fn (obj) (select-tab tab)))))))))

(defun on-new-window (body)
  (load-css-and-menu-buttons body)

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
                  "hidden-on-mobile"))))))))

(defun test ()
  (clog:initialize
   #'on-new-window
   :static-root (asdf:system-relative-pathname "open-orders"
                                               "./static-files/"))
  (clog:set-on-new-window #'on-edit-open-order :path "/edit-open-order")
  (clog:open-browser))
