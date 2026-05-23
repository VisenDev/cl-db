(uiop:define-package #:open-orders.specific
  (:use #:cl #:clog) ;; I know use is bad style but in the interest
                     ;; of brevity I am allowing it here
  (:import-from #:open-orders.utils
                #:fn
                #:*let)
  (:import-from #:defclass-std
                #:defclass/std
                #:class/std)
  (:local-nicknames (#:a #:alexandria)
                    (#:an #:anaphora)
                    (#:tbl #:open-orders.tables)
                    (#:sql #:open-orders.sql-table)
                    (#:rand #:open-orders.random)
                    (#:url #:open-orders.url-parser)
                    (#:auth #:open-orders.auth)
                    (#:class-ui #:open-orders.class-ui)
                    (#:paths #:open-orders.paths)))
(in-package #:open-orders.specific)
(declaim (optimize (debug 3)))

(defvar *db* nil
  "Primary database handle shared by threads.")

(declaim (ftype (function () dbi:dbi-connection) db-get))
(defun db-get ()
  "Returns a handle to the primary database, initializes"
  (unless *db*
    (setf *db* (tbl:database-connect)))
  *db*)

(declaim (ftype (function (clog-body string) (function (&rest t) t))
                assign-url-function))
(defun assign-url-function (body url)
  "Returns a lambda that performs a url assign to the document when called."
  (lambda (&rest args)
    (declare (ignore args))
    (url-assign (location body) url)))

(defun on-login-page (body)
  (load-css (html-document body) "/open-orders.css")
  (let* ((instance (make-instance 'auth:login-form))
         (ui (class-ui:class-ui
              (list
               :username (make-instance 'class-ui:config/text)
               :password (make-instance 'class-ui:config/password))
              instance body
              :form-class "column")))

    ;; TODO fix this, 
    (dbi:with-connection (conn :sqlite3 :database-name paths:*db-path*)
      (set-on-click (create-button body :content "Submit" :class "clickable")
                    (fn (obj)
                      (when (auth:test-credentials
                             body
                             (db-get)
                             (class-ui:finalize-values ui))
                        (url-assign (location body) "/")))))))


(defstruct (menu-button-config (:conc-name mbc-)
                               (:constructor make-menu-button-config
                                   (label url magnifier-icon-p)))
  label url magnifier-icon-p)

(defparameter *menu-buttons*
  (mapcar (a:curry #'apply #'make-menu-button-config)
          '(("Customers"   "/customers"  t)
            ("Open Orders" "/"           t)
            ("Material"    "/material"   t)
            ("History"     "/history"    t)
            ("Forms"       "/forms"    nil))))

(defun create-menu-button (obj content &key magnifier-icon-p)
  (let ((b (create-button obj :class "clickable secondary rounded hoverable
                                      important"
                              :content
                          (if magnifier-icon-p
                              (concatenate 'string content " 🔍")
                              content))))
    b))

(defun load-css-and-menu-buttons (body)
  (load-css (html-document body) "/open-orders.css")
 
  ;; Menu bar
  (let* ((parent-div (create-div body :class "row spaced margin"))
         (primary-div (create-div parent-div :class "row"))
         (secondary-div (create-div parent-div :style "margin-right:50px;")))

    ;; create menu-buttons
    (dolist (btn *menu-buttons*)
      (set-on-click
       (create-menu-button primary-div (mbc-label btn)
                           :magnifier-icon-p (mbc-magnifier-icon-p btn))
       (assign-url-function body (mbc-url btn))))

    ;; New Button
    (set-on-click (create-menu-button secondary-div "New")
                  (assign-url-function body "/new"))

    ;; Exit Button
    (set-on-click (create-p body :content "Exit" :class "exit-button")
                  (fn (obj)
                    (auth:logout body)
                    (url-assign (location body) "/login")))))

(defun create-copyright-bar (body)
  (let ((div (create-div body :class "footer row")))
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

;; (defun on-edit-open-order (body)
;;   ;;menu
;;   (load-css-and-menu-buttons body)

;;   (flet ((create-tab (tab-button-parent tab-content-parent label)
;;            "returns (label button div)"
;;            (list label
;;                  (create-p tab-button-parent
;;                            :class "tab-bar-button"
;;                            :content label)
;;                  (an:aprog1
;;                      (create-div
;;                       tab-content-parent
;;                       :class "tab-bar-content")
;;                    (setf (visiblep an:it) nil)))))

;;     (let* ((div (create-div body :class "window-content"))
;;            (params (url:parse-parameters (url (location body))))
;;            (id (getf params :id))
;;            (header-bar (create-div div :class "header-bar"))
;;            (tab-bar (create-div div :class "tab-bar"))
;;            (tab-labels '("P.O. Details" "Job Ticket" "Shipping Details"
;;                          "Certificate of Conformance" "Part Labels"
;;                          "Packing List" "Additional Documents"))
;;            (tab-content-container (create-div
;;                                    div :class "tab-bar-content-container"))
;;            (tabs
;;              (mapcar
;;               (a:curry #'create-tab tab-bar tab-content-container)
;;               tab-labels)))
;;       (declare (ignorable id header-bar))
;;       (assert (not (null id))) ;; ensure id url parameter was passed

;;       ;; temporary page content for demo
;;       ;; (dolist (tab tabs)
;;       ;;   (create-p (third tab) :content (first tab)))
;;       (create-copyright-bar div)

;;       ;; local functions
;;       (labels ((find-tab (label)
;;                  (an:aprog1
;;                      (assoc label tabs :test #'string-equal)
;;                    (assert (not (null an:it))
;;                            (label) "tab '~a' not found" label)))

;;                (unselect-all-tabs (&optional skip-tab)
;;                  (dolist (tab tabs)
;;                    (unless (equalp tab skip-tab)
;;                      (setf (visiblep (third tab)) nil)
;;                      (clog:remove-class (second tab)
;;                                         "tab-bar-button-selected"))))
               
;;                (select-tab (label-button-div-list)
;;                  (assert (not (null label-button-div-list)))
;;                  (add-class (second label-button-div-list)
;;                             "tab-bar-button-selected")
;;                  (setf (visiblep (third label-button-div-list)) t)
;;                  (unselect-all-tabs label-button-div-list)))

        
;;         (let* ((tab (find-tab "P.O. Details"))
;;                (div (third tab))
;;                (form (create-form div :class "input-area-columns"))
;;                (left-div (create-div form :style "flex:1;"))
;;                (right-div (create-div form :style "flex:1;")))

;;           (let ((row (create-div left-div :class "row")))
;;             (create-label row :content "Customer Code")
;;             (create-form-element row :text :style "width:50%;")
;;             (create-button row :content "E-mail Customer" :style "width:50%;"))

;;           (let ((label (create-label form :content "Customer Name")))
;;             (label-for label (create-button form :content "View Customer Information"))))
        

;;         ;; select first tab by default
;;         (select-tab (find-tab "P.O. Details"))

;;         ;; add on-click behavior to all tabs
;;         (loop :for tab :in tabs
;;               :do (let ((tab tab))
;;                     (set-on-click
;;                      (second tab) (fn (obj) (select-tab tab)))))))))

(defun on-edit-open-order (body)

  ;; Auth Check
  (unless (auth:get-logged-in-user body (db-get) "/login")
    (create-p body :content "Forbidden")
    (return-from on-edit-open-order))

  ;; Load styling
  (load-css-and-menu-buttons body)

  ;; Content
  (*let ((div (create-div body))
         (_header (create-div div :class "header"))
         (content (create-div div))
         (_footer (create-copyright-bar div)))
    (dotimes (i 10)
      (create-p content :content "test-content"))))

(defun on-new-window (body)
  
  ;; Auth check
  (unless (auth:get-logged-in-user body (db-get) "/login")
    (create-p body :content "Forbidden")
    (return-from on-new-window))
 
    ;; Load css and menu
  (load-css-and-menu-buttons body)

    ;; Page Content
  (let* ((tbl (create-table body :class "table"))
         (header (create-table-row tbl :class "header"))
         (content (loop :repeat 20
                        :collect (generate-random-open-order-table-item)))
         (hide-on-mobile-i '(3 4 5 6)))

    ;; Table Header Bar
    (loop :for h :in '("Date" "Code" "Part Number" "P.O.#"
                       "Line" "Status" "File#" "QTY")
          :for i :from 0
          :for col = (create-table-column header :content h :class "hoverable")
          :when (member i hide-on-mobile-i)
            :do (add-class col "hide-on-mobile"))

    ;; Table Rows
    (loop
      :for item :in content
      :for i :from 0
      :do
         (let ((i i)
               (row (clog:create-table-row tbl :class "hoverable clickable"))
               (slots '(date code part-number po-number
                        line status file-number qty)))

           (set-on-click row (assign-url-function
                              body (format nil "/edit-open-order?id=~a"
                                           i)))
           
           ;; Table Row Content
           (loop :for slot :in slots
                 :for j :from 0
                 :for col = (create-table-column
                             row :content (slot-value item slot))
                 :when (member j hide-on-mobile-i)
                   :do (add-class col "hide-on-mobile"))))))

(defun test ()
  (clog:initialize
   #'on-new-window
   :static-root (asdf:system-relative-pathname "open-orders"
                                               "./static-files/"))
  (clog:set-on-new-window #'on-edit-open-order :path "/edit-open-order")
  (clog:set-on-new-window #'on-login-page :path "/login")
  (clog:open-browser))
