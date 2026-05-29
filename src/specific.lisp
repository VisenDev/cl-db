(uiop:define-package #:open-orders.specific
  (:use #:cl #:clog) ;; I know use is bad style but in the interest
                     ;; of brevity I am allowing it here
  (:import-from #:open-orders.utils
                #:fn
                #:*let
                #:defstruct*
                #:strcat)
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


(defstruct* (menu-button-config (:constructor make-menu-button-config
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
         (primary-div (create-div parent-div :class "row column-on-mobile"))
         (secondary-div (create-div parent-div :style "margin-right:50px;")))

    ;; create menu-buttons
    (dolist (btn *menu-buttons*)
      (set-on-click
       (create-menu-button primary-div (label btn)
                           :magnifier-icon-p (magnifier-icon-p btn))
       (assign-url-function body (url btn))))

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
      (create-p div :content msg :class "column"))
    (create-a div :content "Copyright Wess Burnett, 2026"
              :link "https://github.com/VisenDev/open-orders"
              :target "_blank" )))


(defstruct* open-order-table-item
  date code part-number po-number line status
  file-number qty)

(defun generate-random-open-order-table-item ()
  (make-open-order-table-item
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

(defstruct* tab-bar-item
  button
  div)

(defstruct* tab-bar
  (buttons-div nil :type (or null clog-div))
  (items nil :type list))

(declaim (ftype (function (clog-obj list) tab-bar) create-tab-bar))
(defun create-tab-bar (obj tab-names)
  (let ((result (make-tab-bar)))
    (setf (buttons-div result) (create-div obj :class "row margin column-on-mobile"))

    ;; Create individual tab div-button pairs
    (loop :for i :from 0
          :for name :in tab-names
          :for button = (create-button (buttons-div result)
                                       :content name
                                       :class "clickable tab-button")
          :for div = (create-div obj)
          :for tab-bar-item = (make-tab-bar-item :button button
                                                 :div div)
          :if (= i 0)
            :do (add-class button "selected")
          :else
            :do (add-class div "hidden")
          :end
          :collect tab-bar-item :into items
          :finally (setf (items result) items))

    ;; Add click functionality to those pairs
    (labels ((select-button (obj)
               (loop :for item :in (items result)
                     :for button = (button item)
                     :for div = (div item)
                     :if (eq button obj)
                       :do (remove-class div "hidden")
                           (add-class obj "selected")
                     :else
                       :do (remove-class button "selected")
                           (add-class div "hidden")
                     :end)))
      (dolist (item (items result))
        (set-on-click (button item)
                      #'select-button)))

    ;; Return result
    result))


(defparameter *on-edit-open-order-tabs*
  '("P.O. Details" "Job Ticket" "Shipping Details"
    "Certificate of Conformance" "Part Labels"
    "Packing List" "Additional Documents"))


(defun create-labeled-row (obj label)
  "Creates a row div and applies a label to it"
  (an:aprog1 (create-div obj :class "row")
    (create-p an:it :content label :class "label grow")))

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
         (_footer (create-copyright-bar div))
         (tabs (create-tab-bar content *on-edit-open-order-tabs*)))

    ;; PO-Details Content
    (*let ((po-details-div (div (first (items tabs))))
           (_top-gap (create-hr po-details-div))
           (form (create-form po-details-div :class "row wrap"))
           (_bottom-gap (create-hr po-details-div))
           (_1 (add-class form "row"))
           (col-left (create-div form :class "column grow"))
           (_line (create-div form
                              :class "vertical-line hide-on-mobile"))
           (col-right (create-div form :class "column grow")))

      ;; Customer Code
      (*let ((div (create-labeled-row col-left "Customer Code"))
             (input-container (create-div div :class "row grow small-gap")))
        (create-form-element input-container :text :name "cc"
                             :class "grow")
        (create-button input-container :class "grow" :content "Email Customer"))

      ;; Customer Name
      (set-on-click
       (create-button (create-labeled-row col-left "Customer Name")
                      :class "grow"
                      :content "View Customer Information")
       (fn (obj)
         (let ((customer-code (name-value form "cc")))
           (if (string= customer-code "")
               ;;Then
               (alert (window body) "No Customer Code")

               ;;Else
               (url-assign (location body)
                           (format nil "/edit-customer?customer-code=~a"
                                   (url:encode customer-code)))))))

      ;; File/Material/JobStatus
      (dolist (label '("File #" "Material Type" "Job Status" "Notes"))
        (create-form-element (create-labeled-row col-right label) :text
                             :class "grow"))

      ;; (set-on-click (create-button col-left :content "get values")
      ;;               (fn (obj)
      ;;                 (alert (window body) (form-get-data form))))
      )))

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
          :for col = (create-table-column
                      header :content h :class "hoverable")
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
