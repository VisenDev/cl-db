(defpackage #:open-orders.ui
  (:use #:cl)
  (:import-from #:defclass-std
                #:defclass/std
                #:class/std)
  (:import-from #:open-orders.utils
                #:fn
                #:*let)
  (:local-nicknames (#:a #:alexandria)
                    (#:gui #:clog-gui)
                    (#:auth #:clog-auth)
                    (#:sql #:open-orders.sql-table)
                    (#:paths #:open-orders.paths)
                    (#:tbl #:open-orders.tables)))
(in-package #:open-orders.ui)
(declaim (optimize (debug 3)))


(deftype page-function () '(function (clog:clog-obj connection) t))

(defclass/std connection ()
  ((user db db-future menu page-function-stack)))

(declaim (ftype page-function on-logged-in-screen)) ;;forward declaration

(defun page-back (body conn &optional (default #'on-logged-in-screen))
  "Go to the last page on the stack"
  (if (page-function-stack conn)
      (funcall (pop (page-function-stack conn)) body conn)
      (funcall default body conn)))

(defun page-go (body conn &key from to)
  (push from (page-function-stack conn))
  (funcall to body conn))


(defun authentication-token-create ()
  "Create a unique token used to associate a browser with a user"
  (crypto:byte-array-to-hex-string
   (crypto:random-data 16)))

(defmethod marshal:class-persistent-slots ((class tbl:customer))
  (closer-mop:ensure-finalized (class-of class))
  (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots
                                             (class-of class))))

(defparameter *confirm-js*
  "confirm(\"Are you sure you want to go back? Your changes will be lost\");")

(defvar *customer-to-edit* nil
  "Optional parameter to pass to on-edit-customer-screen")
(defun on-edit-customer-screen (body conn)
  (setf (clog:visiblep body) nil)
  (clog:destroy-children body)
  (*let ((div (clog:create-div body :class "container"))
         (header (clog:create-element div "nav"))
         (header-list (clog:create-unordered-list header))
         (back (clog:create-button (clog:create-list-item header-list)
                                   :content "Back"
                                   :class "outline"))
         (clear (clog:create-button (clog:create-list-item header-list)
                                    :content "Clear"
                                    :class "outline"))
         (form-div (clog:create-form div :class "container"))
         (customer (or *customer-to-edit*
                       (a:when-let (saved (clog:storage-element
                                           (clog:window body) :local
                                           "active-customer-edit"))
                         (unless (string-equal saved "null")
                           (marshal:unmarshal (let ((*read-eval* nil))
                                                (read-from-string saved)))))
                       (make-instance 'tbl:customer)))
         (changes-made nil)
         save)
    ;; TODO a cleaner way of hiding the clear button
    (when (tbl:id customer)
      ;; Only allowing clearing new customers, not customers being edited
      (setf (clog:visiblep clear) nil))
    
    (open-orders.class-ui:class-ui (list :id :ignore) customer form-div)
    (setf save (clog:create-button div :content "Save"))
    (setf (clog:visiblep body) t)

    ;; autosave wip input
    (clog:set-on-change
     div
     (fn (obj)
       (setf changes-made t)
       (setf (clog:storage-element (clog:window body) :local
                                   "active-customer-edit")
             (format nil "~S" (marshal:marshal customer)))))

    (clog:set-on-click
     save
     (fn (obj)
       (cond ((null (tbl:id customer))
              (sql:exec-insert customer (db conn))
              (clog:storage-remove (clog:window body)
                                   :local "active-customer-edit")
              (page-back body conn))
             (t (sql:exec-update customer (db conn))
                (format t "Updated customer id ~a~%" (tbl:id customer))
                (clog:storage-remove (clog:window body)
                                     :local "active-customer-edit")
                (page-back body conn)))))
    
    (clog:set-on-click
     clear
     (fn (obj)
       (clog:storage-remove (clog:window body) :local "active-customer-edit")
       (on-edit-customer-screen body conn)))
    (clog:set-on-click
     back
     (fn (obj)

       (cond
         ;; save new input and go back
         ((null (tbl:id customer))
          (setf (clog:storage-element (clog:window body) :local
                                      "active-customer-edit")
                (format nil "~S" (marshal:marshal customer)))
          (page-back body conn))
         
         ;; or warn on changing old input
         ((or (not changes-made)
              (string-equal "true" (clog:js-query body *confirm-js*)))
          (clog:storage-remove (clog:window body) :local
                               "active-customer-edit")
          (page-back body conn)))))))

(defvar *material-to-edit* nil
  "Optional parameter to pass to on-edit-material-screen")
(defun on-edit-material-screen (body conn)
  (setf (clog:visiblep body) nil)
  (clog:destroy-children body)
  (*let ((div (clog:create-div body :class "container"))
         (header (clog:create-element div "nav"))
         (header-list (clog:create-unordered-list header))
         (back (clog:create-button (clog:create-list-item header-list)
                                   :content "Back"
                                   :class "outline"))
         (clear (clog:create-button (clog:create-list-item header-list)
                                    :content "Clear"
                                    :class "outline"))
         (form-div (clog:create-form div :class "container"))
         (material (or *material-to-edit*
                       (a:when-let (saved (clog:storage-element
                                           (clog:window body) :local
                                           "active-material-edit"))
                         (unless (string-equal saved "null")
                           (marshal:unmarshal (let ((*read-eval* nil))
                                                (read-from-string saved)))))
                       (make-instance 'tbl:material)))
         save)
    ;; TODO a cleaner way of hiding the clear button
    (when (tbl:id material)
      ;; Only allowing clearing new materials, not materials being edited
      (setf (clog:visiblep clear) nil))
    
    (open-orders.class-ui:class-ui (list :id :ignore) material form-div)
    (setf save (clog:create-button div :content "Save"))
    (setf (clog:visiblep body) t)

    ;; autosave wip input
    (clog:set-on-change
     div
     (fn (obj)
       (setf (clog:storage-element (clog:window body) :local
                                   "active-material-edit")
             (format nil "~S" (marshal:marshal material)))))

    (clog:set-on-click
     save
     (fn (obj)
       (cond ((null (tbl:id material))
              (sql:exec-insert material (db conn))
              (clog:storage-remove
               (clog:window body) :local "active-material-edit")
              (page-back body conn))
             (t (sql:exec-update material (db conn))
                (format t "Updated material id ~a~%" (tbl:id material))
                (clog:storage-remove
                 (clog:window body) :local "active-material-edit")
                (page-back body conn)))))
    
    (clog:set-on-click
     clear
     (fn (obj)
       (clog:storage-remove (clog:window body) :local "active-material-edit")
       (on-edit-material-screen body conn)))
    (clog:set-on-click
     back
     (fn (obj)
       (unless (tbl:id material)
         (setf (clog:storage-element (clog:window body) :local
                                     "active-material-edit")
               (format nil "~S" (marshal:marshal material))))
       (page-back body conn)))))

(defun on-customers-screen (body conn)
  (clog:destroy-children body)

  (loop
    :with div = (clog:create-div body :class "container")
    :with header = (clog:create-element div "nav")
    :with header-list = (clog:create-unordered-list header)
    :with back = (clog:set-on-click
                  (clog:create-button (clog:create-list-item header-list)
                                      :content "Back"
                                      :class "outline")
                  (fn (obj) (on-logged-in-screen body conn)))

    :with customers = (sql:exec-select-all 'tbl:customer (db conn))
    :with table = (clog:create-table div)
    :with table-head = (clog:create-table-head table)
    :with _ = (progn
                (clog:create-table-heading table-head :content "Name"))
    :for customer :in customers
    :for row = (clog:create-table-row div)
    :do
       (let ((customer customer))
         (clog:create-table-column row :content (tbl:name customer))
         (clog:create-table-column row :content (tbl:contact-email customer))
         (clog:set-on-click
          (clog:create-button (clog:create-table-column row)
                              :content "Edit")

          (fn (obj)
            (let ((*customer-to-edit* customer))
              (page-go body conn :from #'on-customers-screen
                                 :to #'on-edit-customer-screen)))))))

(defun on-materials-screen (body conn)
  (clog:destroy-children body)

  (loop
    :with div = (clog:create-div body :class "container")
    :with header = (clog:create-element div "nav")
    :with header-list = (clog:create-unordered-list header)
    :with back = (clog:set-on-click
                  (clog:create-button (clog:create-list-item header-list)
                                      :content "Back"
                                      :class "outline")
                  (fn (obj) (on-logged-in-screen body conn)))

    :with materials = (sql:exec-select-all 'tbl:material (db conn))
    :with table = (clog:create-table div)
    :with table-head = (clog:create-table-head table)
    :with _ = (progn
                (clog:create-table-heading table-head :content "Name"))
    :for material :in materials
    :for row = (clog:create-table-row div)
    :do
       (let ((material material))
         (clog:create-table-column row :content (tbl:name material))
         (clog:set-on-click
          (clog:create-button (clog:create-table-column row)
                              :content "Edit")

          (fn (obj)
            (let ((*material-to-edit* material))
              (page-go body conn :from #'on-materials-screen
                                 :to #'on-edit-material-screen)))))))

(defun  on-edit-open-order-screen (body conn)
  (clog:destroy-children body)
  (loop 
    :with div = (clog:create-div body :class "container")
    :with header = (clog:create-element div "nav")
    :with header-list = (clog:create-unordered-list header)
    :with back = (clog:set-on-click
                  (clog:create-button (clog:create-list-item header-list)
                                      :content "Back"
                                      :class "outline")
                  (fn (obj) (on-logged-in-screen body conn)))
    :repeat 0))


(defparameter *source-code-message*
  "The Source Code is Freely Available <a href=\"https://github.com/visendev/open-orders\" target=\"_blank\">Here</a>")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (ftype (function (pathname) string) make-b64-image))
  (defun make-b64-image (pathname)
    (with-open-file (fp pathname :element-type '(unsigned-byte 8))
      (let* ((bytes (loop :with bytes = (make-array 0
                                                    :adjustable t
                                                    :fill-pointer 0
                                                    :element-type '(unsigned-byte 8))
                          :repeat (file-length fp)
                          :do (vector-push-extend (read-byte fp) bytes)
                          :finally (return bytes)))
             (b64 (base64:usb8-array-to-base64-string bytes))
             (extension (pathname-type pathname)))
        (when (string-equal extension "svg")
          (setf extension "svg+xml"))
        (format nil "<img src=\"data:image/~a;base64, ~a\"/>" extension b64))))
  
  (defvar *lisp-logo-image*
    (make-b64-image (asdf:system-relative-pathname
                     "open-orders"
                     "static-files/lisp-lizard.svg"))))

(defun menu-bar-generate (body conn)
  (*let ((div (clog:create-div body :class "container"))
         (header (clog:create-element div "nav"))
         (header-list (clog:create-unordered-list header))
         (searchbar-list (clog:create-unordered-list header))
         (_searchbar (clog:create-form-element (clog:create-list-item searchbar-list)
                                               :search
                                               :placeholder "Search..."))
         (account-dropdown (clog:create-details (clog:create-list-item searchbar-list)
                                                :class "dropdown"))
         (_account-dropdown-summary (clog:create-summary account-dropdown
                                                         :content "More..."))
         (account-dropdown-list (clog:create-unordered-list account-dropdown))
         (new-dropdown (clog:create-details
                        (clog:create-list-item header-list)
                        :class "dropdown"))
         (_summary (clog:create-summary new-dropdown :content "New"))
         (new-dropdown-list (clog:create-unordered-list new-dropdown))
         (new-order-button (clog:create-button
                               (clog:create-list-item new-dropdown-list)
                               :content "New Order"
                               :class "outline"))
         (new-customer-button (clog:create-button
                               (clog:create-list-item new-dropdown-list)
                               :content "New Customer"
                               :class "outline"))
         (new-material-button (clog:create-button
                               (clog:create-list-item new-dropdown-list)
                               :content "New Material"
                               :class "outline"))
         (customers (clog:create-button (clog:create-list-item header-list)
                                        :class "outline"
                                        :content "Customers"))
         (materials (clog:create-button (clog:create-list-item header-list)
                                        :class "outline"
                                        :content "Materials"))
         (logout (clog:create-button (clog:create-list-item account-dropdown-list)
                                     :class
                                     "secondary"
                                     :content "Logout"))
         (about (clog:create-button (clog:create-list-item account-dropdown-list)
                                    :class "secondary"
                                    :content "About"))
         (about-modal (clog:create-dialog body))
         (about-body (clog:create-element about-modal "article"))
         (_1 (clog:create-section about-body
                                  :h1 :content "Open Orders"))
         (about-content (clog:create-div about-body))
         (_2 (clog:create-section
              about-content :p :content "Copyright 2026, Robert Wess Burnett."))
         (_3 (clog:create-section
              about-content :p :content "Licensed Under the GPL-3.0."))
         (_4 (clog:create-section
              about-content :p
              :content *source-code-message*))
         (_5 (clog:create-child about-content *lisp-logo-image*))
         (about-modal-done (clog:create-button (clog:create-element about-body "footer")
                                               :content "Done")))
    (clog:set-on-click about
                       (fn (obj)
                         (setf (clog:dialog-openp about-modal) t)))
    (clog:set-on-click about-modal-done (fn (obj)
                                          (setf (clog:dialog-openp about-modal) nil)))

    (clog:set-on-click new-order-button (fn (obj) (on-edit-open-order-screen body conn)))
    (clog:set-on-click new-customer-button
                       (fn (obj) (on-edit-customer-screen body conn)))
    (clog:set-on-click new-material-button
                       (fn (obj) (on-edit-material-screen body conn)))
    (clog:set-on-click customers (fn (obj) (on-customers-screen body conn)))
    (clog:set-on-click materials (fn (obj) (on-materials-screen body conn)))
    (clog:set-on-click logout
                       (lambda (obj)
                         (declare (ignore obj))
                         (clog-auth:remove-authentication-token body)
                         (on-login-screen body conn)))))

(defun on-logged-in-screen (body conn)
  (assert (user conn))
  (clog:destroy-children body)
  (menu-bar-generate body conn)
  (*let ((div (clog:create-div body :class "container"))
         (tbl (clog:create-table div :class "container"))
         (tbl-head (clog:create-table-head tbl))
         (open-orders (sql:exec-select-all 'tbl:open-order (db conn))))
    (clog:create-table-heading tbl-head :content "Open Orders")
    (loop :for order :in open-orders
          :do (clog:create-table-column (clog:create-table-row tbl) :content (tbl:part order)))))

(defun on-login-screen (body conn)
  
  ;; If valid authentication token is found, go to logged in screen
  (a:when-let (tok (clog-auth:get-authentication-token body))
    (setf (db conn) (tbl:database-connect))
    (a:when-let (found-user
                 (sql:exec-select 'tbl:user 'tbl:authentication-token tok
                                  (db conn)))
      (setf (user conn)
            found-user)
      (return-from on-login-screen 
        (on-logged-in-screen body conn))))
  
  (clog:destroy-children body)
  (*let ((div (clog:create-div body :class "container"))
         (header (clog:create-element div "nav"))
         (_login-msg (clog:create-section (clog:create-list-item
                                           (clog:create-unordered-list header))
                                          :h1
                                          :content "Login"))
         (form (clog:create-form div :class "container"))
         (lu  (clog:create-label form :content "Username: "))
         (user (clog:create-form-element form :string :label lu))
         (_ (clog:create-br form))
         (lp       (clog:create-label form :content "Password: "))
         (pass (clog:create-form-element form :password :label lp))
         (stay-logged-in-label (clog:create-label
                                form
                                :content "Stay Logged In On This Device?"))
         (stay-logged-in (clog:create-form-element
                          form :checkbox
                          :label stay-logged-in-label
                          :name "stay-logged-in"))
         (login (clog:create-button
                 div :content "Login" :style "margin-top:20px;"))
         (msg (clog:create-p
               div :content "" :style "padding:10px;color:red;")))

    (unless (db conn)
      (setf (db conn) (tbl:database-connect)))


    ;; Reset msg on keyboard input
    (clog:set-on-key-down form (fn (obj event)
                                 (setf (clog:inner-html msg) "")))
    
    (labels ((try-login (obj)
               (declare (ignorable obj))
               (let ((user-record
                       (handler-case
                           (sql:exec-select 'tbl:user 'tbl:name
                                            (clog:value user)
                                            (db conn))
                         (error (e) (clog:alert (clog:window body) e)))))
                 (cond
                   ((not user-record)
                    (setf (clog:inner-html msg) "Invalid Username"))
                   ((and user-record
                         (tbl:hash user-record)
                         (cl-pass:check-password
                          (clog:value pass) (tbl:hash user-record)))
                    (if (not (clog:checkedp stay-logged-in))
                        (clog-auth:remove-authentication-token body)

                        ;;else 
                        (let ((tok (authentication-token-create)))
                          (setf (tbl:authentication-token user-record) tok)
                          (setf (tbl:authentication-token-timestamp user-record)
                                (get-universal-time))
                          (sql:exec-update user-record (db conn))
                          (clog-auth:store-authentication-token
                           body tok)))
                    
                    ;; goto logged in screen
                    (setf (user conn) user-record)
                    (on-logged-in-screen body conn))
                   (t (setf (clog:inner-html msg) "Incorrect Password")))))
             (handle-keydown (obj event)
               (a:when-let (key (getf event :key))
                 (when (equal key "Enter")
                   (try-login obj)))))
      (clog:set-on-click login #'try-login)
      (clog:set-on-key-down pass #'handle-keydown)
      (clog:set-on-key-down user #'handle-keydown)
      (clog:set-on-key-down login #'handle-keydown))))


(defparameter *use-css* t)
(defparameter *use-external-css* nil)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *pico-css*
    (format nil "<style>~a</style>"
            (uiop:read-file-string
             (asdf:system-relative-pathname
              "open-orders" "static-files/pico.min.css")))))
(defparameter *pico-css-url*
  "https://cdn.jsdelivr.net/npm/@picocss/pico@2.1.1/css/pico.min.css")


(defun on-new-window (body)
  
  (let ((conn (make-instance 'connection)))
    ;; Load css
    (when *use-css*
      (if *use-external-css*
          (clog:load-css (clog:html-document body) *pico-css-url*)

          ;; otherwise use local cached version
          (clog:create-child (clog:head-element (clog:html-document body))
                             *pico-css*)))


    (clog:set-html-on-close body "<script>close();</script>")
    (setf (clog:title (clog:html-document body)) "Overhead")
    (clog:enable-clog-popup)            ; To allow browser popups

    ;; loading bar
    (clog:create-child body "<div aria-busy=\"true\"/>")

    (on-login-screen body conn)

    ;; Block until body has been closed
    (clog:run body)
    (when (db conn)
      (tbl:database-disconnect (db conn)))))


(defun test ()
  (clog:initialize #'on-new-window)
  (clog:open-browser))
