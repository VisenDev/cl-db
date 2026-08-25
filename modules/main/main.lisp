(defpackage #:open-orders.main
  (:use #:cl
        #:open-orders.pagen
        #:open-orders.sql-table
        #:open-orders.tables)
  (:export
   #:main))
(in-package #:open-orders.main)

(defclass-std:class/std app db acceptor)
(defvar *app* nil)


(defparameter *auth-cookie* "AUTH_TOKEN")
(defun perform-auth-check ()
  (let ((token (hunchentoot:cookie-in *auth-cookie*)))
    (unless token
      (hunchentoot:redirect "/login"))
    (let ((user (exec (db *app*) (select 'user 'authentication-token token))))
      (unless user
        (hunchentoot:redirect "/login")))))

(hunchentoot:define-easy-handler (css :uri "/orders.css") ()
  (setf (hunchentoot:content-type*) "text/css")
  #.(uiop:read-file-string (asdf:system-relative-pathname "open-orders.main"
                                                          "orders.css")))

(defmacro link-bar (&rest name-href-pairs)
  `(table ()
     (tr ()
       ,@(loop :for name-href :in name-href-pairs
              :for name = (if (listp name-href) (first name-href)
                              name-href)
              :for href = (if (listp name-href) (second name-href)
                              `(format nil "/~a" ,name-href))
              :collect `(td ()
                         (a (:href ,href) ,name))))))

(defmacro toplevel-tab-bar ()
  `(link-bar "logout" "open-orders" "customers" "inventory"))

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

(defmacro with-internal-page (&body body)
  `(progn
     (perform-auth-check)
     (with-page
       (h1 () "Campro Open Orders")
       (toplevel-tab-bar)
       (hr () )
       ,@body)))

(declaim (ftype (function () string) auth-token-create))
(defun auth-token-create ()
  "Create a unique token used to associate a browser with a user"
  (crypto:byte-array-to-hex-string
   (crypto:random-data 16)))

(hunchentoot:define-easy-handler (logouyt :uri "/logout") ()
  (hunchentoot:set-cookie *auth-cookie* :value nil)
  (hunchentoot:redirect "/login"))

(hunchentoot:define-easy-handler (login :uri "/login") ((username :init-form nil)
                                                        (password :init-form nil))

  (let ((errmsg nil))

    ;; Attempt login
    (when (or username password)
      (let ((user (exec (db *app*) (select 'user 'name username))))
        (cond
          ;; Success
          ((and user (cl-pass:check-password password (hash user)))
           (setf (authentication-token user) (auth-token-create))
           (exec (db *app*) (update user))
           (hunchentoot:set-cookie *auth-cookie*
                                   :value (authentication-token user))
           (hunchentoot:redirect "/"))

          ;; User found but wrong password
          (user
           (setf errmsg "Incorrect Password"))

          ;; Incorrect username and password
          (t
           (setf errmsg "Unknown Username")))))

    ;; Display login screen
    (setf (hunchentoot:content-type*) "text/html")
    (with-page
      (h1 () "Campro Login")
      (when errmsg
        (p () errmsg))
      (form (:method "POST" :action "/login")
        (table ()
          (tr ()
            (td () (label (:for "username") "Username"))
            (td () (input (:type "text" :name "username"
                           :id "username" :autocomplete "on"))))
          (tr ()
            (td () (label (:for "password") "Password"))
            (td () (input (:type "password" :name "password"
                           :id "password" :autocomplete "on"))))
          (tr ()
            (td () (input (:type "submit") ""))))))))

(hunchentoot:define-easy-handler (index :uri "/") ()
  (perform-auth-check)
  (hunchentoot:redirect "/open-orders"))



(defun get-open-orders-sorted-by (sort-by)
  (let ((raw (exec (db *app*) (select-all 'open-order))))
    (if (null sort-by)
        raw
        ;; else
        (sort
         raw 
         (alexandria:switch (sort-by :test #'string=)
           ("part"
            (lambda (lhs rhs)
              (string-lessp
               (ignore-errors
                (part-number (exec (db *app*) (select 'part 'id (part lhs)))))
               (ignore-errors
                (part-number (exec (db *app*) (select 'part 'id (part rhs))))))))
           ("purchase-order"
            (lambda (lhs rhs)
              (string-lessp (purchase-order lhs)
                            (purchase-order rhs))))
           ("due-date"
            (lambda (lhs rhs)
              (< (open-order-deadline lhs)
                 (open-order-deadline rhs))))
           ("line-item"
            (lambda (lhs rhs)
              (< (line-item lhs)
                 (line-item rhs)))))))))

(hunchentoot:define-easy-handler (open-orders :uri "/open-orders") (sort-by)
  (with-internal-page
    (table ()
      (tr ()
        (th () (a (:href "/open-orders?sort-by=part" ) "Part Number"))
        (th () (a (:href "/open-orders?sort-by=purchase-order" ) "Po Number"))
        (th () (a (:href "/open-orders?sort-by=due-date" ) "Due Date"))
        (th () (a (:href "/open-orders?sort-by=line-item" ) "Line Item"))
        (th () (a (:href "/new?type=order") (button () "New"))))
      (macrolet
          ((with-link-to-edit-url (&body body)
             `(a (:href (format nil "/edit-order?id=~a" (id order)))
                ,@body)))
        (mapcar (lambda (order)

                  (tr ()
                    
                    (td ()
                      (with-link-to-edit-url
                          (ignore-errors
                           (part-number
                            (exec (db *app*)
                                  (select 'part 'id (part order)))))))
                    (td () (with-link-to-edit-url (purchase-order order)))
                    (td () (with-link-to-edit-url "TODO"))
                    (td () (with-link-to-edit-url (line-item order)))))
                (get-open-orders-sorted-by sort-by))))))

(hunchentoot:define-easy-handler (new :uri "/new") (type)
  (with-internal-page 
    (alexandria:switch (type :test #'string=)
      ("order"
       (let ((id (exec (db *app*)
                       (insert (make-instance
                                'open-order
                                :purchase-order (auth-token-create))))))
         (hunchentoot:redirect (format nil "/edit-order?id=~a" id))))
      (otherwise
       (h1 () "Error: don't know how to make a new '~a'" type)))))

(defmacro when-assoc ((varname key alist) &body body)
  (alexandria:with-gensyms (keyval value)
    `(let* ((,keyval ,key)
            (,value (assoc ,key ,alist :test #'string-equal))
            (,varname (cdr ,value)))
       (declare (ignorable ,value ,keyval ,varname))
       (when ,varname
         ,@body))))

(hunchentoot:define-easy-handler
    (save-order-po-details :uri "/save-order") (id tab)

  (perform-auth-check)
  
  (let ((params (hunchentoot:post-parameters*))
        (order (exec (db *app*) (select 'open-order 'id (parse-integer id)))))

    (print "Looking for order")
    (terpri)
    (when order
      (print "Order found!")
      (terpri)

      (print params)
      (terpri)

      ;; Customer Code
      (when-assoc (customer-code 'customer-code params)
        (let ((customer (exec (db *app*) (select 'customer 'name customer-code))))
          (if customer
              
              ;;customer found, update id
              (progn
                (setf (customer order) (id customer))
                (print "customer found")
                (terpri))

              ;; else create new customer
              (let* ((new-customer (make-instance 'customer
                                                  :name customer-code))
                     (new-customer-id (exec (db *app*)
                                            (insert new-customer))))
                (print "Creating a new customer")
                (terpri)
                (setf (customer order) new-customer-id)))))

      ;; finally save order and redirect
      (exec (db *app*) (update order)))

    ;; Redirect back to app
    (hunchentoot:redirect (format nil "/edit-order?id=~a&tab=~a"
                                  id tab)
                          :code 303)))

(hunchentoot:define-easy-handler (edit-order :uri "/edit-order") (id tab)
  ;; redirect to po-details tab
  (format t "id=~S,tab=~S~%" id tab)
  (let ((order (exec (db *app*) (select 'open-order 'id (parse-integer id)))))
    (flet ((tab-link (tab)
             (format nil "/save-order?id=~a&tab=~a" id tab)))
      (unless tab
        (hunchentoot:redirect (tab-link "po-details")))
      (with-internal-page
        (link-bar ("po-details" (tab-link "po-details"))
                  ("job-ticket" (tab-link "job-ticket"))
                  ("shipping-details" (tab-link "shipping-details"))
                  ("certificate-of-conformance"
                   (tab-link "certificate-of-conformance")))
        (h1 ()
          (hunchentoot:post-parameters*))
        (form (:method "POST" :action (tab-link "po-details"))
          (table ()
            (tr ()
              (td () (input (:type "submit"))))
            (tr ()
              (td ()
                (table ()
                  (tr ()
                    (td () "Customer Code")
                    (td ()
                      (input (:type "text" :name "customer-code"
                              :id "customer-code" :list "customer-code-list"
                              :value (ignore-errors
                                      (or
                                       (name
                                        (or (exec (db *app*)
                                                  (select 'customer 'id
                                                          (customer order)))))
                                       "")))))
                    (datalist (:id "customer-code-list")
                      (mapcar #'name (exec (db *app*)
                                           (select-all 'customer)))))
                  (tr ()
                    (td () "Customer Name")
                    (td () (a (:href "/edit-customer?id=TODO"
                               :style "padding:none;")
                             (button () "View Customer Information")))))
                ))
            )
          )
        ))))

(hunchentoot:define-easy-handler (customers :uri "/customers") ()
  (with-internal-page
    (p () "Customers Page")
    (h3 () "Primary content goes here :)")))

(hunchentoot:define-easy-handler (inventory :uri "/inventory") ()
  (with-internal-page
    (p () "Inventory Page")
    (h3 () "Primary content goes here :)")))

(defun start ()
  (unless *app*
    (setf *app*
          (make-instance
           'app
           :db (open-orders.tables:database-connect)
           :acceptor (make-instance 'hunchentoot:easy-acceptor :port 8000))))
  (hunchentoot:start (acceptor *app*)))

(defun stop ()
  (when *app*
    (when (db *app*)
      (open-orders.tables:database-disconnect (db *app*))
      (setf (db *app*) nil))
    (when (acceptor *app*)
      (hunchentoot:stop (acceptor *app*))))
  (setf *app* nil))


(defun main ()
  (start)
  (unwind-protect (loop (sleep 1))
    (stop)))
