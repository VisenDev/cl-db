(defpackage #:open-orders.main
  (:use #:cl
        #:open-orders.pagen
        #:open-orders.sql-table
        #:open-orders.tables)
  (:export
   #:main))
(in-package #:open-orders.main)

(defvar *db* nil)
(defparameter *auth-cookie* "AUTH_TOKEN")
(defun perform-auth-check ()
  (let ((token (hunchentoot:cookie-in *auth-cookie*)))
    (unless token
      (hunchentoot:redirect "/login"))
    (let ((user (exec *db* (select 'user 'authentication-token token))))
      (unless user
        (hunchentoot:redirect "/login")))))

(hunchentoot:define-easy-handler (css :uri "/orders.css") ()
  (setf (hunchentoot:content-type*) "text/css")
  #.(uiop:read-file-string (asdf:system-relative-pathname "open-orders.main"
                                                          "orders.css")))

(defmacro with-page (&body body)
  `(doctype ()
     (html ()
       (head ()
         (title () "Open Orders")
         (meta (:charset "utf-8"))
         (link (:href "/orders.css" :rel "stylesheet")))
       (body ()
         ,@body))))

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
      (let ((user (exec *db* (select 'user 'name username))))
        (cond
          ;; Success
          ((and user (cl-pass:check-password password (hash user)))
           (setf (authentication-token user) (auth-token-create))
           (exec *db* (update user))
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

(defun insert-tab-bar ()
  (let ((pages '("logout" "open-orders" "customers" "inventory")))
    (table ()
      (tr ()
        (loop :for page :in pages
              :collect (td ()
                         (a (:href (format nil "~a" page)) page)))))))

(hunchentoot:define-easy-handler (open-orders :uri "/open-orders") ()
  (perform-auth-check)
  (setf (hunchentoot:content-type*) "text/html")
  (with-page
    (h1 () "Campro Open Orders")
    (insert-tab-bar)
    (p () "Open Orders Page")
    (h3 () "Primary content goes here :)")))

(hunchentoot:define-easy-handler (customers :uri "/customers") ()
  (perform-auth-check)
  (setf (hunchentoot:content-type*) "text/html")
  (with-page
    (h1 () "Campro Open Orders")
    (insert-tab-bar)
    (p () "Customers Page")
    (h3 () "Primary content goes here :)")))

(hunchentoot:define-easy-handler (inventory :uri "/inventory") ()
  (perform-auth-check)
  (setf (hunchentoot:content-type*) "text/html")
  (with-page
    (h1 () "Campro Open Orders")
    (insert-tab-bar)
    (p () "Inventory Page")
    (h3 () "Primary content goes here :)")))


(defun main ()
  (unless *db*
    (setf *db* (open-orders.tables:database-connect)))
  (let ((acc (make-instance 'hunchentoot:easy-acceptor :port 8000)))
    (unwind-protect
         (progn
           (hunchentoot:start acc)
           ;; sleep while waiting for connections
           (loop (sleep 1)))
      (hunchentoot:stop acc))))
