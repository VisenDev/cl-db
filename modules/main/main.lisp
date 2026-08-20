(defpackage #:open-orders.main
  (:use #:cl #:open-orders.pagen #:open-orders.sql-table))
(in-package #:open-orders.main)

(defparameter *auth-cookie* "AUTH_TOKEN")
(defun perform-auth-check ()
  (let ((token (hunchentoot:cookie-in *auth-cookie*)))
    (unless token
      (hunchentoot:redirect "/login"))))

(hunchentoot:define-easy-handler (login :uri "/login") (username password)

  
  (setf (hunchentoot:content-type*) "text/html")
  (html ()
    (head ()
      (title () "Campro Login"))
    (body ()
      (h1 () "Campro Login")
      (when (or username password)
        (p () "Login not implemented yet :("))
      (table ()
        (form (:method "POST" :action "/login")
          (tr ()
            (td () (label (:label-for "username") "Username"))
            (td () (input (:type "text" :name "username"))))
          (tr ()
            (td () (label (:label-for "password") "Password"))
            (td () (input (:type "password" :name "password"))))
          (tr ()
            (td () (input (:type "submit") ""))))))))

(hunchentoot:define-easy-handler (index :uri "/") ()
  (perform-auth-check)
  
  (setf (hunchentoot:content-type*) "text/html")
  (html ()
    (head ()
      (title () "Hello World"))
    (body ()
      (h1 () "Hello There")
      (p () "Primary content goes here :)"))))


