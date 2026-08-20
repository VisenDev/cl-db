(defpackage #:open-orders.main
  (:use #:cl #:open-orders.pagen #:open-orders.sql-table)
  (:export
   #:main))
(in-package #:open-orders.main)

(defparameter *auth-cookie* "AUTH_TOKEN")
(defun perform-auth-check ()
  (let ((token (hunchentoot:cookie-in *auth-cookie*)))
    (unless token
      (hunchentoot:redirect "/login"))

    ;; TODO check validity of auth token
    ))

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

(hunchentoot:define-easy-handler (login :uri "/login") ((username :init-form nil)
                                                        (password :init-form nil))

  ;; TODO setup proper auth
  (when (or username password)
    (hunchentoot:set-cookie *auth-cookie* :value "TODO")
    (hunchentoot:redirect "/"))
  
  (setf (hunchentoot:content-type*) "text/html")
  (with-page
    (h1 () "Campro Login")
    (table ()
      (form (:method "POST" :action "/login")
        (tr ()
          (td () (label (:label-for "username") "Username"))
          (td () (input (:type "text" :name "username"))))
        (tr ()
          (td () (label (:label-for "password") "Password"))
          (td () (input (:type "password" :name "password"))))
        (tr ()
          (td () (input (:type "submit") "")))))))

(hunchentoot:define-easy-handler (index :uri "/") ()
  (perform-auth-check)
  
  (setf (hunchentoot:content-type*) "text/html")
  (html ()
    (head ()
      (title () "Hello World"))
    (body ()
      (h1 () "Hello There")
      (p () "Primary content goes here :)"))))


(defun main ()
  (let ((acc (make-instance 'hunchentoot:easy-acceptor :port 8000)))
    (hunchentoot:start acc))
  ;; sleep
  (loop (sleep 1)))
