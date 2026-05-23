(uiop:define-package #:open-orders.auth
  (:use #:cl)
  (:import-from #:open-orders.utils
                #:fn)
  (:import-from #:defclass-std
                #:defclass/std
                #:class/std)
  (:local-nicknames (#:a #:alexandria)
                    (#:tbl #:open-orders.tables)
                    (#:sql #:open-orders.sql-table)
                    (#:class-ui #:open-orders.class-ui))
  (:export
   #:logout
   #:test-credentials
   #:login-form
   #:username
   #:password
   #:stay-logged-in
   #:auth-token-get
   #:get-logged-in-user))
(in-package #:open-orders.auth)
(declaim (optimize (debug 3)))

(declaim (ftype (function () string) auth-token-create))
(defun auth-token-create ()
  "Create a unique token used to associate a browser with a user"
  (crypto:byte-array-to-hex-string
   (crypto:random-data 16)))

(declaim (ftype (function (clog:clog-body) (or null string)) auth-token-get))
(defun auth-token-get (body)
  "Returns any stored auth tokens from a page, or redirects to 
  login url and return nil"
  (clog-auth:get-authentication-token body))

(defun get-logged-in-user (body db login-url)
  "Get logged in tbl:user or return nil and redirect to login-url"
  (let ((user
          (sql:exec-select 'tbl:user 'tbl:authentication-token
                           (auth-token-get body)
                           db)))

    
    (unless user
      (format t "Failed login... redirecting~%")
      (clog:url-assign (clog:location body) login-url))
    
    user))

(class/std login-form
  username password)

(declaim
 (ftype (function (clog:clog-body dbi:dbi-connection login-form) t)
  test-credentials))
(defun test-credentials (body database-conn login-form)
  "Attempt to perform login using form, storing auth token on success"

  (format t "Testing creds: ~a ~a~%"
          (username login-form) (password login-form))
  
  ;; Attempt to retreive user record using login-form
  (let ((user-record
          (handler-case
              (sql:exec-select 'tbl:user 'tbl:name
                               (username login-form)
                               database-conn)
            (error (e) (clog:alert (clog:window body) e)))))

    (format t "Found user record: ~a~%" user-record)

    ;; Check if user record is not null and matches the password
    (let ((valid
            (and user-record
                 (tbl:hash user-record)
                 (cl-pass:check-password
                  (password login-form)
                  (tbl:hash user-record)))))

      ;; Store auth token if valid
      (when valid
        (format t "Valid password...~%~%")
        (let ((tok (authentication-token-create)))
          (setf (tbl:authentication-token user-record) tok)
          (sql:exec-update user-record database-conn)
          (clog-auth:store-authentication-token
           body tok)))

      ;; Finally return result
      valid)))


(declaim (ftype (function (clog:clog-body) t) logout))
(defun logout (body)
  (clog-auth:remove-authentication-token body)
  (clog:url-replace (clog:location body) "/login"))
