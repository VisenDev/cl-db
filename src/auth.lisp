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
   #:stay-logged-in))
(in-package #:open-orders.auth)

(declaim (optimize (debug 3)))

(defun authentication-token-create ()
  "Create a unique token used to associate a browser with a user"
  (crypto:byte-array-to-hex-string
   (crypto:random-data 16)))

(class/std login-form username password stay-logged-in)

(defun test-credentials (body conn login-form)
  (let ((user-record
          (handler-case
              (sql:exec-select 'tbl:user 'tbl:name
                               (username login-form)
                               (tbl:db conn))
            (error (e) (clog:alert (clog:window body) e)))))
    (let ((valid
            (and user-record
                 (tbl:hash user-record)
                 (cl-pass:check-password
                  (password login-form)
                  (tbl:hash user-record)))))
      (when valid
        (setf (tbl:user conn) user-record)
        (if (stay-logged-in login-form)
            
            ;; then
            (let ((tok (authentication-token-create)))
              (setf (tbl:authentication-token user-record) tok)
              (sql:exec-update user-record (tbl:db conn))
              (clog-auth:store-authentication-token
               body tok))

            ;;else 
            (clog-auth:remove-authentication-token body))
        valid))))


(defun logout (body conn)
  (clog-auth:remove-authentication-token body)
  (setf (tbl:user conn) nil)
  (clog:url-replace (clog:location body) "/"))

;; (declaim (ftype (function (clog:clog-body) (or null tbl:connection))
;;                 get-connection))
;; (defun get-connection (body)
;;   (let ((conn (clog:connection-data-item body "conn")))
;;     (unless conn
;;       (clog:url-replace (clog:location body) "/"))
;;     conn))

;; (declaim (ftype (function (clog:clog-body) (or null tbl:connection))
;;                 get-logged-in-connection))
;; (defun get-logged-in-connection (body)
;;   "Like get-connection but it ensures the user is logged in"
;;   (let ((conn (get-connection body)))
;;     (when conn
;;       (unless (tbl:user conn)
;;         (clog:url-replace (clog:location body) "/")))
;;     conn))
