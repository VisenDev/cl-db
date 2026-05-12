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
   #:on-login
   #:logout))
(in-package #:open-orders.auth)

(defun authentication-token-create ()
  "Create a unique token used to associate a browser with a user"
  (crypto:byte-array-to-hex-string
   (crypto:random-data 16)))

(class/std login-form username password stay-logged-in)

(defun test-credentials (body conn login-form msg)
  (let ((user-record
          (handler-case
              (sql:exec-select 'tbl:user 'tbl:name
                               (username login-form)
                               (tbl:db conn))
            (error (e) (clog:alert (clog:window body) e)))))
    (cond
      ((not user-record)
       (setf (clog:inner-html msg) "Invalid Username"))
      ((and user-record
            (tbl:hash user-record)
            (cl-pass:check-password
             (password login-form)
             (tbl:hash user-record)))
       (if (not (stay-logged-in login-form))
           (clog-auth:remove-authentication-token body)

           ;;else 
           (let ((tok (authentication-token-create)))
             (setf (tbl:authentication-token user-record) tok)
             (setf (tbl:authentication-token-timestamp user-record)
                   (get-universal-time))
             (sql:exec-update user-record (tbl:db conn))
             (clog-auth:store-authentication-token
              body tok)))
       
       ;; goto logged in screen
       (setf (tbl:user conn) user-record)
       (clog:url-push-state (clog:window body) "/home")
       (open-orders.home:on-home body))
      (t (setf (clog:inner-html msg) "Incorrect Password")))))

(defun on-login (body &aux conn)
  (setf conn (clog:connection-data-item body "conn"))
  (clog:destroy-children body)

  ;; CHECK FOR AUTH TOKEN
  (a:if-let (tok (clog-auth:get-authentication-token body))

    ;; LOGIN USING AUTH TOKEN
    (a:when-let (found-user
                 (sql:exec-select 'tbl:user 'tbl:authentication-token tok
                                  (tbl:db conn)))
      (setf (tbl:user conn)
            found-user)
      (clog:url-push-state (clog:window body) "/home")
      (open-orders.home:on-home body))

    ;; OTHERWISE LOGIN NORMALLY
    (let*
        ((instance (make-instance 'login-form))
         (ui (class-ui:class-ui
              (list :username (make-instance 'class-ui:config/text
                                             :label "Username")
                    :password (make-instance 'class-ui:config/password
                                             :label "Password")
                    :stay-logged-in (make-instance 'class-ui:config/toggle))
              instance body))
         (msg (clog:create-p body :content "")))
      (clog:set-on-click (clog:create-button body :content "Submit")
                         (fn (obj)
                           (class-ui:finalize-values ui)
                           (test-credentials body conn instance msg))))))


(defun logout (body)
  (clog-auth:remove-authentication-token body)
  (clog:url-push-state (clog:window body) "/")
  (on-login body))
