(uiop:define-package #:open-orders.pages
  (:use #:cl)
  (:import-from #:open-orders.utils
                #:fn)
  (:import-from #:defclass-std
                #:defclass/std
                #:class/std)
  (:local-nicknames (#:a #:alexandria)
                    (#:tbl #:open-orders.tables)
                    (#:sql #:open-orders.sql-table)
                    (#:class-ui #:open-orders.class-ui)
                    (#:auth #:open-orders.auth))
  (:export
   #:on-home
   #:on-login
   #:setup-popstate-handler))
(in-package #:open-orders.pages)

(defun on-home (body &aux conn)
  (setf conn (clog:connection-data-item body "conn"))
  ;; TODO check if user is logged in here
  
  (clog:destroy-children body)
  
  (clog:set-on-click (clog:create-button body :content "Logout")
                     (fn (obj)
                       (auth:logout body)
                       (clog:url-push-state (clog:window body) "/")
                       (on-login body)))
  (clog:create-p body :content "Home")
  )


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
      (on-home body))

    ;; OTHERWISE LOGIN NORMALLY
    (let*
        ((instance (make-instance 'auth:login-form))
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
                           (if (auth:test-credentials body conn instance)
                               (progn
                                 (clog:url-push-state
                                  (clog:window body) "/home")
                                 (on-home body))
                               (setf (clog:inner-html msg) "Login Failed")
                               ))))))

(defun set-on-popstate (body handler)
  
  (setf (gethash "body:popstate" (clog:connection-data body))
        handler)

  (clog:js-execute
   body
   "
window.addEventListener('popstate', function(e) {
  ws.send(
    'E:body:popstate ' + window.location.pathname
  );
});
"))

(defun popstate-handler (body page)
  (a:eswitch (page :test 'string-equal)
    ("/" (on-login body))
    ("/home" (on-home body))))


(defun setup-popstate-handler (body)
  (set-on-popstate
   body (a:curry #'popstate-handler body)))
