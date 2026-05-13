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
   #:setup-popstate-handler
   #:setup-page-urls))
(in-package #:open-orders.pages)

(declaim (optimize (debug 3)))

(defun create-menu-bar (body name-action-plist)
  (let ((header (clog:create-div
                 body :style "display:flex;flex-direction:row;")))
    (a:doplist (name action name-action-plist)
      (clog:set-on-click (clog:create-button header :content name)
                         (let ((action action))
                           (fn (obj)
                             (funcall action body)))))))



(defclass/std page ()
  ((name :type string)
   (action)
   (url :std nil)
   (show-menu-p :type boolean :std t)
   (menu-button-p :type boolean :std t)
   (login-required-p :type boolean :std t)))

(defparameter *pages*
  (list (make-instance 'page :name "Logout"
                             :action (fn (body)
                                       (auth:logout
                                        body (clog:connection-data-item
                                              body "conn"))))
        (make-instance 'page :name "Login"
                             :action 'on-login
                             :login-required-p nil
                             :show-menu-p nil
                             :url "/login"
                             :menu-button-p nil)
        (make-instance 'page :name "Home"
                             :action 'on-home
                             :url "/home")
        (make-instance 'page :name "About"
                             :action 'on-about
                             :url "/about")
        (make-instance 'page :name "Customers"
                             :action 'on-customers
                             :url "/customers")))

(defun find-page (name)
  "Lookup a pages definition by name"
  (find name *pages* :key #'name :test 'equal))

(declaim (ftype (function (clog:clog-body page)
                          (or null tbl:connection))
                initialize-page-content))

(defun initialize-page-content (body page)
  "Get connection, destroy children, update url, and create a menu bar"

  (let ((conn (clog:connection-data-item body "conn")))

    ;; Create root connection if needed
    (unless conn
      (clog:url-replace (clog:location body) "/")
      (return-from initialize-page-content))

    ;; Redirect to login if needed
    (when (login-required-p page)
      (unless (tbl:user conn)
        (on-login body)
        (return-from initialize-page-content)))

    ;; Setup Page content
    (clog:destroy-children body)
    (when (show-menu-p page)
      (create-menu-bar body (loop :for p :in *pages*
                                  :when (menu-button-p p)
                                    :appending (list (name p) (action p)))))

    ;; Rename Url
    (when (url page)
      (clog:url-rewrite (clog:window body) (url page)))
    conn))




(defun on-about (body)
  (a:when-let (conn (initialize-page-content body (find-page "About")))
    (clog:create-p body :content "Written By Wess Burnett")
    (clog:create-p body :content "Copyright 2026")
    (clog:create-p body :content "Licensed under the GPL-3.0")))

(defun on-edit (body)
  (a:when-let (conn (initialize-page-content body (find-page "About")))
    (clog:create-p body :content "TODO")))

(defun on-customers (body)
  (a:when-let (conn (initialize-page-content body (find-page "Customers")))
    (let ((customers (sql:exec-select-all 'tbl:customer (tbl:db conn)))
          (tbl (clog:create-table body)))
      (dolist (c customers)
        (let* ((row (clog:create-table-row tbl))
               (btn (clog:create-p (clog:create-table-column row)
                                        :content (tbl:name c)
                                        :style "cursor:pointer;")))
          (clog:set-on-mouse-over
           btn (fn (obj)
                 (setf (clog:style btn "text-decoration")
                       "underline")))
          (clog:set-on-mouse-leave
           btn (fn (obj)
                 (setf (clog:style btn "text-decoration")
                       "none")))
          (clog:set-on-click
           btn
           (let ((c c))
             (declare (ignore c))
             (fn (obj)
               ;; TODO finish this
               (on-edit body))))
          )
        )
      )
    )
  )

(defun on-home (body)
  (a:when-let (conn (initialize-page-content body (find-page "Home")))
    (clog:create-p body :content "Home")))


(defun on-login (body &aux conn)
  (setf conn (initialize-page-content body (find-page "Login")))

  (let* ((tok (clog-auth:get-authentication-token body))
         (found-user
           (when tok
             (sql:exec-select 'tbl:user 'tbl:authentication-token tok
                              (tbl:db conn)))))

    (when found-user
      (setf (tbl:user conn)
            found-user)
      (return-from on-login (on-home body)))

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
      (clog:set-on-click
       (clog:create-button body :content "Submit")
       (fn (obj)
         (class-ui:finalize-values ui)
         (let ((login-successful-p
                 (auth:test-credentials body conn instance)))
           (cond
             (login-successful-p
              (on-home body))
             (t
              (setf (clog:inner-html msg) "Login Failed")))))))))

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

(defun setup-page-urls ()
  (dolist (page *pages*)
    (when (url page)
      (let ((page page))
        (clog:set-on-new-window
         (lambda (body)
           (funcall (action page) body))
         :path (url page))))))

(defun setup-popstate-handler (body)
  (set-on-popstate
   body (fn (page-url)
          (let ((page (find page-url *pages* :key #'url)))
            (if page
                (funcall (action page) body)
                (funcall (action (find-page "Login")) body))))))
