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
  (let ((header (clog:create-div body :style "display:flex;flex-direction:row;")))
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
   (require-login-p :type boolean :std t)))

(defun make-page (&rest args &key name action url show-menu-p require-login-p)
  (declare (ignore name action url show-menu-p require-login-p))
  (apply #'make-instance 'page args))

(defparameter *pages*
  (list (make-page :name "Logout"
                   :action (fn (body) (auth:logout body (auth:get-connection body))))
        (make-page :name "Login"
                   :action 'on-login
                   :require-login-p nil
                   :show-menu-p nil)
        (make-page :name "Home"
                   :action 'on-home
                   :url "/home")
        (make-page :name "About"
                   :action 'on-about
                   :url "/about")))

(defun find-page (name)
  "Lookup a pages definition by name"
  (find name *pages* :key #'name))

(defun initialize-page-content (body page)
  "Get connection, destroy children, update url, and create a menu bar"
  (let ((conn (auth:get-logged-in-connection body)))
    (unless conn
      (return-from initialize-page-content))
    (clog:destroy-children body)
    (when (show-menu-p page)
      (create-menu-bar body (loop :for p :in *pages*
                                  :appending (list (name p) (action p)))))
    (when (url page)
      (clog:url-push-state (clog:window body) (url page)))
    conn))

(defun on-about (body)
  (let ((conn (initialize-page-content body (find-page "About"))))
    (declare (ignorable conn))
    (clog:create-p body :content "Written By Wess Burnett")))

(defun on-home (body)
  (let ((conn (initialize-page-content body (find-page "Home"))))
    (declare (ignorable conn))
    (clog:create-p body :content "Home")))


(defun on-login (body &aux conn)
  (setf conn (auth:get-connection body))
  (clog:destroy-children body)

  (let* ((tok (clog-auth:get-authentication-token body))
         (found-user
           (when tok (sql:exec-select 'tbl:user 'tbl:authentication-token tok
                                      (tbl:db conn)))))

    (when found-user
      (setf (tbl:user conn)
            found-user)
      (clog:url-rewrite (clog:window body) "/home")
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
      (clog:set-on-click (clog:create-button body :content "Submit")
                         (fn (obj)
                           (class-ui:finalize-values ui)
                           (let ((login-successful-p
                                   (auth:test-credentials body conn instance)))
                             (cond
                               (login-successful-p
                                (clog:url-rewrite (clog:window body) "/home")
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
