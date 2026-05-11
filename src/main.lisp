(defpackage #:open-orders.main
  (:use #:cl #:clog)
  (:import-from #:defclass-std
                #:defclass/std
                #:class/std)
  (:local-nicknames (#:a #:alexandria)
                    (#:tbl #:open-orders.tables)
                    (#:sql #:open-orders.sql-table))
  (:export #:main))
(in-package #:open-orders.main)

(defun init-site (body)
  (clog-web:clog-web-initialize body)
  (setf (title (html-document body)) "Open Orders")
  (clog-web:create-web-site body
                   ;; use the default theme
                   :theme 'clog-web:default-theme
                   ;; theme settings - in this case w3.css color of menu bar
                   :settings '(:color-class "w3-gray")
                   ;; :title "Open Orders"
                   :footer "(c) 2026 Wess Burnett"
                   ;; :logo "/lisp-lizard.svg"
                   ))

;; This is the menu structure
(defparameter *menu* `(("Content" (("Open Orders"         "/home"       on-open-orders)
                                   ("Content from Lambda" "/lambda" on-lambda)
                                   ("Content from File"   "/readme" on-readme)))
                       ("Help"    (("About"               "/about"  on-about)))
                       ("Logout" ())))

;; Page handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; / -  a simple content string
(defun on-open-orders (body)
  (unless (clog:connection-data-item body "conn")
    (clog:url-replace (clog:location body) "/login"))
  ;; We call init-site on every page to load our theme and settings
  (init-site body)
  (clog-web:create-web-page body :main `(:menu    ,*menu*
                                :content "<b>Welcome to tutorial 30</b><p>Any HTML works!")))

;; /readme - get content from a text file
(defun on-readme (body)
  (init-site body)
  (let ((readme (alexandria:read-file-into-string
                 (format nil "~A~A" (asdf:system-source-directory :clog) "README.md"))))
    (clog-web:create-web-page body :main `(:menu    ,*menu*
                                  :content ,(format nil "<pre>~A</pre>" readme)))))

;; /lambda - use a function to output to the page content
(defun on-lambda (body)
  (unless (clog:connection-data-item body "conn")
    (clog:url-replace (clog:location body) "/login"))
  (init-site body)
  (clog-web:create-web-page body :main `(:menu    ,*menu*
                                :content ,(lambda (obj)
                                            (create-div obj :content "I am in the content area")))))

(defun on-login (body)
  (let ((conn (clog:connection-data-item body "conn")))
    (unless conn
      (setf conn (make-instance 'tbl:connection))
      (setf (tbl:db conn) (tbl:database-connect))
      (setf (clog:connection-data-item body "conn") conn))
    
    (init-site body)
    (clog-web:create-web-page
     body
     :login `(:menu      (()) ;; ,*menu*
              :on-submit ,(lambda (obj)
                            (if (clog-web-dbi:login body (tbl:db conn)
                                                    (name-value obj "username")
                                                    (name-value obj "password"))
                                ;; url-replace removes login from history stack
                                (url-replace (location body) "/home")
                                (clog-web:clog-web-alert obj "Invalid" "The username and password are invalid."
                                                         :time-out 3
                                                         :place-top t))))
     ;; don't authorize use of page if logged in
     :authorize nil))
  )

;; /about
(defun on-about (body)
  (unless (clog:connection-data-item body "conn")
    (clog:url-replace (clog:location body) "/login"))
  (init-site body)
  (clog-web:create-web-page body :main `(:menu    ,*menu*

                                                  :content "About Me")))

(defun on-main (body)
  (unless (clog:connection-data-item body "conn")
    (clog:url-replace (clog:location body) "/login"))
  (init-site body)
  (clog-web:create-web-page body :main `(:menu    ,*menu*

                                                  :content "main")))
(defun main ()
  (clog:initialize
   'on-main
   :static-root (asdf:system-relative-pathname "open-orders"
                                               "./static-files/"))
  
  ;; clog web helper to set up routes in menu
  (clog-web:clog-web-routes-from-menu *menu*)
  (clog:set-on-new-window #'on-login :path "/login")
  (clog:open-browser))



(defun test ()
  (clog:initialize
   (lambda (body)
     (setf (clog:connection-data-item body "conn") 1)
     (clog:url-push-state (clog:window body) "/test")
     ;; (clog:url-replace (clog:location body) "/test")
     (clog:create-p body :content "It worked!")
     )
   :static-root (asdf:system-relative-pathname "open-orders"
                                               "./static-files/"))
  
  ;; clog web helper to set up routes in menu
  (clog:set-on-new-window (lambda (body)
                            (clog:create-p body :content (clog:connection-data-item body "conn")))
                          :path "/test")
  (clog:open-browser))
