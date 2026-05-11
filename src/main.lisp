(defpackage #:open-orders.main
  (:use #:cl #:clog)
  (:import-from #:defclass-std
                #:defclass/std
                #:class/std)
  (:local-nicknames (#:a #:alexandria))
  (:export #:main))
(in-package #:open-orders.main)

(defun init-site (body)
  (clog-web:clog-web-initialize body)
  (setf (title (html-document body)) "Open Orders")
  (clog-web:create-web-site body
                   ;; use the default theme
                   :theme 'clog-web:default-theme
                   ;; theme settings - in this case w3.css color of menu bar
                   :settings '(:color-class "w3-black")
                   :title "Open Orders"
                   :footer "(c) 2026 Wess Burnett"
                   :logo "/img/clog-liz.png"))

;; This is the menu structure
(defparameter *menu* `(("Content" (("Home"                "/"       on-main)
                                   ("Content from Lambda" "/lambda" on-lambda)
                                   ("Content from File"   "/readme" on-readme)))
                       ("Help"    (("About"               "/about"  on-about)))
                       ("Logout" ())))

;; Page handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; / -  a simple content string
(defun on-main (body)
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
  (init-site body)
  (clog-web:create-web-page body :main `(:menu    ,*menu*
                                :content ,(lambda (obj)
                                            (create-div obj :content "I am in the content area")))))

;; /about
(defun on-about (body)
  (init-site body)
  (clog-web:create-web-page body :main `(:menu    ,*menu*

                                :content "About Me")))
(defun main ()
  (clog:initialize 'on-main)
  
  ;; clog web helper to set up routes in menu
  (clog-web:clog-web-routes-from-menu *menu*)
  (clog:open-browser))

