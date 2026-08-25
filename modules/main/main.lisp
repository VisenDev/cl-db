(defpackage #:open-orders.main
  (:use #:cl
        #:open-orders.pagen
        #:open-orders.sql-table
        #:open-orders.tables
        #:open-orders.templates
        #:open-orders.auth)
  (:import-from #:url-rewrite
                #:url-encode)
  (:export
   #:main))
(in-package #:open-orders.main)

(defvar *acceptor* nil)

(defparameter *toplevel-links*
  '("logout" "open-orders" "customers" "inventory"))

(defun insert-toplevel-links ()
  (form ()
   (table ()
     (tr ()
       (mapcar (lambda (arg)
                 (td () 
                   (button (:type "submit"
                            :formaction  (format nil "/~a" arg))
                     arg)))
               *toplevel-links*)))))

(hunchentoot:define-easy-handler (index :uri "/") ()
  (perform-auth-check)
  (hunchentoot:redirect "/open-orders"))

(defun get-open-orders-sorted-by (sort-by reverse)
  (let* ((raw (select-all 'open-order))
         (sorted 
           (if (null sort-by)
               raw
               ;; else
               (sort
                raw 
                (alexandria:switch (sort-by :test #'string=)
                  ("part-number"
                   (lambda (lhs rhs)
                     (string-lessp
                      (ignore-errors
                       (part-number (select 'part 'id (part lhs))))
                      (ignore-errors
                       (part-number (select 'part 'id (part rhs)))))))
                  ("purchase-order"
                   (lambda (lhs rhs)
                     (string-lessp (purchase-order lhs)
                                   (purchase-order rhs))))
                  ("due-date"
                   (lambda (lhs rhs)
                     (< (open-order-deadline lhs)
                        (open-order-deadline rhs))))
                  ("line-item"
                   (lambda (lhs rhs)
                     (< (line-item lhs)
                        (line-item rhs)))))))))
    (if reverse (reverse sorted) sorted)))

(hunchentoot:define-easy-handler (open-orders :uri "/open-orders") (sort-by reversed)

  (let ((reversed-p (string= reversed "true")))
    (flet ((table-header-href (sort-by)
             (format nil "/open-orders?sort-by=~a&reversed=~a"
                     sort-by
                     (if reversed-p "false" "true")))
           (edit-order-href (open-order-id)
             (format nil "/edit-order?id=~a" open-order-id)))
      (with-internal-page
        (insert-toplevel-links)
        (hr ())
        (table ()
          (tr ()
            (th () (a (:href (table-header-href "part-number")) "Part Number"))
            (th () (a (:href (table-header-href "purchase-order")) "Po Number"))
            (th () (a (:href (table-header-href "due-date")) "Due Date"))
            (th () (a (:href (table-header-href "line-item")) "Line Item"))
            (th () (a (:href "/new?type=order") (button () "New"))))
          (mapcar (lambda (order)
                    (let ((href (edit-order-href (id order))))
                      (tr ()
                        (td ()
                          (a (:href href)
                            (ignore-errors
                             (part-number
                              (select 'part 'id (part order))))))
                        (td () (a (:href href)
                                 (purchase-order order)))
                        (td () (a (:href href) "TODO"))
                        (td () (a (:href href) (line-item order))))))
                  (get-open-orders-sorted-by sort-by reversed-p)))))))

(hunchentoot:define-easy-handler (new :uri "/new") (type)
  (with-internal-page 
    (alexandria:switch (type :test #'string=)
      ("order"
       (let ((id (insert (make-instance
                          'open-order
                          :purchase-order (auth-token-create)))))
         (hunchentoot:redirect (format nil "/edit-order?id=~a" id))))
      (otherwise
       (h1 () "Error: don't know how to make a new '~a'" type)))))

(defmacro when-assoc ((varname key alist) &body body)
  (alexandria:with-gensyms (keyval value)
    `(let* ((,keyval ,key)
            (,value (assoc ,key ,alist :test #'string-equal))
            (,varname (cdr ,value)))
       (declare (ignorable ,value ,keyval ,varname))
       (when ,varname
         ,@body))))

(hunchentoot:define-easy-handler
    (save-order-po-details :uri "/save-order") (id redirect)

  (perform-auth-check)
  
  (let ((params (hunchentoot:post-parameters*))
        (order (select 'open-order 'id (parse-integer id))))
    (format t "Params: ~a~%~%" params)
    (when order
      ;; Customer Code
      (when-assoc (customer-code 'customer-code params)
        (format t "customer-code: ~a~%~%" customer-code)
        (let ((customer (select 'customer 'name customer-code)))
          (if customer
              
              ;;customer found, update id
              (setf (customer order) (id customer))

              ;; else create new customer
              (let* ((new-customer (make-instance 'customer
                                                  :name customer-code))
                     (new-customer-id (insert new-customer)))
                (format t "New customer id: ~a~%~%" new-customer-id)
                (setf (customer order) new-customer-id)))))

      ;; finally save order and redirect
      (update order))

    ;; Redirect back to app
    (hunchentoot:redirect redirect :code 303)))

(hunchentoot:define-easy-handler (edit-order :uri "/edit-order") (id tab)

  (unless tab
      (hunchentoot:redirect (format nil "/edit-order?id=~a&tab=po-details" id)))

  (labels ((format-save-url (redirect)
             (format nil "/save-order?id=~a&redirect=~a" id (url-encode redirect)))
           (save-button (contents destination)
             (td ()
               (button (:type "submit"
                        :formaction (format-save-url destination))
                 contents)))
           (format-tab-link (tab)
             (format nil "/edit-order?id=~a&tab=~a" id tab)))
    (let ((order (select 'open-order 'id (parse-integer id))))
      (with-internal-page
        (form (:method "POST" :action (format-save-url
                                       (format-tab-link "po-details")))
          (table ()
            ;; toplevel tab bar
            (tr ()
              (mapcar (lambda (arg)
                        (save-button arg (format nil "/~a" arg)))
                      *toplevel-links*)))

          (hr ())

          ;; order tab bar
          (table ()
            (tr ()
              (mapcar (lambda (tab)
                        (save-button tab (format-tab-link tab)))
                      '("po-details" "job-ticket"
                        "shipping-details" "certificate-of-conformance"))))
          
          (table ()
            ;; (tr ()
            ;;   (td () (input (:type "submit"))))
            (tr ()
              (td ()
                (table ()
                  (tr ()
                    (td () "Customer Code")
                    (td ()
                      (input (:type "text" :name "customer-code"
                              :id "customer-code" :list "customer-code-list"
                              :value (ignore-errors
                                      (or
                                       (name (select 'customer 'id
                                                     (customer order)))
                                       "")))))
                    (datalist (:id "customer-code-list")
                      (mapcar #'name (select-all 'customer))))
                  (tr ()
                    (td () "Customer Name")
                    (td () (a (:href "/edit-customer?id=TODO"
                               :style "padding:none;")
                             (button () "View Customer Information")))))
                ))
            )
          )
        ))))

(hunchentoot:define-easy-handler (customers :uri "/customers") ()
  (with-internal-page
    (p () "Customers Page")
    (h3 () "Primary content goes here :)")))

(hunchentoot:define-easy-handler (inventory :uri "/inventory") ()
  (with-internal-page
    (p () "Inventory Page")
    (h3 () "Primary content goes here :)")))

(defun start ()
  (database-connect)
  (setf *acceptor* (make-instance 'hunchentoot:easy-acceptor :port 8000))
  (hunchentoot:start *acceptor*))

(defun stop ()
  (database-disconnect)
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil)))


(defun main ()
  (start)
  (unwind-protect (loop (sleep 1))
    (stop)))
