(defpackage #:open-orders.tables
  (:use #:cl #:open-orders.sql-table)
  (:import-from #:defclass-std
                #:defclass/std)
  (:export #:user
           #:person
           #:customer
           #:part
           #:supplier
           #:material
           #:open-order
           #:name
           #:hash
           #:first-name
           #:last-name
           #:email
           #:phone
           #:notes
           #:id
           #:purchase-order
           #:line-item
           #:ship-terms
           #:billing-terms
           #:ship-notes
           #:primary-contact
           #:part-number
           #:description
           #:revision
           #:suppliers
           #:supplies
           #:categories
           #:with-database
           #:database-disconnect
           #:database-connect
           #:authentication-token
           #:authentication-token-timestamp
           #:contactable-mixin
           #:contact-first-name
           #:contact-last-name
           #:contact-email
           #:contact-phone
           #:connection
           #:db
           #:user-create-new
           #:scheduled-shipment
           #:make-scheduled-shipment
           #:scheduled-shipment-p
           #:copy-scheduled-shipment
           #:scheduled-shipment-date
           #:scheduled-shipment-quota
           #:scheduled-shipment-amount
           #:scheduled-shipment-completed-p
           #:open-order-deadline))
(in-package #:open-orders.tables)

(defclass autodefined-table () ())
(defclass open-orders-table ()
  ((id :accessor id
       :type integer
       :primary-key t
       :autoincrement t
       :initform nil
       :initarg :id)
   (notes :accessor notes
          :type list
          :initform nil
          :initarg :notes))
  (:metaclass sql-table))

(defclass/std user (autodefined-table)
  ((name :type string :primary-key t)
   (hash authentication-token :type string)
   (authentication-token-timestamp :type integer))
  (:metaclass sql-table))





;; (defclass/std person (open-orders-table)
;;   ((first-name last-name email phone :type string))
;;   (:metaclass sql-table))

(defclass/std contactable-mixin (autodefined-table)
  ((contact-first-name contact-last-name contact-email contact-phone
                       :type string :std ""))
  (:metaclass sql-table))

(defclass/std customer (open-orders-table contactable-mixin autodefined-table)
  ((name :type string))
  (:metaclass sql-table))

(defstruct scheduled-shipment
  (date (get-universal-time) :type integer)
  (quota 0 :type integer)
  (amount 0 :type integer))

(declaim (ftype (function (scheduled-shipment) boolean)
                scheduled-shipment-completed-p))
(defun scheduled-shipment-completed-p (shipment)
  (>= (scheduled-shipment-amount shipment)
      (scheduled-shipment-quota shipment)))

(defclass/std part (open-orders-table autodefined-table)
  ((part-number :type string)
   (description :type string)
   (revision :type string)
   (inventory-count :type integer)
   (inventory-location :type integer))
  (:metaclass sql-table))

(defclass/std suppliers (open-orders-table contactable-mixin autodefined-table)
  ((name :type string)
   (supplies :type list))
  (:metaclass sql-table))

(defclass/std material (open-orders-table autodefined-table)
  ((name :type string)
   (categories :type list)
   (suppliers :type list))
  (:metaclass sql-table))

(defclass/std open-order (open-orders-table autodefined-table)
  ((customer :type integer :references (customer id))
   (purchase-order :type string)
   (line-item :type integer :std 0)
   (part :type integer :references (part id))
   (ship-terms :type string :std "PrePay and Add")
   (billing-terms :type string :std "Net 30")
   (ship-notes :type string)
   (material :type integer :references (material id))
   (run-status :type string)
   (scheduled-shipments :type list :doc "list of scheduled-shipment"))
  (:metaclass sql-table))

(declaim (ftype (function (open-order) integer) open-order-deadline))
(defun open-order-deadline (open-order)
  (let ((filtered (remove-if #'scheduled-shipment-completed-p
                             (scheduled-shipments open-order))))
    (loop :for shipment :in filtered
          :minimize (scheduled-shipment-date shipment))))

(defclass/std connection ()
  ((user db)))


;;; Utils
(defun database-connect ()
  (let ((db (dbi:connect :sqlite3 :database-name "open-orders.sqlite3")))

    (dolist (class
             (closer-mop:class-direct-subclasses (find-class 'autodefined-table)))
      (exec db (create (class-name class) t)))
    db))

(defun database-disconnect (db)
  (dbi:disconnect db))

(defun %nuke-tables (db)
  (dolist (class
           (closer-mop:class-direct-subclasses (find-class 'autodefined-table)))
    (exec db (drop (class-name class) t))))

(defun user-create-new (name password)
  (let ((db (database-connect)))
    (exec
     db (insert
         (make-instance 'user
                        :name name
                        :hash (cl-pass:hash password))))
    (database-disconnect db)))
