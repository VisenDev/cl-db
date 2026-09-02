(defpackage #:open-orders.query-builder
  (:use #:cl #:open-orders.sql-table #:open-orders.pagen))
(in-package #:open-orders.query-builder)


;;;; Desired interface
(make-select
 (open-order (make-fields :fields (list 'due-date 'name 'part) :from 'open-orders))
 (part (make-fields :fields (list 'name) :from 'parts :where (parts)))
 :where ()
 )

(defstruct target
  table
  fields)

(defstruct ref table field)

(defstruct where
  lhs op rhs)

(defstruct join
  lhs rhs)

(defstruct query
  targets
  where
  join)



(defparameter *query*
  (make-query
   :targets (list (make-target :table 'open-order
                               :fields (list 'due-date 'line-item 'part))
                  (make-target :table 'part
                               :fields (list 'id 'name)))
   :join (make-join :lhs (make-ref :table 'open-order
                                   :field 'part)
                    :rhs (make-ref :table 'part
                                   :field 'id))))

(defun retrieve (query)
  (let* ((target-tables (mapcar #'target-table (query-targets query)))
         (target-data (mapcar #'select-all target-tables))
         (filtered-data (filter-data target-data (query-where query))))
    (join-data filtered-data (query-join query))))
