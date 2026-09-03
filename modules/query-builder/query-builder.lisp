(defpackage #:open-orders.query-builder
  (:use #:cl #:open-orders.sql-table #:open-orders.pagen))
(in-package #:open-orders.query-builder)


;;;; Desired interface
(defstruct target
  table
  fields)

(defstruct ref table field)

(defstruct where
  lhs test rhs)

(defstruct join
  lhs rhs)

(defstruct query
  targets
  wheres
  joins)



(defparameter *query*
  (make-query
   :targets (list (make-target :table 'open-order
                               :fields (list 'due-date 'line-item 'part))
                  (make-target :table 'part
                               :fields (list 'id 'name)))
   :wheres (list (make-where :lhs (make-ref :table 'open-order
                                            :field 'tag)
                             :test #'string-equal
                             :rhs :vintage-air))
   :joins (list (make-join :lhs (make-ref :table 'open-order
                                          :field 'part)
                           :rhs (make-ref :table 'part
                                          :field 'id)))))



(defun retrieve (query)
  (let* ((target-tables (mapcar #'target-table (query-targets query)))
         (target-data (mapcar #'select-all target-tables))
         (filtered-data (filter-data target-data (query-where query))))
    (join-data filtered-data (query-join query))))
