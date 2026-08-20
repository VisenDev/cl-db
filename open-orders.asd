(in-package #:asdf-user)

(defsystem "open-orders" 
  :author "Robert Burnett"
  :depends-on ("open-orders.main")
  :build-operation program-op
  :build-pathname "open-orders"
  :entry-point "open-orders.main:main")

