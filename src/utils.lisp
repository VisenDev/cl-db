(uiop:define-package #:open-orders.utils
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export #:*let
           #:fn
           #:diff))
(in-package #:open-orders.utils)

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defun ignored-binding-p (binding)
    (and (listp binding)
         (char= #\_ (char (symbol-name (first binding)) 0))))
  (defun binding->name (binding)
    (if (listp binding)
        (first binding)
        binding)))

(defmacro *let (bindings &body body)
  "let* except it allows underscore prefixed vars to be ignored automatically"
  `(let* ,bindings
     (declare (ignorable ,@(remove-duplicates
                            (mapcar #'binding->name
                                    (remove-if-not #'ignored-binding-p bindings)))))
     ,@body))

(defmacro fn (args &body body)
  "Shorter lambda that automatically makes args ignorable"
  `(lambda ,args
     (declare (ignorable ,@args))
     ,@body))

(defun diff (a b)
  "returns the difference between two numbers"
  (abs (- a b)))
