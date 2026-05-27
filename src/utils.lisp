(uiop:define-package #:open-orders.utils
  (:use #:cl)
  (:export #:*let
           #:fn
           #:diff
           #:defstruct*
           #:strcat))
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


(defmacro defstruct* (name-and-options &body slots)
  "Like defstruct except it also defines short generic accessor functions
   for every slot using the slot name."
  
  (let* ((name (if (listp name-and-options)
                   (car name-and-options)
                   name-and-options))
         (options (if (listp name-and-options)
                      (cdr name-and-options)
                      nil))
         (conc-name (or (cadr (find :conc-name options :key #'car))
                        (alexandria:symbolicate name '-)))
         (slot-names (mapcar (lambda (slot)
                               (if (listp slot)
                                   (car slot)
                                   slot))
                             slots))
         (include (cadr (find :include options :key #'car))))

    ;; Handle include option
    #+closer-mop
    (when include
      (closer-mop:ensure-finalized (find-class include))
      (alexandria:appendf
       slot-names
       (mapcar #'closer-mop:slot-definition-name
               (closer-mop:class-slots (find-class include)))))
    #-closer-mop
    (when include
      (error "Cannot handle '(:include ~a)' without closer-mop" include))

    ;; Define expansion
    `(progn

       ;; Struct Definition
       (defstruct ,name-and-options ,@slots)

       ;; Generic Accessors for Slots
       ,@(mapcar (lambda (slot-name)
                   `(progn

                      ;; Getter
                      (defmethod ,slot-name ((,name ,name))
                        (,(alexandria:symbolicate conc-name slot-name) ,name))

                      ;; Setter
                      (defmethod (setf,slot-name) (new-value (,name ,name))
                        (setf (,(alexandria:symbolicate conc-name slot-name) ,name)
                              new-value))))
                 slot-names))))


(declaim (ftype (function (&rest (or null string)) string) strcat))
(defun strcat (&rest strings)
  (apply #'concatenate 'string strings))
