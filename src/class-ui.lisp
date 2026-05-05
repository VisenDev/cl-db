(open-orders.utils:defpackage* #:open-orders.class-ui
  (:use #:cl)
  (:import-from #:open-orders.utils
                #:fn
                #:*let)
  (:import-from #:introspect-environment
                #:typexpand)
  (:import-from #:defclass-std
                #:defclass/std
                #:class/std)
  (:local-nicknames (#:a #:alexandria)
                    (#:mop #:closer-mop))
  (:export #:config/checkbox
           #:slot-ui
           #:config/toggle
           #:style
           #:config/filepicker
           #:config/text
           #:placeholder
           #:config/integer
           #:min-value
           #:max-value
           #:config/number
           #:config/slider
           #:config/radio
           #:options
           #:class-ui))
(in-package #:open-orders.class-ui)

(declaim (optimize (debug 3) (safety 3)))

(defclass/std config ()
  ((label)
   (value)
   (div-class label-class input-class :std "")))

(defgeneric slot-ui (config container on-update-function))

(defclass/std config/toggle (config)
  ((style
    :std :checkbox
    :type (member :switch :checkbox))))

(defmethod slot-ui ((config config/toggle) container on-update-function)
  (let* ((div (clog:create-div container :class (div-class config)))
         (label (clog:create-label div :content (label config) :class (label-class config)))
         (name (symbol-name (gensym "open-orders-toggle")))
         (input (clog:create-form-element
                 div :checkbox :role (if (eq (style config) :switch) "switch" "")
                 :name name
                 :label label
                 :value (if (value config) "on" "off")
                 :class (label-class config))))
    (clog:set-on-change
     input
     (fn (obj)
       (funcall on-update-function (clog:checkedp input))))))

(defclass/std config/filepicker (config) ())

(defmethod slot-ui ((config config/filepicker) container on-update-function)
  (let* ((div (clog:create-div container :class (div-class config)))
         (label (clog:create-label div :content (label config) :class (label-class config)))
         (input (clog:create-form-element
                 div :file
                 :label label
                 :class (input-class config))))
    (clog:set-on-change
     input (fn (obj)
             (a:when-let (path (ignore-errors (pathname (clog:value input))))
               (funcall on-update-function path))))))

(defclass/std config/text (config)
  ((placeholder :std "" :type string)))

(defmethod slot-ui ((config config/text) container on-update-function)
  (let* ((div (clog:create-div container :class (div-class config)))
         (label (clog:create-label div :content (label config) :class (label-class config)))
         (input (clog:create-form-element
                 div :text
                 :label label
                 :value (if (value config) (value config) "")
                 :placeholder (placeholder config)
                 :class (input-class config))))

    (clog:set-on-change
     input (fn (obj)
             (funcall on-update-function
                      (format nil "~a"
                              (clog:value input)))))))

(defclass config/integer (config)
  ((min :accessor min-value :initarg :min :initform nil)
   (max :accessor max-value :initarg :max :initform nil)))

(defmethod slot-ui ((config config/integer) container on-update-function)
  (let* ((div (clog:create-div container :class (div-class config)))
         (label (clog:create-label div :content (label config)
                                       :class (label-class config)))
         (args (list
                div :number
                :value (if (value config) (value config) 0)
                :label label
                :class (input-class config))))
    (when (min-value config) (a:appendf args (list :min (min-value config))))
    (when (max-value config) (a:appendf args (list :max (max-value config))))
    (let ((input (apply #'clog:create-form-element args)))
      (clog:set-on-change
       input (fn (obj)
               (a:when-let (val (ignore-errors (parse-integer (clog:value input))))
                 (funcall on-update-function val)))))))

(defclass config/number (config)
  ((min :accessor min-value :initarg :min :initform nil)
   (max :accessor max-value :initarg :max :initform nil)))

(defmethod slot-ui ((config config/number) container on-update-function)
  (let* ((div (clog:create-div container :class (div-class config)))
         (label (clog:create-label div :content (label config)
                                       :class (label-class config)))
         (args (list
                div :number
                :value (if (value config) (value config) 0)
                :label label
                :class (input-class config))))
    (when (min-value config) (a:appendf args (list :min (min-value config))))
    (when (max-value config) (a:appendf args (list :max (max-value config))))
    (let ((input (apply #'clog:create-form-element args)))
      (clog:set-on-change
       input (fn (obj)
               (a:when-let (val (ignore-errors
                                 (parse-float:parse-float (clog:value input) :type 'real)))
                 (funcall on-update-function val)))))))

(defclass config/slider (config)
  ((min :accessor min-value :initarg :min
        :initform (error "This slot is mandatory"))
   (max :accessor max-value :initarg :max
        :initform (error "This slot is mandatory"))))

(defmethod slot-ui ((config config/slider) container on-update-function)
  (let* ((div (clog:create-div container :class (div-class config)))
         (label (clog:create-label div :content (label config)
                                       :class (label-class config)))
         (input (clog:create-form-element div :range
                                          :label label
                                          :value (or (value config) 0)
                                          :min (min-value config)
                                          :max (max-value config)
                                          :class (input-class config))))
    
    (clog:set-on-change
     input (fn (obj)
             (a:when-let (val (ignore-errors
                               (parse-integer (clog:value input))))
               (setf (clog:inner-html label)
                     (format nil "~a: ~a" (label config) val))
               (funcall on-update-function val))))))

(defclass/std config/radio (config)
  ((options :type list)))

(defmethod slot-ui ((config config/radio) container on-update-function)
  (let* ((div (clog:create-fieldset container :class (div-class config)))
         (legend (clog:create-legend div :content (label config)
                                         :class (label-class config)))
         (name (symbol-name (gensym "open-orders-radio")))
         (inputs
           (loop :for option :in (reverse (options config))
                 :for radio-label = (clog:create-label div :content option)
                 :collect
                 (clog:create-form-element
                  radio-label :radio :name name :auto-place :top))))
    (declare (ignore legend))
    (dolist (input inputs)
      (clog:set-on-change
       input (fn (obj)
               (funcall on-update-function
                        (clog:radio-value div name)))))))

(defclass/std config/color (config) ())

(defmethod slot-ui ((config config/color) container on-update-function)
  (let* ((div (clog:create-div container :class (div-class config)))
         (label (clog:create-label div :content (label config) :class (label-class config)))
         (input (clog:create-form-element div :color :label label
                                                     :class (input-class config))))

    (clog:set-on-change
     input (fn (obj)
             (funcall on-update-function
                      (clog:value input))))))

(defmethod slot-ui ((config (eql :ignore)) container on-update-function)
  "Configs of value :ignore should be ignored")

(defclass/std config/list (config)
  ((item-config :std (make-instance 'config/text :label ""))
   (item-count :std 0)
   (adjustable :std t)))

(defun remove-nth (n list)
  (nconc (subseq list 0 n) (nthcdr (1+ n) list)))

(defun pad-list (list new-length &optional default-value)
  (if (> new-length (length list))
    (loop :repeat (- new-length (length list))
          :collect default-value :into tail
          :finally (return (append list tail)))
    list))


(defun duplicate-instance (instance)
  (mop:ensure-finalized (class-of instance))
  (let* ((class (class-of instance))
         (copy (allocate-instance class)))
    (loop :for slot :in (mapcar #'mop:slot-definition-name
                                (mop:class-slots class))
          :when  (slot-boundp instance slot)
            :do (setf (slot-value copy slot)
                      (slot-value instance slot))
          :finally (return copy))))



(defmethod slot-ui ((config config/list) container on-update-function)
  (warn "Using list ui, which is broken right now")
  (*let ((div (clog:create-div container :class (div-class config)))
         ;; (_label (clog:create-label div :class (label-class config)
         ;;                               :content (label config)))
         (details (clog:create-details div))
         (_summary (clog:create-summary details :content (label config)
                                                :class (label-class config)))
         (blockquote (clog:create-element details "blockquote"))
         (list (clog:create-div blockquote))
         ;; (n (if (not (plusp (item-count config)))
         ;;        1
         ;;        (item-count config)))
         (values (pad-list (value config) (item-count config))))
    (labels
        ((list-ui ()
           (clog:destroy-children list)
           (loop
             :for val :in values
             :for i :from 0
             :do
                (let ((c (duplicate-instance (item-config config)))
                      (i i))
                  (setf (label c) "")
                  (setf (value c) val)
                  (slot-ui c list
                           (lambda (new-value)
                             (setf (nth i values) new-value)
                             (funcall on-update-function values))))
             :finally
                (when (adjustable config)
                  (let* ((nav (clog:create-element list "nav"))
                         (ctrl-list (clog:create-unordered-list nav)))
                    (clog:set-on-click
                     (clog:create-button (clog:create-list-item ctrl-list)
                                         :content "Add")
                     (fn (obj)
                       (a:appendf values (list nil))
                       (funcall on-update-function values)
                       (list-ui)))

                    (when (plusp (length values))
                      (clog:set-on-click
                       (clog:create-button (clog:create-list-item ctrl-list) :content "Remove")
                       (fn (obj)
                         (setf values (subseq values 0 (1- (length values))))
                         (funcall on-update-function values)
                         (list-ui)
                         ))))))
           ))
      (list-ui)
      ))
  
  ;; (let* (;; (div (clog:create-div container))
  ;;        ;; (label (clog:create-label div :content (label config)))
  ;;        (toplevel (clog:create-div container))
  ;;        (details (clog:create-details toplevel))
  ;;        (label (clog:create-summary details :content (label config)))
  ;;        (values (make-list (item-count config))))
  ;;   (declare (ignore label))

  ;;   (loop
  ;;     :for i :below (item-count config)
  ;;     :for div = (clog:create-list-item details)
  ;;     :collect div :into divs
  ;;     :do
  ;;        (let ((i i))
  ;;          (slot-ui (item-config config) div
  ;;                   (lambda (item-value)
  ;;                     (setf (nth i values) item-value)
  ;;                     (funcall on-update-function values))))
  ;;     :finally


  ;;        ;; TODO

  ;;        ;; This code is all super buggy
  ;;        ;; I need to rethink this whole list editing system
  ;;        (when (adjustable config)
  ;;          (labels ((create-destroy-button (i div)
  ;;                     (clog:set-on-click
  ;;                      (clog:create-button div :content "X")
  ;;                      (fn (obj)
  ;;                        (setf values (remove-nth i values))
  ;;                        (clog:destroy div)
  ;;                        (setf divs (remove-nth i divs))
  ;;                        (funcall on-update-function values)
  ;;                        (when (plusp i)
  ;;                          (create-destroy-button (1- (length values))
  ;;                                                 (nth (1- (length divs)) divs)))))))

  ;;            (when div
  ;;              (create-destroy-button (1- i) div))

  ;;            (clog:set-on-click
  ;;             (clog:create-button toplevel :content "New")
  ;;             (fn (obj)
  ;;               (let ((div (clog:create-list-item details))
  ;;                     (i (1- (length divs))))
  ;;                 (a:appendf divs (list div))
  ;;                 (slot-ui
  ;;                  (item-config config) div
  ;;                  (lambda (item-value)
  ;;                    (setf (nth i values) item-value)
  ;;                    (funcall on-update-function values)))
  ;;                 )
  ;;               )
  ;;             )
  ;;            ))

  ;;     ))
  )


(defun diff (a b)
  "returns the difference between two numbers"
  (abs (- a b)))

(defgeneric finalize-config (config instance slotd))
(defmethod finalize-config ((config (eql :ignore)) instance slotd))
(defmethod finalize-config ((config config) instance slotd)
  (let ((name (mop:slot-definition-name slotd))
        (type (typexpand (mop:slot-definition-type slotd))))
    (unless (label config)
      (setf (label config)
            (map 'string (fn (ch) (if (char= #\- ch) #\Space ch))
                 (symbol-name name))))
    (unless (value config)
      (when (slot-boundp instance name)
        (setf (value config) (slot-value instance name))))
    
    (when (eq (class-of config) (find-class 'config))
      (cond
        ;; checkbox / toggle
        ((subtypep type 'boolean)
         (change-class config 'config/toggle))

        ;; radio
        ((and 
          (listp type)
          (eq 'member (first type)))
         (change-class config 'config/radio)
         (setf (options config) (rest type)))

        ;; file-author
        ((subtypep type 'pathname)
         (change-class config 'config/filepicker))

        ;; slider
        ((and (subtypep type 'integer)
              (listp type)
              (= 3 (length type))
              (numberp (second type))
              (numberp (third type))
              (< 1024 (diff (second type) (third type))))
         (change-class config 'config/slider :min (second type)
                                             :max (third type)))

        ;; integer
        ((subtypep type 'integer)
         (change-class config 'config/integer)
         (when (listp type)
           (when (and ( = (length type) 2)
                      (numberp (second type)))
             (setf (min-value config) (second type)))
           
           (when (and ( = (length type) 3)
                      (numberp (second type))
                      (numberp (third type)))
             (setf (min-value config) (second type))
             (setf (max-value config) (second type)))))

        ;; number
        ((subtypep type 'real)
         (change-class config 'config/number))

        ;; object
        ;; TODO

        ;; list
        ;; TODO
        ((subtypep type 'list)
         (change-class config 'config/list)
         (setf (item-count config) 
               (if (slot-boundp instance name)
                   (length (slot-value instance name))
                   1))
         (setf (item-config config) (make-instance 'config/text))
         (setf (adjustable config) t))

        ;; hash table
        ;; TODO

        (t
         (change-class config 'config/text))))))

(defun getf* (plist symbol &optional default)
  "getf but it also checks using the keyword version of <symbol>"
  (getf plist symbol
        (getf plist (a:make-keyword symbol)
              default)))

(defparameter *form-class* "container")

(defmethod class-ui (slot-config-plist
                      (instance standard-object) (container clog:clog-obj))
  (let ((form (clog:create-form container :class *form-class*)))
    (dolist (slotd (reverse (mop:class-slots (class-of instance))))
      (let* ((name (mop:slot-definition-name slotd))
             (config (getf* slot-config-plist name (make-instance 'config))))
        (finalize-config config instance slotd)
        (slot-ui config form
                 (lambda (new-value)
                   (setf (slot-value instance name) new-value)))))))

(defclass/std person ()
  ((name hobbies siblings phone email address notes)
   (age :type integer)
   (in-prison :type boolean)
   (papers :type pathname)
   (favorite-color)))

(defvar *person* (make-instance 'person :name "John"))

(defun test ()

  (clog:initialize
   (lambda (body)

     ;; ;; Picocss
     (clog:load-css
      (clog:html-document body)
      "https://cdn.jsdelivr.net/npm/@picocss/pico@2.1.1/css/pico.min.css")


     ;; ;; Win 7
     ;; (clog:load-css (clog:html-document body)
     ;;                "https://unpkg.com/7.css")

     ;; ;; Apple Classic
     ;; (clog:load-css (clog:html-document body)
     ;;                "https://unpkg.com/@sakun/system.css")

     ;; NES
     ;; (clog:load-css (clog:html-document body)
     ;;                "https://unpkg.com/nes.css@latest/css/nes.min.css")

     ;; XP
     ;; (clog:load-css (clog:html-document body)
     ;;                "https://unpkg.com/xp.css")

     ;; 98
     ;; (clog:load-css (clog:html-document body)
     ;;                "https://unpkg.com/xp.css@0.2.3/dist/98.css")

     ;; PS1
     ; (clog:l
     ;; (clog:
     ;; (clog:load-css (clog:html-document body)
     ;;                "https://cdn.jsdelivr.net/gh/98mprice/PSone.css@master/PSone.min.css")

     (class-ui (list :email (make-instance 'config/text :placeholder "foo@foo.com")
                     :age (make-instance 'config/slider :min 0 :max 100)
                     :siblings (make-instance 'config/radio
                                              :options '(one two three four five+))
                     :favorite-color (make-instance 'config/color)
                     :hobbies (make-instance 'config/list :item-count 5))
               *person* body)
       

     ;; (let ((person (make-instance 'person :name "Bobby")))
     ;;   (create-form-from-object
     ;;    body person
     ;;    (list :age (make-config :type :range
     ;;                            :label "Person Age: ")
     ;;          :name (make-config :placeholder "(Name....)"))
     ;;    :form-class "container" )

     ;;   (create-form-from-object*
     ;;    body person (:age (:type :range :label "Person Age / v2 test: ")
     ;;                 :name (:placeholder "(Name HERE....)")
     ;;                 :phone (:validate-function
     ;;                         (lambda (val)
     ;;                           (every #'digit-char-p val)))))

     ;;   )
     ))
  (clog:open-browser)
  )

