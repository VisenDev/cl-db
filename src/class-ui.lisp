(open-orders.utils:defpackage* #:open-orders.class-ui
  (:use #:cl)
  (:import-from #:open-orders.utils
                #:fn
                #:*let
                #:diff)
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

(defun upcase-first-letter (string)
  (if (plusp (length string))
      (let ((downcase (map 'string #'char-downcase string)))
        (setf (char downcase 0) (char-upcase (char downcase 0)))
        downcase)
      string))

(defun prettify-lisp-identifier (symbol)
  "Convert a symbol like foo-bar-bap into the string 'Foo Bar Bap'"
  (let* ((str (map 'string (fn (ch) (if (char= #\- ch) #\Space ch))
                   (symbol-name symbol)))
         (words (uiop:split-string str)))
    (loop :for word :in words
          :appending (list (upcase-first-letter word) " ") :into results
          :finally (return
                     (string-right-trim
                      " " (apply #'concatenate 'string results))))))

(defclass/std config ()
  ((label)
   (value)
   (div-class :std "grid container")
   (label-class input-class :std "")
   (div-role :std "")))

(defclass/std slot-ui ()
  ((div label input extract-value-function)))

(defgeneric slot-ui (config container on-update-function))
(defmethod slot-ui ((config config) container on-update-function)
  (let ((result (make-instance
                 'slot-ui :div (clog:create-div container
                                                :class (div-class config)))))
    (setf (label result)  (clog:create-label (div result)
                                             :content (label config)
                                             :class (label-class config)))
    (setf (clog:attribute (div result) "role") (div-role config))

    (clog:set-on-change
     (div result)
     (fn (obj)
       (when (extract-value-function result)
         (a:when-let
             (val
              (ignore-errors
               (funcall (extract-value-function result))))
           (funcall on-update-function val)))))

    result))

(defclass/std config/toggle (config)
  ((style
    :std :checkbox
    :type (member :switch :checkbox))))

(defmethod slot-ui ((config config/toggle) container on-update-function)
  (let* ((result (call-next-method)))
    (setf (input result)
          (clog:create-form-element
           (div result) :checkbox
           :role (if (eq (style config) :switch) "switch" "")
           :label (label result)
           :value (if (value config) "on" "off")
           :class (input-class config)))
    (setf (extract-value-function result)
          (fn () (clog:checkedp (input result))))
    result))

(defclass/std config/text (config)
  ((placeholder :std "" :type string)))

(defmethod slot-ui ((config config/text) container on-update-function)
  (let* ((result (call-next-method)))
    (setf (input result)
          (clog:create-form-element
                 (div result) :text
                 :label (label result)
                 :value (if (value config) (value config) "")
                 :placeholder (placeholder config)
                 :class (input-class config)))
    (setf (extract-value-function result)
          (fn () (format nil "~a" (clog:value (input result)))))
    result))

(defclass config/integer (config)
  ((min :accessor min-value :initarg :min :initform nil)
   (max :accessor max-value :initarg :max :initform nil)))

(defmethod slot-ui ((config config/integer) container on-update-function)
  (let* ((result (call-next-method))
         (args (list
                (div result) :number
                :value (if (value config) (value config) 0)
                :label (label result)
                :class (input-class config))))
    (when (min-value config) (a:appendf args (list :min (min-value config))))
    (when (max-value config) (a:appendf args (list :max (max-value config))))
    (setf (input result) (apply #'clog:create-form-element args))
    (setf (extract-value-function result)
          (fn ()
            (ignore-errors
             (parse-integer (clog:value (input result))))))))

(defclass config/number (config)
  ((min :accessor min-value :initarg :min :initform nil)
   (max :accessor max-value :initarg :max :initform nil)))

(defmethod slot-ui ((config config/number) container on-update-function)
    (let* ((result (call-next-method))
         (args (list
                (div result) :number
                :value (if (value config) (value config) 0)
                :label (label result)
                :class (input-class config))))
    (when (min-value config) (a:appendf args (list :min (min-value config))))
    (when (max-value config) (a:appendf args (list :max (max-value config))))
    (setf (input result) (apply #'clog:create-form-element args))
    (setf (extract-value-function result)
          (fn ()
            (ignore-errors
             (parse-float:parse-float (clog:value (input result))
                                      :type 'real))))
      result))

(defclass config/slider (config)
  ((min :accessor min-value :initarg :min
        :initform (error "This slot is mandatory"))
   (max :accessor max-value :initarg :max
        :initform (error "This slot is mandatory"))))

(defmethod slot-ui ((config config/slider) container on-update-function)
  (let* ((result (call-next-method)))
    (setf (input result)
          (clog:create-form-element
                 (div result) :range
                 :label (label result)
                 :value (or (value config) 0)
                 :min (min-value config)
                 :max (max-value config)
                 :class (input-class config)))
    (setf (extract-value-function result)
          (fn ()
            (a:when-let (val (ignore-errors
                              (parse-integer (clog:value (input result)))))
              (setf (clog:inner-html (label result))
                    (format nil "~a: ~a" (label config) val))
              val)))))

(defclass/std config/radio (config)
  ((options :type list)))

(defmethod slot-ui ((config config/radio) container on-update-function)
  (let* ((result (call-next-method))
         (name (symbol-name (gensym "radio"))))
    (setf (input result)
          (clog:create-div (div result) :class (input-class result)))
    (clog:label-for (label result) (input result))
    (loop :for option :in (reverse (options config))
          :for radio-label = (clog:create-label (input result) :content option)
          :do
             (clog:create-form-element
              (input result) :radio
              :name name :auto-place :top
              :label radio-label))
    (setf (extract-value-function result)
          (fn ()
            (clog:radio-value (input result) name)))
    result))

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
  (let ((result (call-next-method))
        ;; (value-list (pad-list (value-config) (item-count config)))
        ;; (ui-list nil)
        (uis '())
        (values '())
        )
    (setf (input result)
          (clog:create-div (div result) :class (input-class config)))
    (labels ((push-ui (ui-config i)
               (push (slot-ui ui-config (input result)
                              (lambda (new-value)
                                (setf (nth i values) new-value)))
                     uis))
             (pop-ui () (pop uis)))

      (dotimes (i (max (length (value config))
                       (item-count config)))
        (push-ui (duplicate-instance (item-config config)) i))
      )
    )
  )

;; (defmethod slot-ui ((config config/list) container on-update-function)

;; ;;;; TODO rewrite this function to use the new method for writing slot uis
;;   (*let ((div (clog:create-div container :class (div-class config)
;;                                          :style "min-height:78px;" ;; so that the detail
;;                                ;; has the same height
;;                                ;; as textboxes
;;                                ))
;;          (details (clog:create-details div))
;;          (_summary (clog:create-summary details :content (label config)
;;                                                 :class (label-class config)))
;;          (blockquote (clog:create-element details "blockquote"))
;;          (list (clog:create-div blockquote))
;;          (values (pad-list (value config) (item-count config))))
;;     (setf (clog:attribute div "role") (div-role config))
;;     (when values
;;       (setf (clog:details-openp details) t))
;;     (labels
;;         ((list-ui ()
;;            (clog:destroy-children list)
;;            (loop
;;              :for val :in values
;;              :for i :from 0
;;              :do
;;                 (let ((c (duplicate-instance (item-config config)))
;;                       (i i))
;;                   (setf (label c) "")
;;                   (setf (value c) val)
;;                   (slot-ui c list
;;                            (lambda (new-value)
;;                              (setf (nth i values) new-value)
;;                              (funcall on-update-function values))))
;;              :finally
;;                 (when (adjustable config)
;;                   (let* ((nav (clog:create-element list "nav"))
;;                          (ctrl-list (clog:create-unordered-list nav)))
;;                     (clog:set-on-click
;;                      (clog:create-button (clog:create-list-item ctrl-list)
;;                                          :content "Add")
;;                      (fn (obj)
;;                        (a:appendf values (list nil))
;;                        (funcall on-update-function values)
;;                        (list-ui)))

;;                     (when (plusp (length values))
;;                       (clog:set-on-click
;;                        (clog:create-button
;;                         (clog:create-list-item ctrl-list) :content "Remove")
;;                        (fn (obj)
;;                          (setf values (subseq values 0 (1- (length values))))
;;                          (funcall on-update-function values)
;;                          (list-ui)
;;                          ))))))
;;            ))
;;       (list-ui))))

(defgeneric finalize-config (config instance slotd))
(defmethod finalize-config ((config (eql :ignore)) instance slotd))
(defmethod finalize-config ((config config) instance slotd)
  (let ((name (mop:slot-definition-name slotd))
        (type (typexpand (mop:slot-definition-type slotd))))
    (unless (label config)
      (setf (label config)
            (prettify-lisp-identifier name)))
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
        ;; ((subtypep type 'pathname)
        ;;  (change-class config 'config/filepicker))

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
               (or (ignore-errors (length (slot-value instance name)))
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

(defclass/std class-ui ()
  ((slot-ui-list)
   (slot-name-list)
   (instance)))

(defmethod finalize-values ((class-ui class-ui))
  "Loop through all the slot-ui elements and save their values to the instance,
   this ensures the instance has the most up to date values entered into the slot-uis.
   This function should be called before things like serializing/saving the values of 
   the instance"
  (loop :for slot :in (slot-ui-list class-ui)
        :for name :in (slot-name-list class-ui)
        :do (setf (slot-value (instance class-ui) name)
                  (funcall (extract-value-function slot))))
  (instance class-ui))

(defmethod class-ui (slot-config-plist
                     (instance standard-object) (container clog:clog-obj))
  (let ((form (clog:create-form container))
        (result (make-instance 'class-ui)))
    (setf (instance result) instance)
    (dolist (slotd (reverse (mop:class-slots (class-of instance))))
      (let* ((name (mop:slot-definition-name slotd))
             (config (getf* slot-config-plist name (make-instance 'config))))
        (finalize-config config instance slotd)
        (push (slot-ui config form
                       (lambda (new-value)
                         (setf (slot-value instance name) new-value)))
              (slot-ui-list result))
        (push name (slot-name-list result))
        result))))



;;;; ============== TEST ===============
;;;; =                                 =
;;;; ===================================

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

