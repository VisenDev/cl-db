(open-orders.utils:defpackage* #:open-orders.tab-bar
  (:use #:cl)
  (:import-from #:defclass-std
                #:defclass/std
                #:class/std)
  (:import-from #:open-orders.utils
                #:fn
                #:*let)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:tab-bar
   #:tab-names
   #:tab-buttons
   #:div
   #:selected-tab-name
   #:create-tab-bar))
(in-package #:open-orders.tab-bar)
(declaim (optimize (debug 3)))

(defclass/std tab-bar ()
  ((tab-names)
   (tab-buttons)
   (div)
   (selected-tab-name)
   (on-click-function)))

(declaim (ftype (function (clog:clog-obj list) tab-bar) create-tab-bar))
(defun create-tab-bar (clog-obj tab-names)
  (let ((result (make-instance 'tab-bar :tab-names tab-names
                               :div (clog:create-div clog-obj
                                                     :style "margin-bottom:20px;"))))
    (setf (clog:attribute (div result) "role") "group")
    ;; (clog:add-class (div result) "grid")
    (labels ((select-button (button name)
               (dolist (b (tab-buttons result))
                 (clog:remove-attribute b "disabled"))
               (ignore-errors
                (setf (clog:attribute button "disabled") ""))
               (setf (selected-tab-name result) name)
               (when (on-click-function result)
                 (funcall (on-click-function result) name))))
      (declare (ftype (function (clog:clog-button string) t) select-button))
      (loop :for name :in tab-names
            :for button = (clog:create-button (div result) :content name :class "secondary")
            :do (let ((name name)
                      (button button))
                  (clog:set-on-click
                   button (fn (obj) (select-button button name))))
                (push button (tab-buttons result)))
      (select-button (car (last (tab-buttons result)))
                     (car (last (tab-names result)))))
    result))

(defmethod clog:set-on-click ((clog-obj tab-bar)
                              on-click-handler &key one-time cancel-event)
  (declare (ignore one-time cancel-event))
  (setf (on-click-function clog-obj) on-click-handler))
