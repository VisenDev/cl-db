(defpackage #:open-orders.url-parser
  (:local-nicknames (#:a #:alexandria))
  (:use #:cl)
  (:export
   #:parse-parameters))
(in-package #:open-orders.url-parser)

(declaim (ftype (function (string) list) parse-parameters))
(defun parse-parameters (url)
  (a:when-let (params-begin (position #\? url))
    (let* ((params-string
             (subseq url (1+ params-begin)))
           (params-list (uiop:split-string params-string :separator "&")))
      (loop :for key-value :in params-list
            :for (key value) = (uiop:split-string key-value :separator "=")
            :appending (list (intern (string-upcase key) 'keyword) value)))))
