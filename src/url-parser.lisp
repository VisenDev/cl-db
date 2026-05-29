(uiop:define-package #:open-orders.url-parser
  (:local-nicknames (#:a #:alexandria))
  (:import-from #:open-orders.utils
                #:mapstring)
  (:use #:cl)
  (:export
   #:parse-parameters
   #:decode
   #:encode))
(in-package #:open-orders.url-parser)

(declaim (ftype (function (character) boolean) valid-url-char-p))
(defun valid-url-char-p (ch)
  (or (alphanumericp ch)
      (not (not (member ch '(#\- #\. #\_ #\~))))))

(declaim (ftype (function (string) string) string-encode))
(defun encode (string)
  (mapstring (lambda (ch)
               (if (valid-url-char-p ch)
                   ch
                   (format nil "%~2,'0x" (char-code ch))))
             string))

(declaim (ftype (function (string) string) string-decode))
(defun decode (string)
  (let ((chars
          (loop :for i :from 0 :below (length string)
                :for ch :across string
                :if (char= ch #\%)
                  :collect (code-char
                            (let ((*read-base* 16)
                                  (*read-eval* nil))
                              (incf i 2)
                              (read-from-string
                               (subseq string (- i 1) (+ i 1)))))
                :else
                  :collect ch
                :end)))
    (make-array (length chars) :element-type 'character
                               :initial-contents chars)))



(declaim (ftype (function (string) list) parse-parameters))
(defun parse-parameters (url)
  (a:when-let (params-begin (position #\? url))
    (let* ((params-string
             (subseq url (1+ params-begin)))
           (params-list (uiop:split-string params-string :separator "&")))
      (loop :for key-value :in params-list
            :for (key value) = (uiop:split-string key-value :separator "=")
            :appending (list (intern (string-upcase key) 'keyword) value)))))
