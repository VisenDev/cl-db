(defpackage #:pagegen
  (:use #:cl))
(in-package #:pagegen)


(defconstant +doctype-html+ (defvar *doctype-html* "<!DOCTYPE html>"))

(defun tag (name &optional attributes-plist contents)
  (concatenate
   'string
   "<" (format nil "~a" name)
   (loop :for (name value) :on attributes-plist :by #'cddr
         :collect (format nil " ~a=\"~a\"" name value)
           :into strs
         :finally (return (string-downcase
                           (apply #'concatenate 'string strs))))
   ">"
   (when contents (format nil "~a" contents))
   (format nil "</~a>" name)))

(defun tag-form-p (form)
  (and (listp form)
       (not (= 0 (length form)))
       (keywordp (first form))))

(defmacro with-tags (&body body)
  `(concatenate
    'string
    ,@(loop :for form :in body
            :if (tag-form-p form)
              :collect `(tag
                         ,(string-downcase (symbol-name (first form)))
                         ,(when (second form)
                            (cons 'quote (second form))
                            nil)
                         (with-tags ,(third form)))
                :into forms
            :else
              :collect form :into forms
            :finally (return forms))))

(with-tags
  (:html
   ()
   (:head ()
          (:title () "Testy Test"))
   (:body ()
          (:h1 (:class "header" :id "primary header")
               "hello")
          (:h2 () "hello there")
          (:p () "bye"))))



