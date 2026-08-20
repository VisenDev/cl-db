(defpackage #:open-orders.pagen
  (:use #:cl)
  (:export #:deftag
           #:tag
           #:+doctype-html+
           #:deftags

           ;; Tags
           h1 h2 h3 h4 h5 p
           abbreviation acronym address anchor
           applet area article aside
           audio base basefont bdi
           bdo bgsound big blockquote
           body bold #|break|# button
           caption canvas center cite
           code colgroup col comment
           data datalist dd define
           #|delete|#
           details dialog dir
           div dl dt embed
           fieldset figcaption figure font
           footer form frame frameset
           head header heading hgroup
           hr html iframe image
           input ins isindex italic
           kbd keygen label legend
           link #|list|# main mark
           marquee menuitem meta meter
           nav nobreak noembed noscript
           object optgroup option output
           paragraph param em pre
           progress q rp rt
           ruby s samp script
           section small source spacer
           span strike strong style
           sub sup summary svg
           table tbody td template
           tfoot th thead #|time|#
           title tr track tt
           underline var video wbr
           xmp))
(in-package #:open-orders.pagen)

(defconstant +doctype-html+ (defvar *doctype-html* "<!DOCTYPE html>"))

(defun tag (name attributes-plist &rest contents)
  (concatenate
   'string
   "<" (format nil "~a" name)
   (loop :for (name value) :on attributes-plist :by #'cddr
         :collect (format nil " ~a=\"~a\"" name value)
           :into strs
         :finally (return (string-downcase
                           (apply #'concatenate 'string strs))))
   ">"
   (format nil "~{~a~}" (remove nil contents))
   (format nil "</~a>" name)))

(defmacro deftag (name)
  `(defmacro ,name (attributes-plist &body body)
     `(tag ,,(string-downcase (symbol-name name))
           (list ,@attributes-plist) ,@body)))

(defmacro deftags (&body names)
  (loop :for name :in names
        :collect `(deftag ,name) :into forms
        :finally (return `(progn ,@forms))))

(deftags
  h1 h2 h3 h4 h5 p
  abbreviation acronym address anchor
  applet area article aside
  audio base basefont bdi
  bdo bgsound big blockquote
  body bold #|break|# button
  caption canvas center cite
  code colgroup col comment
  data datalist dd define
  #|delete|# details dialog dir
  div dl dt embed
  fieldset figcaption figure font
  footer form frame frameset
  head header heading hgroup
  hr html iframe image
  input ins isindex italic
  kbd keygen label legend
  link #|list|# main mark
  marquee menuitem meta meter
  nav nobreak noembed noscript
  object optgroup option output
  paragraph param em pre
  progress q rp rt
  ruby s samp script
  section small source spacer
  span strike strong style
  sub sup summary svg
  table tbody td template
  tfoot th thead #|time|#
  title tr track tt
  underline var video wbr
  xmp)

#+nil
(defparameter *test*
  (html ()
    (head ()
      (title () "Testy Test"))
    (body ()
      (h1 (:class "header" :id "primary header"))
      (h2 () "hello there")
      (p () "hi"))))
