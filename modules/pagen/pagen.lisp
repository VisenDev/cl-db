(defpackage #:open-orders.pagen
  (:use #:cl)
  (:export #:deftag
           #:tag
           #:+doctype-html+
           #:deftags

           ;; Tags
           h1 h2 h3 h4 h5 p a
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
           link #|list|# #|main|# mark
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
           xmp
           #:doctype))
(in-package #:open-orders.pagen)

(eval-when (:compile-toplevel :load-toplevel)
  (defun concatenate-string-p (form)
    (and (listp form)
         (eq 'concatenate (first form))
         (equalp (quote (quote string)) (second form))))

  (defun deduplicate-concatenate (forms)
    (loop :for form :in forms
          :appending
          (if (concatenate-string-p form)
              (cddr form)
              (list form))))
  
  (defun compress-adjacent-strings (forms)
    (let ((result nil))
      (dolist (form forms)
        (if (and (stringp (first result)) (stringp form))
            (setf (first result)
                  (concatenate 'string (first result) form))
            (push form result)))
      (nreverse result))))

(defmacro doctype (attributes-plist &body contents)
  (declare (ignore attributes-plist))
  `(concatenate 'string "<!DOCTYPE html>"
                ,@contents))

(defmacro tag (name attributes-plist &rest contents &environment env)
  (compress-adjacent-strings
   `(concatenate
     'string
     ,(concatenate
       'string
       "<" (format nil "~a" name)
       (loop :for (name value) :on attributes-plist :by #'cddr
             :collect (format nil " ~a=\"~a\"" name value)
               :into strs
             :finally (return (string-downcase
                               (apply #'concatenate 'string strs))))
       ">")
     ,@(deduplicate-concatenate (mapcar (lambda (form) (macroexpand form env))
                                        contents))
     ,(format nil "</~a>" name))))

(defmacro deftag (name)
  `(defmacro ,name (attributes-plist &body body)
     `(tag ,,(string-downcase (symbol-name name))
           ,attributes-plist ,@body)))

(defmacro deftags (&body names)
  (loop :for name :in names
        :collect `(deftag ,name) :into forms
        :finally (return `(progn ,@forms))))

(deftags
  h1 h2 h3 h4 h5 p a
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
  link #|list|# #|main|# mark
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


;; test
#+nil
(html ()
  (head ()
    (title () "Testy Test"))
  (body ()
    (h1 (:class "header" :id "primary header"))
    (h2 () (if (boundp 'foo)
               (h3 () (a () "hello there")) "no foo"))
    (p () "hi")))









