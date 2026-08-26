(defpackage #:open-orders.pagen
  (:use #:cl)
  (:export #:deftag
           #:tag
           #:+doctype-html+
           #:deftags
           ;; Tags
           #:h1 #:h2 #:h3 #:h4 #:h5 #:p #:a
           #:abbreviation #:acronym #:address #:anchor
           #:applet #:area #:article #:aside
           #:audio #:base #:basefont #:bdi
           #:bdo #:bgsound #:big #:blockquote
           #:body #:bold #|break|# #:button
           #:caption #:canvas #:center #:cite
           #:code #:colgroup #:col #:comment
           #:data #:datalist #:dd #:define
           #|delete|#
           #:details #:dialog #:dir
           #:div #:dl #:dt #:embed
           #:fieldset #:figcaption #:figure #:font
           #:footer #:form #:frame #:frameset
           #:head #:header #:heading #:hgroup
           #:hr #:html #:iframe #:image
           #:input #:ins #:isindex #:italic
           #:kbd #:keygen #:label #:legend
           #:link #|list|# #|main|# #:mark
           #:marquee #:menuitem #:meta #:meter
           #:nav #:nobreak #:noembed #:noscript
           #:object #:optgroup #:option #:output
           #:paragraph #:param #:em #:pre
           #:progress #:q #:rp #:rt
           #:ruby #:s #:samp #:script
           #:section #:small #:source #:spacer
           #:span #:strike #:strong #:style
           #:sub #:sup #:summary #:svg
           #:table #:tbody #:td #:template
           #:tfoot #:th #:thead #|time|#
           #:title #:tr #:track #:tt
           #:underline #:var #:video #:wbr
           #:xmp
           #:doctype
           #:br))
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
      (nreverse result)))
  (defun format-attributes-plist (attributes-plist)
    (loop :for (name value) :on attributes-plist :by #'cddr
          :collect (if (and (or (stringp name) (keywordp name))
                            (or (stringp value) (keywordp value)))

                       ;; create the attributes string at compile time if possible
                       (string-downcase
                        (format nil " ~a=\"~a\"" name value))

                       ;; otherwise just create the code to do so at runtime
                       `(string-downcase
                         (format nil " ~a=\"~a\"" ,name ,value))))))

(defmacro doctype (attributes-plist &body contents &environment env)
  "Special doctype tag"
  (declare (ignore attributes-plist))
  `(concatenate 'string "<!DOCTYPE html>"
                ,@(deduplicate-concatenate
                   (mapcar (lambda (form) (macroexpand form env)) contents))))



(defmacro self-closing-tag (name attributes-plist)
  (compress-adjacent-strings
   `(concatenate
     'string
     ;; Tag Open
     ,(format nil "<~a" name)
     ,@(format-attributes-plist attributes-plist)
     ">")))

(defmacro tag (name attributes-plist &rest contents &environment env)
  (compress-adjacent-strings
   `(concatenate
     'string

     ;; Tag Open
     ,(format nil "<~a" name)
     ,@(format-attributes-plist attributes-plist)
     ">"

     ;; Tag body, with nest (concatenate 'string) forms collapsed
     ,@(deduplicate-concatenate
        (mapcar (lambda (form)

                  ;; macroexpand body to so that we can optimize
                  (let ((expanded (macroexpand form env)))

                    ;; if the form is a string, we can just return it as is
                    (cond ((or (stringp expanded) (concatenate-string-p expanded))
                           expanded)

                          ;; Otherwise the form needs to be formatted at runtime
                          (t (let ((result (gensym)))
                               `(let ((,result ,expanded))
                                  (if (listp ,result)
                                      (format nil "~{~a~}" ,result)
                                      (format nil "~a" ,result))))))))
                contents))

     ;; Tag Close
     ,(format nil "</~a>" name))))

(defmacro deftag (name &key self-closing-p)
  (if self-closing-p
      `(defmacro ,name (attributes-plist)
         `(self-closing-tag ,,(string-downcase (symbol-name name))
                            ,attributes-plist))
      `(defmacro ,name (attributes-plist &body body)
         `(tag ,,(string-downcase (symbol-name name))
               ,attributes-plist ,@body))))

(defmacro deftags (&body forms)
  (loop :for form :in forms
        :for name = (if (listp form) (first form) form)
        :for self-closing-p = (if (listp form) (third form) nil)
        :collect `(deftag ,name :self-closing-p ,self-closing-p) :into forms
        :finally (return `(progn ,@forms))))

(deftags
  h1 h2 h3 h4 h5 p a
  abbreviation acronym address anchor
  applet (area :self-closing-p t)
  article aside
  audio (base :self-closing-p t)
  basefont bdi
  bdo bgsound big blockquote
  body bold #|break|# (br :self-closing-p t)
  button caption canvas center cite
  code colgroup (col :self-closing-p t)
  comment data datalist dd define
  #|delete|# details dialog dir
  div dl dt (embed :self-closing-p t)
  fieldset figcaption figure font
  footer form frame frameset
  head header heading hgroup
  (hr :self-closing-p t) html iframe
  (image :self-closing-p t)
  (img :self-closing-p t)
  (input :self-closing-p t)
  ins isindex italic
  kbd keygen label legend
  (link :self-closing-p t)
  #|list|# #|main|# mark
  marquee menuitem (meta :self-closing-p t) meter
  nav nobreak noembed noscript
  object optgroup option output
  paragraph (param :self-closing-p t) em pre
  progress q rp rt
  ruby s samp script
  section small (source :self-closing-p t) spacer
  span strike strong style
  sub sup summary svg
  table tbody td template
  tfoot th thead #|time|#
  title tr (track :self-closing-p t) tt
  underline var video (wbr :self-closing-p t)
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









