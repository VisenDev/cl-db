

;; On a lot of my devices the asdf/uiop version doesn't support
;; uiop:define-package with local-nicknames, in such a case, we
;; need to load our vendored asdf version

(defun local-nicknames-supported-p ()
  (not (not (ignore-errors (eval `(uiop:define-package ,(gensym) (:local-nicknames)))))))

#+sbcl (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
#+sbcl (declaim (sb-ext:muffle-conditions cl:warning))

(unless (local-nicknames-supported-p)
  (format t "LOCAL NICKNAMES NOT SUPPORTED BY UIOP, LOADING VENDORED ASDF~%~%")
  (let* ((asdf-path (merge-pathnames "vendored/asdf.lisp" (uiop:getcwd)))
         (asdf-fasl-path (merge-pathnames 
                          (format nil "vendored/asdf-~a-~a.fasl"
                                  (lisp-implementation-type)
                                  (lisp-implementation-version))
                          (uiop:getcwd))))
    (let ((*standard-output* (make-broadcast-stream))
          (*error-output* (make-broadcast-stream)))
      (unless (probe-file asdf-fasl-path)
        (compile-file asdf-path :output-file asdf-fasl-path))
      (load asdf-fasl-path))))


(let ((*standard-output* (make-broadcast-stream)))
  (asdf:initialize-source-registry
   `(:source-registry
     (:tree ,(uiop:getcwd))
     :ignore-inherited-configuration))
  (asdf:load-system "clog"))

  #+sbcl(declaim (sb-ext:unmuffle-conditions sb-ext:compiler-note))
  #+sbcl(declaim (sb-ext:unmuffle-conditions cl:warning))

(asdf:load-system "open-orders")
