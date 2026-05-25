(when (uiop:version< uiop:*uiop-version* "3.3")
  (let ((asdf-path (merge-pathnames "vendored/asdf.lisp" (uiop:getcwd)))
        (asdf-fasl-path (merge-pathnames "vendored/asdf.fasl" (uiop:getcwd))))
    (unless (probe-file asdf-fasl-path)
      (compile-file asdf-path :output-file asdf-fasl-path))
    (load asdf-fasl-path)))


#+sbcl (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
#+sbcl (declaim (sb-ext:muffle-conditions cl:warning))
(let ((*standard-output* (make-broadcast-stream)))
  (asdf:initialize-source-registry
   `(:source-registry
     (:tree ,(uiop:getcwd))
     :ignore-inherited-configuration))
  (asdf:load-system "clog"))

  #+sbcl(declaim (sb-ext:unmuffle-conditions sb-ext:compiler-note))
  #+sbcl(declaim (sb-ext:unmuffle-conditions cl:warning))

(asdf:load-system "open-orders")
