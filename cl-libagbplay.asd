#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(defsystem cl-libagbplay
  :version "1.0.0"
  :author "Bohong Huang <1281299809@qq.com>"
  :maintainer "Bohong Huang <1281299809@qq.com>"
  :license "lgpl3"
  :description "Play GBA game music in Common Lisp"
  :homepage "https://github.com/BohongHuang/cl-libagbplay"
  :bug-tracker "https://github.com/BohongHuang/cl-libagbplay/issues"
  :source-control (:git "https://github.com/BohongHuang/cl-libagbplay.git")
  :components ((:file "cl-libagbplay"))
  :depends-on (#:cffi-libffi))

(defsystem cl-libagbplay/example
  :components ((:file "example"))
  :build-operation program-op
  :build-pathname "libagbplay"
  :entry-point "libagbplay-example:run-example"
  :depends-on (#:asdf
               #:cl-libagbplay
               #:cl-raylib))
