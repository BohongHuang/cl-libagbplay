#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(defsystem cl-agbplay
  :version "1.0.0"
  :author "Bohong Huang <1281299809@qq.com>"
  :maintainer "Bohong Huang <1281299809@qq.com>"
  :license "lgpl3"
  :description "Play GBA game music in Common Lisp"
  :homepage "https://github.com/BohongHuang/cl-agbplay"
  :bug-tracker "https://github.com/BohongHuang/cl-agbplay/issues"
  :source-control (:git "https://github.com/BohongHuang/cl-agbplay.git")
  :components ((:file "cl-agbplay"))
  :depends-on (#:cffi-libffi))

(defsystem cl-agbplay/example
  :components ((:file "example"))
  :build-operation program-op
  :build-pathname "agbplay"
  :entry-point "agbplay-example:run-example"
  :depends-on (#:asdf
               #:cl-agbplay
               #:cl-raylib))
