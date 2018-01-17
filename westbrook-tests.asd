;;;; westbrook-tests.asd

(asdf:defsystem #:westbrook-tests
  :depends-on (#:westbrook
               #:fiasco)
  :description "Tests for westbrook"
  :author "Zach Beane <xach@xach.com>"
  :license "MIT"
  :serial t
  :perform (test-op (o c)
                    (funcall (read-from-string "fiasco:run-package-tests")
                             :package :westbrook-tests))
  :components ((:file "tests")))
