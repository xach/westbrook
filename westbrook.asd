;;;; westbrook.asd

(asdf:defsystem #:westbrook
  :description "An RSS feed generator."
  :author "Zach Beane <xach@xach.com>"
  :license "BSD"
  :serial t
  :depends-on (#:cxml)
  :in-order-to ((test-op (test-op "westbrook-tests")))
  :components ((:file "package")
               (:file "pubdate")
               (:file "feed")
               (:file "generate-xml")))
