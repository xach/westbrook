;;;; package.lisp

(defpackage #:westbrook
  (:use #:cl
        #:cxml)
  (:export #:generate-to
           #:generate
           #:feed
           #:item
           #:items
           #:make-feed-item
           #:add-feed-item))

