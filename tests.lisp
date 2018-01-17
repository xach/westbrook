;;;; tests.lisp

(fiasco:define-test-package #:westbrook-tests
  (:use #:cl
        #:westbrook))

(in-package #:westbrook-tests)

(deftest test-item-creation ()
  (let ((feed (make-instance 'feed)))
    (let ((item
           (make-feed-item feed
                           :title "Hello, world"
                           :description "What's up?"
                           :link "https://www.xach.com/#1"
                           :guid "https://www.xach.com/#1"
                           :guid-permalink-p t)))
      (push item (items feed))
      (is (eql (length (items feed)) 1))
      (add-feed-item feed
                     :title "Second item"
                     :description "Not much"
                     :link "https://www.xach.com/#2"
                     :guid "westbrook:test-feed:item:2"
                     :guid-permalink-p nil)
      (is (eql (length (items feed))  2)))))

