;;;; feed.lisp

(in-package #:westbrook)

(defvar *generator-string* "commonlisp/westbrook/xach")

(defclass feed ()
  ((title
    :initarg :title
    :accessor title)
   (link
    :initarg :link
    :accessor link)
   (description
    :initarg :description
    :accessor description)
   (pub-date
    :initarg :pub-date
    :accessor pub-date)
   (last-build-date
    :initarg :last-build-date
    :accessor last-build-date)
   (category
    :initarg :category
    :initform nil
    :accessor category)
   (image
    :initarg :image
    :accessor image
    :initform nil)
   (items
    :initarg :items
    :initform ()
    :accessor items)
   (item-class
    :initarg :item-class
    :initform 'item
    :accessor item-class)))

(defclass item ()
  ((title
    :initarg :title
    :accessor title
    :documentation "Required. A string title.")
   (link
    :initarg :link
    :accessor link
    :documentation "Required. A string link to view this item.")
   (description
    :initarg :description
    :accessor description
    :initform "")
   (pub-date
    :initarg :pub-date
    :accessor pub-date
    :initform (get-universal-time)
    :documentation "A universal-time representing the publication date
    of the item. Defaults to the current time.")
   (guid
    :initarg :guid
    :accessor guid
    :documentation "Required. A string representing a unique (global)
    id of this item. If guid-permalink-p is true for the item, the
    guid should be a unique link to the item.")
   (guid-permalink-p
    :initarg :guid-permalink-p
    :accessor guid-permalink-p
    :initform nil)))

(defgeneric make-feed-item (feed &rest initargs &key &allow-other-keys)
  (:method (feed &rest initargs &key &allow-other-keys)
    (apply #'make-instance (item-class feed) initargs)))

(defgeneric add-feed-item (feed &rest initargs &key &allow-other-keys)
  (:method (feed &rest initargs &key &allow-other-keys)
    (push (apply #'make-feed-item feed initargs) (items feed))
    feed))

