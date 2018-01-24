;;;; feed.lisp

(in-package #:westbrook)

(defvar *generator-string* "commonlisp/westbrook/xach")

(defun check-required-slots (object slots)
  (dolist (slot slots)
    (unless (slot-boundp object slot)
      (error "Slot ~S must be initialized in ~S objects"
             slot
             (class-name (class-of object))))))

(defclass checked-slots-mixin ()
  ((checked-slots
    :reader checked-slots
    :initarg :checked-slots)))

(defmethod shared-initialize :after
    ((object checked-slots-mixin) (slot-names t)
     &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (check-required-slots object (checked-slots object)))

(defclass feed (checked-slots-mixin)
  ((title
    :initarg :title
    :accessor title
    :documentation "Required. A short description/title for the
    feed.")
   (link
    :initarg :link
    :accessor link
    :documentation "Required. A link to the RSS feed itself.")
   (description
    :initarg :description
    :accessor description
    :documentation "Required. A long-ish description of the feed.")
   (pub-date
    :initarg :pub-date
    :accessor pub-date
    :initform (get-universal-time))
   (last-build-date
    :initarg :last-build-date
    :accessor last-build-date
    :initform (get-universal-time))
   (items
    :initarg :items
    :initform '()
    :accessor items)
   (item-class
    :initarg :item-class
    :initform 'item
    :accessor item-class
    :documentation "When using ADD-FEED-ITEM or MAKE-FEED-ITEM, new
    items are created as instances of this class."))
  (:default-initargs
   :checked-slots '(title link)))

(defmethod print-object ((feed feed) stream)
  (print-unreadable-object (feed stream :type t :identity t)
    (format stream "~S with ~:D item~:P"
            (if (slot-boundp feed 'title)
                (title feed)
                "[missing title]")
            (length (items feed)))))

(defclass item (checked-slots-mixin)
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
    :initform nil))
  (:default-initargs
   :checked-slots '(link title guid)))

(defmethod print-object ((item item) stream)
  (print-unreadable-object (item stream :type t :identity t)
    (format stream "~S"
            (if (slot-boundp item 'title)
                (title item)
                "[missing title]"))))

(defgeneric make-feed-item (feed &rest initargs &key &allow-other-keys)
  (:method (feed &rest initargs &key &allow-other-keys)
    (apply #'make-instance (item-class feed) initargs)))

(defgeneric add-feed-item (feed &rest initargs &key &allow-other-keys)
  (:method (feed &rest initargs &key &allow-other-keys)
    (push (apply #'make-feed-item feed initargs) (items feed))
    feed))

