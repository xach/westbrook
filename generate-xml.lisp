;;;; generate-xml.lisp

(in-package #:westbrook)

(defun element (name value)
  (labels ((content (value)
             (if (consp value)
                 (apply #'elements value)
                 (text value))))
    (if (consp name)
        (with-element* ((first name) (second name))
          (content value))
        (with-element name (content value)))))

(defun elements (&rest names-and-values)
  (loop for (name value) on names-and-values by #'cddr
        do (element name value)))

(defgeneric generate (object))

(defmethod generate :around ((feed feed))
  (with-xml-output (make-string-sink) (call-next-method)))

(defmethod generate (feed)
  (with-element "rss"
    (attribute "version" "2.0")
    (with-element "channel"
      (elements "title" (title feed)
                "link" (link feed)
                "description" (description feed)
                "pubDate" (pubdate-string (pub-date feed))
                "lastBuildDate" (pubdate-string (last-build-date feed))
                "generator" *generator-string*)
      (let ((categories (category feed)))
        (when (atom categories)
          (setf categories (list categories))))
      (when (image feed)
        (with-element "image"
          (elements "url" (image feed)
                    "title" (title feed)
                    "link" (link feed))))
      (let ((items (copy-list (items feed))))
        (setf items (sort items #'> :key 'pub-date))
        (dolist (item  (items feed))
          (generate item))))))

(defmethod generate ((item item))
  (with-element "item"
    (elements "title" (title item)
              "link" (link item)
              "description" (description item)
              "pubDate" (pubdate-string (pub-date item)))
    (with-element "guid"
      (attribute "isPermaLink" (if (guid-permalink-p item)
                                   "true"
                                   "false"))
      (text (guid item)))))

(defun generate-to (file object)
  (with-open-file (stream file :direction :output
                          :if-exists :rename-and-delete)
    (write-string (generate object) stream))
  (probe-file file))
