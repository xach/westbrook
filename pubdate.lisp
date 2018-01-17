;;;; pubdate.lisp

(in-package #:westbrook)

;;; Convert universal-times to pubdate format.

(defvar *short-month-names*
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
    "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defvar *short-weekday-names*
  #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defun pubdate-string (universal-time)
  (multiple-value-bind (second minute hour date month year day dst zone)
      (decode-universal-time universal-time)
    (when dst
      (decf zone))
    (let ((month-name (aref *short-month-names* (1- month)))
          (weekday-name (aref *short-weekday-names* day)))
      (format nil "~A, ~2,'0D ~A ~4,'0D ~
                   ~2,'0D:~2,'0D:~2,'0D ~[-~;+~;+~]~2,'0D00"
              weekday-name
              date
              month-name
              year
              hour
              minute
              second
              (1+ (- (signum zone)))
              (abs zone)))))

(defun iso8601-date-string (universal-time)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal-time)
    (declare (ignore second minute hour))
    (format nil "~4,'0D-~2,'0D-~2,'0D"
            year month date)))
