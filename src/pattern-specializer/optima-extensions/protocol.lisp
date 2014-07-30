;;;; protocol.lisp --- Protocol provided by the optima-extensions module.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer.optima-extensions)

;;; Subpattern protocol
;;;
;;; Optima should provide this.

(defgeneric pattern-subpatterns (pattern)
  (:documentation
   "If PATTERN is a composite pattern return its subpatterns as a
    list. Otherwise return the empty list."))

;; Default behavior

(defmethod pattern-subpatterns ((pattern optima::pattern))
  '())

(defmethod pattern-subpatterns ((pattern complex-pattern))
  (complex-pattern-subpatterns pattern))
