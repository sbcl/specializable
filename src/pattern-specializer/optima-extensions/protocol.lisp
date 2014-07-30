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

;;; Pattern type specifier protocol

(defgeneric pattern-type-specifier (pattern)
  (:documentation
   "Return a type specifier that bounds the set of values matching
    PATTERN from above.

    Note that the returned type may be a very coarse approximation in
    many cases, for example:

    * (class clazz (a 1)) => clazz
    * (guard x (< x 5))   => t"))
