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

;;; Pattern keys protocol

(defgeneric pattern-keys (pattern)
  (:documentation
   "Return a list of objects uniquely encoding the structural
    positions of variables in PATTERN.

    For example, the keys of a `cons-pattern' are '(0 1), the keys of
    a `class-pattern' are the (ordered) slot names."))

;; Default behavior

(defmethod pattern-keys ((pattern optima::pattern))
  '())

(defmethod pattern-keys ((pattern complex-pattern))
  (iota (length (pattern-subpatterns pattern))))

(defmethod pattern-keys ((pattern class-pattern))
  (values (class-pattern-slot-names pattern)
          (class-pattern-class-name pattern)))

;;; Pattern type specifier protocol

(defgeneric pattern-type-specifier (pattern)
  (:documentation
   "Return a type specifier that bounds the set of values matching
    PATTERN from above.

    Note that the returned type may be a very coarse approximation in
    many cases, for example:

    * (class clazz (a 1)) => clazz
    * (guard x (< x 5))   => t"))
