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

;;; Normalization protocol


(defgeneric pattern-normalize (form pattern)
  (:documentation
   "Normalize PATTERN into normal form FORM and return the normalized
    pattern.

    Examples of FORM are

    :literal
      TODO

    :cnf
      TODO

    :cnf/strict"))

(defgeneric pattern-normalize-1 (form pattern)
  (:documentation
   "Perform one step towards normalizing PATTERN into normal form FORM
    and return the resulting pattern.

    Return nil if no such step can be made. This function is intended
    for use in iterative normalization schemes."))

;; Default implementation

(defmethod pattern-normalize ((form    t)
                              (pattern t))
  ;; The default behavior consists in normalizing iterative and
  ;; traversing PATTERN.
  (pattern-normalize/iterate+traverse
   (curry #'pattern-normalize-1 form) pattern))

(defmethod pattern-normalize ((form    list)
                              (pattern t))
  ;; Subsequent normalize to the forms in the list FORM.
  (reduce (lambda (result form)
            (pattern-normalize form result))
          form :initial-value pattern))

(defmethod pattern-normalize-1 ((form    t)
                                (pattern t))
  nil)

(defun pattern-normalize/iterate (function pattern)
  "Iteratively normalize PATTERN by repeatedly calling FUNCTION on
   intermediate normalization results.

   Return two values: 1. the normalized pattern derived from PATTERN
   2. a Boolean indicating whether the first return value is different
   from PATTERN.

   When called with a pattern, FUNCTION has to return either a
   modified pattern based on the argument or nil to indicate that the
   argument should not be transformed further."
  (let ((function (coerce function 'function)))
    (do ((any-change-p nil)
         (changep      t)
         (pattern      pattern))
        ((not changep) (values pattern any-change-p))
      (setf changep nil
            pattern (multiple-value-bind (result change1p)
                        (funcall function pattern)
                      (when change1p
                        (setf any-change-p t
                              changep      t))
                      result)))))

(defun pattern-normalize/traverse (function pattern)
  "Call FUNCTION on PATTERN and its subpatterns.

   Return two values: 1. a pattern tree consisting of the return
   values of individual calls of FUNCTION 2. a Boolean indicating
   whether the first return value is different from PATTERN."
  (let ((function (coerce function 'function))
        (changep nil))
    (values
     (map-pattern/reconstitute
      (lambda (pattern recurse reconstitute)
        (declare (ignore recurse reconstitute))
        (when-let ((result (funcall function pattern)))
          (setf changep t)
          result))
      pattern)
     changep)))

(defun pattern-normalize/iterate+traverse (function pattern)
  "Use `pattern-normalize/iterate' and `pattern-normalize/traverse' to
   repeatedly apply FUNCTION recursively to PATTERN.

   Return values like `pattern-normalize/iterate'."
  (pattern-normalize/iterate
   (lambda (pattern)
     (pattern-normalize/traverse function pattern))
   pattern))
