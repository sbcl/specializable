;;;; type-interoperation.lisp --- Interoperation with types.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer.optima-extensions)

;;; Utilities

(defun compare-types (type1 type2)
  (multiple-value-bind (result1 definitive1p)
      (subtypep type1 type2)
    (multiple-value-bind (result2 definitive2p)
        (subtypep type2 type1)
      (multiple-value-bind (result-and definitive-and-p)
          (subtypep `(and ,type1 ,type2) nil)
        (cond
          ((and result-and definitive-and-p)
           '//)
          ((and result1 definitive1p result2 definitive2p)
           '=)
          ((and result1 definitive1p)
           '<)
          ((and result2 definitive2p)
           '>)
          (t
           '/=))))))

;; The following two functions are use by
;; `guard-pattern-maybe-{type,predicate}'. Optima's syntax for
;; structure patterns requires specification of the "nonc" name
;; (e.g. `guard-pattern-') which is read in the current package (and
;; does not exist in the optima package). Consequently, the derived
;; predicate name (e.g. `guard-pattern-p') also has to be a symbol in
;; the current package.
;; TODO can we avoid this?
(defun guard-pattern-p (pattern)
  (typep pattern 'guard-pattern))

(defun variable-pattern-p (pattern)
  (typep pattern 'variable-pattern))

(defun guard-pattern-maybe-type (pattern)
  (match pattern
    ((structure guard-pattern-
                (test-form  (list 'typep (and variable (type symbol))
                                  (list 'quote type)))
                (subpattern (structure variable-pattern-
                                       (name (guard name (equal name variable))))))
     type)))

(defun guard-pattern-maybe-predicate (pattern)
  (match pattern
    ((structure guard-pattern-
                (test-form  (list (and predicate (type symbol))
                                  (and variable (type symbol))))
                (subpattern (structure variable-pattern-
                                       (name (guard name (equal name variable))))))
     predicate)))

(defun guard-pattern-maybe-type-specifier (pattern)
  (flet ((predicate->type (predicate)
           (case predicate
             (consp   'cons)
             (arrayp  'array)
             (vectorp 'vector)
             (stringp 'string))))
    (or (when-let ((type (guard-pattern-maybe-type pattern)))
          (match type
            ((list 'satisfies predicate)
             (predicate->type predicate))
            (otherwise
             type)))
        (when-let ((predicate (guard-pattern-maybe-predicate pattern)))
          (predicate->type predicate)))))

;;; Implementation of the `pattern-type-specifier' protocol

(defmethod pattern-type-specifier ((pattern constant-pattern))
  `(eql ,(constant-pattern-value pattern)))

(defmethod pattern-type-specifier ((pattern variable-pattern))
  t)

(defmethod pattern-type-specifier ((pattern class-pattern))
  (class-pattern-class-name pattern))

(defmethod pattern-type-specifier ((pattern structure-pattern))
  ;; TODO this is only approximately correct
  (let ((predicate (symbolicate (structure-pattern-conc-name pattern) '#:-p)))
    `(satisfies ,predicate)))

(flet ((recurse (type-specifier pattern)
         `(,type-specifier ,@(mapcar #'pattern-type-specifier
                                     (pattern-subpatterns pattern)))))

  (defmethod pattern-type-specifier ((pattern cons-pattern))
    (recurse 'cons pattern))

  (defmethod pattern-type-specifier ((pattern guard-pattern))
    (cond
      ((guard-pattern-maybe-type-specifier pattern))
      ((when-let ((predicate (guard-pattern-maybe-predicate pattern)))
         `(satisfies ,predicate)))
      (t t)))

  (defmethod pattern-type-specifier ((pattern not-pattern))
    (recurse 'not pattern))

  (defmethod pattern-type-specifier ((pattern and-pattern))
    (recurse 'and pattern))

  (defmethod pattern-type-specifier ((pattern or-pattern))
    (recurse 'or pattern)))
