;;;; transform.lisp --- Framework for transformation of optima patterns.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer.optima-extensions)

;; TODO should be merged upstream
(defmethod unparse-pattern ((pattern constant-pattern))
  (let ((value (constant-pattern-value pattern)))
    (typecase value
      ((and symbol (not keyword) (not (member nil t)))
       `(quote ,value))
      (cons
       `(quote ,value))
      (t
       value))))

(defun reconstitute-pattern (pattern subpatterns)
  "Return a copy of PATTERN with subpatterns replaced by SUBPATTERN. "
  #+TODO-maybe-later (make-instance (class-of pattern)
                                    :subpatterns (funcall recurse))
  (typecase pattern
    (and-pattern
     (apply #'make-and-pattern subpatterns))
    (or-pattern
     (apply #'make-or-pattern subpatterns))
    (t
     (let ((copy (parse-pattern (unparse-pattern pattern))))
       (when subpatterns
         (setf (optima.core:complex-pattern-subpatterns copy) subpatterns))
       copy))))

;;; map-pattern family of functions

(defun map-pattern (result-type function pattern)
  "Call FUNCTION with each pattern in PATTERN return a tree of return
   values of FUNCTION.

   The returned tree is represented as nested sequences of type
   RESULT-TYPE.

   Example:

     (map-pattern 'list (lambda (pattern recurse)
                          (list* (type-of pattern)
                                 (funcall recurse)))
                  (optima.core:parse-pattern '(and x 1)))
     => (AND-PATTERN (VARIABLE-PATTERN) (CONSTANT-PATTERN))"
  (labels ((recurse* (function pattern)
             (funcall function pattern (curry #'recurse pattern)))
           (recurse (pattern &optional (function function))
             (map result-type (curry #'recurse* function)
                  (pattern-subpatterns pattern))))
    (recurse* function pattern)))

(defun mapc-pattern (function pattern)
  "Like `map-pattern', call FUNCTION with each pattern in PATTERN.

   This function is only called for side effects and does not return
   any values."
  (map-pattern nil (lambda (pattern recurse)
                     (funcall function pattern)
                     (funcall recurse))
               pattern)
  (values))

(defun map-pattern/reconstitute (function pattern)
  "Like `map-pattern', but construct new patterns from return values
   of FUNCTION.

   Example:

     (map-pattern/reconstitute
      (lambda (pattern recurse reconstitute)
        (if (typep pattern 'optima.core:variable-pattern)
            (optima.core:make-variable-pattern)
            (funcall reconstitute)))
      (optima.core:parse-pattern '(and 1 (cons x y))))
     => (AND 1 (CONS OPTIMA::_ OPTIMA::_))

   I.e. variable names in variable-pattern s have been anonymized
   without modifying the rest of the pattern structure."
  (map-pattern 'list (lambda (pattern recurse)
                       (flet ((reconstitute ()
                                (reconstitute-pattern pattern (funcall recurse))))
                         (or (funcall function pattern recurse #'reconstitute)
                             (reconstitute))))
               pattern))

;; map-patterns-and-paths family of functions

(defun map-patterns-and-paths (result-type function pattern)
  "Like `map-pattern' but FUNCTION is called with two arguments:
   1. the pattern 2. the path of the pattern in the pattern
   tree.

   The path argument is a list of the form

     ((PATTERN-KIND1 . PATTERN-KEY1) ...)

   where PATTERN-KEYN is the result of calling `pattern-keys' on the
   respective containing patterns.

   Example:

     (map-patterns-and-paths
      'list (lambda (pattern path recurse)
              (list* path (reduce #'append (funcall recurse))))
      (optima.core:parse-pattern '(and 1 x)))
     => (NIL
         ((AND-PATTERN . 0))
         ((AND-PATTERN . 1)))"
  (labels ((empty-path ()
             '())
           (process-pattern (pattern r next-path)
             (multiple-value-bind (subpattern-keys pattern-key)
                 (pattern-keys pattern)
               (let* ((path         (funcall next-path))
                      (kind         (class-name (class-of pattern)))
                      (kind-and-key (if pattern-key
                                        (cons kind pattern-key)
                                        kind)))
                 (labels ((next-path ()
                            (list* (cons kind-and-key (pop subpattern-keys)) path))
                          (recurse ()
                            (funcall r (rcurry #'process-pattern #'next-path))))
                   (funcall function pattern (reverse path) #'recurse))))))
    (map-pattern result-type (rcurry #'process-pattern #'empty-path) pattern)))

(defun mapc-patterns-and-paths (function pattern)
  "Like `map-patterns-and-paths', call FUNCTION with each pattern and
   path in the pattern tree PATTERN.

   This function is only called for side effects and does not return
   any values."
  (map-patterns-and-paths nil (lambda (pattern path recurse)
                                (funcall function pattern path)
                                (funcall recurse))
                          pattern)
  (values))

(defun map-patterns-and-paths/reconstitute (function pattern)
  "Like `map-patterns-and-paths', but construct new patterns from
   return values of FUNCTION.

   Example:

     (map-patterns-and-paths/reconstitute
      (lambda (pattern path recurse reconstitute)
        (print path)
        (when (and (typep pattern 'optima.core:variable-pattern)
                   (typep (lastcar path) '(cons (eql optima.core:cons-pattern)
                                                (eql 1))))
          (optima.core:make-variable-pattern)))
      (optima.core:parse-pattern '(and 1 (cons x y))))
     => (AND 1 (CONS OPTIMA::X OPTIMA::_))

   Note that only the variable-pattern in the cdr position has been
   modified."
  (map-patterns-and-paths
   'list (lambda (pattern path recurse)
           (flet ((reconstitute ()
                    (reconstitute-pattern pattern (funcall recurse))))
             (or (funcall function pattern path recurse #'reconstitute)
                 (reconstitute))))
   pattern))

;;; Specialized pattern predicates and transformations

(defun pattern-subpatterns-unrestricted-p (pattern)
  ;; TODO check for "at least top-pattern" instead
  (every (of-type 'variable-pattern) (pattern-subpatterns pattern)))

(defun mapc-variables-and-paths (function pattern
                                 &key
                                 include-unnamed
                                 include-uninterned)
  "Call FUNCTION with the name and path of each `variable-pattern' in
   the pattern tree PATTERN.

   The path argument is a list of the form

     ((PATTERN-KIND1 . PATTERN-KEY1) ...)

   where PATTERN-KEYN is the result of calling `pattern-keys' on the
   respective containing patterns.

   INCLUDE-UNNAMED controls whether variables with name NIL should be
   included.

   INCLUDE-UNINTERNED controls whether variables named by uninterned
   symbols should be included.

   This function is only called for side effects and does not return
   any values."
  (mapc-patterns-and-paths
   (lambda (pattern path)
     (typecase pattern
       (variable-pattern
        (let ((name (variable-pattern-name pattern)))
          (when (and (or include-unnamed name)
                     (or include-uninterned (symbol-package name)))
            (funcall function name path))))))
   pattern))

(defun pattern-variables-and-paths (pattern
                                    &key
                                    include-unnamed
                                    include-uninterned)
  "Return a list of variables and their respective path in the pattern
   tree PATTERN with elements of the form

     (NAME . PATH)

   where PATH is a list of the form

     ((PATTERN-KIND1 . PATTERN-KEY1) ...)

   where PATTERN-KEYN is the result of calling `pattern-keys' on the
   respective containing patterns.

   INCLUDE-UNNAMED controls whether variables with name NIL should be
   included.

   INCLUDE-UNINTERNED controls whether variables named by uninterned
   symbols should be included.

   For example, the return value for a `cons-pattern' of the
   form (cons a b) would be

     ((A . ((cons-pattern . 0))) (b . ((cons-pattern . 1))))

   "
  (let ((result '()))
    (mapc-variables-and-paths
     (lambda (name path)
       (push (cons name path) result))
     pattern
     :include-unnamed include-unnamed :include-uninterned include-uninterned)
    (nreverse result)))

(defun pattern-anonymize-variables (pattern)
  "Return a copy of PATTERN in which all variables are
   anonymous (i.e. have the name NIL)."
  (map-pattern/reconstitute
   (lambda (pattern recurse reconstitute)
     (declare (ignore recurse reconstitute))
     (typecase pattern
       (variable-pattern (make-variable-pattern))
       (guard-pattern    pattern)))
   pattern))

(defun make-predicate-form (pattern object-var)
  `(optima:match ,object-var
     (,(pattern-anonymize-variables pattern)
       t)))

(defun make-predicate (pattern)
  "Return a function of one argument that returns true when its
   argument matches PATTERN.

   This function mainly solves the problem of avoiding unused variable
   warnings that would otherwise be generated when PATTERN contains
   variables."
  (with-unique-names (object-var)
    (compile nil `(lambda (,object-var)
                    ;; optima may compile everything away ...
                    (declare (ignorable ,object-var))
                    ,(make-predicate-form pattern object-var)))))
