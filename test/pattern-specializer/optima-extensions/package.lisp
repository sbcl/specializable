;;;; package.lisp --- Package definition for tests of the optima extensions.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:pattern-specializer.optima-extensions.test
  (:use
   #:cl
   #:alexandria

   #:fiveam

   #:pattern-specializer.optima-extensions)

  (:import-from #:optima
   #:guard
   #:match)

  (:import-from #:optima.core
   #:parse-pattern #:unparse-pattern

   #:constant-pattern
   #:variable-pattern #:make-variable-pattern #:variable-pattern-name
   #:cons-pattern     #:make-cons-pattern
   #:class-pattern
   #:guard-pattern
   #:not-pattern      #:make-not-pattern
   #:and-pattern      #:make-and-pattern
   #:or-pattern)

  (:export
   #:run-tests))

(cl:in-package #:pattern-specializer.optima-extensions.test)

;;; Test suite

(def-suite :pattern-specializer.optima-extensions
    :in :pattern-specializer)

(defun run-tests ()
  (run! :pattern-specializer.optima-extensions))

;;; Test utilities

(defun call-as-pattern-test-case (patterns args thunk)
  (let ((patterns/parsed (mapcar #'parse-pattern patterns)))
    (apply thunk patterns/parsed args)))

(defun make-pattern-predicate-test-case-thunk (predicate arity)
  (lambda (spec)
    (let ((patterns (subseq spec 0 arity))
          (args     (subseq spec arity (1- (length spec))))
          (expected (lastcar spec)))
      (call-as-pattern-test-case
       patterns (list expected)
       (lambda (patterns expected)
         (let ((result (apply predicate (append patterns args))))
           (is (equal expected result)
               "~@<(~S ~{~S~^ ~}) => ~S [expected ~S]~@:>"
               predicate patterns result expected)))))))

(defun make-pattern-transform-test-case-thunk (transform &rest transform-args)
  (lambda (spec)
    (destructuring-bind (pattern &rest more-transform-args) (butlast spec)
      (let ((expected (lastcar spec)))
        (call-as-pattern-test-case
         (list pattern) (append more-transform-args (list expected))
         (lambda (patterns expected)
           (let* ((pattern   (first patterns))
                  (all-args  (append transform-args more-transform-args))
                  (result    (apply transform
                                    (append all-args (list pattern))))
                  (result*   (case expected
                               (:unchanged result)
                               (t          (unparse-pattern result))))
                  (expected* (case expected
                               (:unchanged
                                pattern)
                               (t
                                (unparse-pattern (parse-pattern expected))))))
             (is (equal expected* result*)
                 "~@<(~S~@[ ~{~S~^ ~}~] ~S) => ~S [expected ~S]~@:>"
                 transform all-args pattern
                 result* expected*))))))))
