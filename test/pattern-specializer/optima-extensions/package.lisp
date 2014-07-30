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
   #:guard)

  (:import-from #:optima.core
   #:parse-pattern #:unparse-pattern)

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
