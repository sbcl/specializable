;;;; package.lisp --- Package for unit tests of the specializable system.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:specializable-test
  (:use
   #:cl

   #:fiveam

   #:specializable)

  ;; Test utilities
  (:export
   #:define-specializable-generic-function
   #:with-specializable-generic-function

   #:test-example)

  ;; Test execution protocol
  (:export
   #:run-tests))

(cl:in-package #:specializable-test)

(def-suite :specializable
  :description
  "Test suite of the specializable system.")

(defun run-tests ()
  (run! :specializable))
