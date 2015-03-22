;;;; type-specializer.lisp --- Unit tests for the type specializer.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:type-specializer.test
  (:use
   #:cl
   #:alexandria ; for `set-equal'
   #:fiveam

   #:type-specializer)

  (:import-from #:specializable-test
   #:with-specializable-generic-function

   #:test-example)

  (:export
   #:run-tests))

(cl:in-package #:type-specializer.test)

(def-suite :specializable.type-specializer
    :in :specializable)

(defun run-tests ()
  (run! :specializable.type-specializer))
