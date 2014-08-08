;;;; prototype-specializer.lisp --- Unit tests for the prototype specializer.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:prototype-specializer.test
  (:use
   #:cl
   #:alexandria ; for `set-equal'
   #:fiveam

   #:prototype-specializer)

  (:import-from #:specializable-test
   #:with-specializable-generic-function

   #:test-example)

  (:export
   #:run-tests))

(cl:in-package #:prototype-specializer.test)

(def-suite :specializable.prototype-specializer
    :in :specializable)

(defun run-tests ()
  (run! :specializable.prototype-specializer))
