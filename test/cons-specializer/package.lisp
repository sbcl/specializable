;;;; cons-specializer.lisp --- Unit tests for the cons specializer.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:cons-specializer.test
  (:use
   #:cl
   #:fiveam

   #:cons-specializer)

  (:export
   #:run-tests))

(cl:in-package #:cons-specializer.test)

(def-suite :specializable.cons-specializer
    :in :specializable)

(defun run-tests ()
  (run! :specializable.cons-specializer))
