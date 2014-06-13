;;;; accept-specializer.lisp --- Unit tests for the accept specializer.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:accept-specializer.test
  (:use
   #:cl
   #:fiveam

   #:accept-specializer)

  (:export
   #:run-tests))

(cl:in-package #:accept-specializer.test)

(def-suite :specializable.accept-specializer
    :in :specializable)

(defun run-tests ()
  (run! :specializable.accept-specializer))
