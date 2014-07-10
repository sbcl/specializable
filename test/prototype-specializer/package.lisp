;;;; prototype-specializer.lisp --- Unit tests for the prototype specializer.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:prototype-specializer.test
  (:use
   #:cl
   #:fiveam

   #:prototype-specializer)

  (:export
   #:run-tests))

(cl:in-package #:prototype-specializer.test)

(def-suite :specializable.prototype-specializer
    :in :specializable)

(defun run-tests ()
  (run! :specializable.prototype-specializer))
