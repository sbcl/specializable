;;;; type-specializer.lisp --- Unit tests for the type specializer.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:type-specializer.test)

;;; Utilities

(defmacro with-type-generic-function ((name lambda-list &rest options)
                                           &body body)
  `(with-specializable-generic-function
       (type-generic-function ,name ,lambda-list ,@options)
     ,@body))

;; Test suite

(in-suite :specializable.type-specializer)
