;;;; examples.lisp --- Test examples.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:specializable-test)

(in-suite :specializable)

(test example.signum-specializer
  (test-example "signum-specializer"))
