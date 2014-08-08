;;;; examples.lisp --- Test cons-specializer examples.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:cons-specializer.test)

(in-suite :specializable.cons-specializer)

(test example.cons-specializer
  "Test compiling and loading the cons-specializer example."

  (test-example "cons-specializer"))
