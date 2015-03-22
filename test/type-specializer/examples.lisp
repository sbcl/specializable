;;;; examples.lisp --- Test type-specializer examples.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:type-specializer.test)

(in-suite :specializable.type-specializer)

(test example.type-specializer
  "Test compiling and loading the type-specializer example."

  (test-example "type-specializer"))
