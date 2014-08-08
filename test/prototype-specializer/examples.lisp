;;;; examples.lisp --- Test prototype-specializer examples.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:prototype-specializer.test)

(in-suite :specializable.prototype-specializer)

(test example.prototype-specializer
  "Test compiling and loading the prototype-specializer example."

  (test-example "prototype-specializer"))
