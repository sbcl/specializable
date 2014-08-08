;;;; examples.lisp --- Test accept-specializer examples.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:accept-specializer.test)

(in-suite :specializable.accept-specializer)

(test example.accept-specializer
  "Test compiling and loading the accept-specializer example."

  (test-example "accept-specializer"))
