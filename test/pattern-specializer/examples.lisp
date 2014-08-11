;;;; examples.lisp --- Test pattern-specializer examples.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer.test)

(in-suite :pattern-specializer)

(test example.lambda-calculus
  "Test compiling and loading the lambda-calculus example."

  (test-example "lambda-calculus"))

(test example.simplifier
  "Test compiling and loading the simplifier example."

  (test-example "simplifier"))

(test example.code-walker
  "Test compiling and loading the code-walker example."

  (test-example "code-walker"))
