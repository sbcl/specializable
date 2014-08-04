;;;; package.lisp --- Package definition for tests of the pattern-specializer system.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:pattern-specializer.test
  (:use
   #:cl
   #:alexandria

   #:fiveam

   #:pattern-specializer

   #:specializable-test)

  (:import-from #:optima
   #:guard)

  (:import-from #:pattern-specializer
   #:pattern-specializer

   #:make-specializer-component
   #:specializer-component-add-specializer
   #:specializer-component-remove-specializer

   #:make-required-parameter-info

   #:required-parameter-info-ensure-component-for
   #:required-parameter-info-add-specializer
   #:required-parameter-info-remove-specializer

   #:required-parameter-info-ensure-binding-slot)

  (:export
   #:run-tests))

(cl:in-package #:pattern-specializer.test)

(def-suite :pattern-specializer)

(defun run-tests ()
  (run! :pattern-specializer))
