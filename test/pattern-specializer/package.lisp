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

  (:export
   #:run-tests))

(cl:in-package #:pattern-specializer.test)

(def-suite :pattern-specializer)

(defun run-tests ()
  (run! :pattern-specializer))
