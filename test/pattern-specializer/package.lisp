;;;; package.lisp ---
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:pattern-specializer.test
  (:use
   #:cl
   #:alexandria

   #:fiveam

   #:pattern-specializer)

  (:import-from #:pattern-specializer
   #:pattern-more-specific-p) ; TODO export this?

  (:export
   #:run-tests))

(cl:in-package #:pattern-specializer.test)

(def-suite :pattern-specializer)

(defun run-tests ()
  (run! :pattern-specializer))
