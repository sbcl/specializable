;;;; package.lisp --- Package definition for tests of the optima extensions.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:pattern-specializer.optima-extensions.test
  (:use
   #:cl
   #:alexandria

   #:fiveam

   #:pattern-specializer.optima-extensions)

  (:import-from #:optima
   #:guard)

  (:export
   #:run-tests))

(cl:in-package #:pattern-specializer.optima-extensions.test)

(def-suite :pattern-specializer.optima-extensions
    :in :pattern-specializer)

(defun run-tests ()
  (run! :pattern-specializer.optima-extensions))
