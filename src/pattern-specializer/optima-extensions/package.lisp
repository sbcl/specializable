;;;; package.lisp --- Package definition for the optima-extensions module.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:pattern-specializer.optima-extensions
  (:use
   #:cl
   #:alexandria)

  (:import-from #:optima.core
   #:parse-pattern #:unparse-pattern)

  ;; Subpattern protocol
  (:export
   #:pattern-subpatterns)

  ;; Basic pattern transformation framework
  (:export
   #:reconstitute-pattern

   ;; map-pattern family of functions
   #:map-pattern
   #:mapc-pattern
   #:map-pattern/reconstitute

   ;; map-patterns-and-paths family of functions
   #:map-patterns-and-paths
   #:mapc-patterns-and-paths
   #:map-patterns-and-paths/reconstitute)

  ;; Specialized pattern transformations
  (:export
   )

  (:documentation
   "This package contains extensions to the optima system that are
    required by the pattern-specializer system."))
