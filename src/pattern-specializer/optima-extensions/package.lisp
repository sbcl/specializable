;;;; package.lisp --- Package definition for the optima-extensions module.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:pattern-specializer.optima-extensions
  (:use
   #:cl
   #:alexandria)

  (:import-from #:optima
   #:match

   #:guard)

  (:import-from #:optima.core
   #:parse-pattern #:unparse-pattern

   #:constant-pattern                          #:constant-pattern-value
   #:variable-pattern  #:make-variable-pattern #:variable-pattern-name
   #:complex-pattern                           #:complex-pattern-subpatterns
   #:cons-pattern      #:make-cons-pattern     #:cons-pattern-car-pattern #:cons-pattern-cdr-pattern
   #:class-pattern     #:make-class-pattern    #:class-pattern-class-name #:class-pattern-slot-names
   #:structure-pattern                         #:structure-pattern-conc-name
   #:guard-pattern                             #:guard-pattern-subpattern #:guard-pattern-test-form
   #:not-pattern       #:make-not-pattern      #:not-pattern-subpattern
   #:and-pattern       #:make-and-pattern
   #:or-pattern        #:make-or-pattern)

  ;; Subpattern protocol
  (:export
   #:pattern-subpatterns)

  ;; Pattern type specifier protocol
  (:export
   #:pattern-type-specifier)

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
