;;;; package.lisp --- Package definition for the language-extension.pattern-specializer system.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:pattern-specializer
  (:use
   #:cl
   #:alexandria

   #:pattern-specializer.optima-extensions)

  (:import-from #:sb-mop

   #:funcallable-standard-class
   #:set-funcallable-instance-function

   #:specializer
   #:specializer-direct-methods

   #:method-specializers
   #:method-function

   #:compute-discriminating-function
   #:compute-effective-method

   #:generic-function-name
   #:generic-function-methods
   #:add-direct-method
   #:remove-direct-method)

  (:import-from #:sb-pcl
   #:parse-specializer-using-class
   #:unparse-specializer-using-class
   #:make-specializer-form-using-class

   #:specializer-type-specifier

   #:make-method-lambda-using-specializers)

  (:import-from #:optima
   #:match #:ematch)

  (:import-from #:optima.core
   #:parse-pattern #:unparse-pattern

   #:constant-pattern                         #:constant-pattern-value
   #:variable-pattern #:make-variable-pattern #:variable-pattern-name
   #:guard-pattern
   #:and-pattern      #:make-and-pattern)

  ;; Specifier symbol for the pattern specializer
  (:export
   #:pattern)

  ;; Conditions
  (:export
   #:pattern-variable-name-error
   #:pattern-variable-name-error-specializer
   #:pattern-variable-name-error-name)

  ;; Pattern specializer class
  (:export
   #:pattern-specializer
   #:specializer-pattern ; TODO keep both? renamed to pattern-specializer-[parsed-]pattern?
   #:specializer-parsed-pattern)

  ;; Generic function and method
  (:export
   #:pattern-generic-function

   #:pattern-method)

  (:documentation
   "This package contains the implementation of pattern specializers
    based on the optima system."))
