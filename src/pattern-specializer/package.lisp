;;;; package.lisp --- Package definition for the language-extension.pattern-specializer system.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:pattern-specializer
  (:use
   #:cl
   #:alexandria)

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

   #:make-method-lambda-using-specializers)

  ;; Specifier symbol for the pattern specializer
  (:export
   #:pattern)

  ;; Pattern specializer class
  (:export
   #:pattern-specializer
   #:specializer-pattern)

  ;; Generic function and method
  (:export
   #:pattern-generic-function

   #:pattern-method))
