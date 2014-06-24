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

   #:make-method-lambda-using-specializers)

  (:documentation
   "This package contains the implementation of pattern specializers
    based on the optima system."))
