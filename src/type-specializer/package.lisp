;;;; package.lisp --- Package definition for the language-extension.type-specializer system.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:type-specializer
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

   #:specializer-type-specifier)

  ;; Type specializer class
  (:export
   #:type-specializer
   #:specializer-type)

  ;; Generic function and method
  (:export
   #:type-generic-function

   #:type-method)

  (:documentation
   "This package contains the implementation of type specializers."))
