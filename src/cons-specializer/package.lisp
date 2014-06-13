;;;; package.lisp --- Package definition for the language-extension.cons-specializer system.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:cons-specializer
  (:use
   #:cl
   #:specializable)

  (:export
   #:cons-generic-function))
