;;;; package.lisp --- Package definition for the language-extension.accept-specializer system.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:accept-specializer
  (:use
   #:cl
   #:specializable)

  ;; Generic function class, specializer and method combinations
  (:export
   #:accept-generic-function

   #:accept ; specializer syntax

   #:content-negotiation
   #:content-negotiation/or)

  ;; Request adapter protocol
  (:export
   #:handle-content-type))
