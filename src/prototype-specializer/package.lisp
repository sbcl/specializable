;;;; package.lisp --- Package definition for the language-extension.prototype-specializer system.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:prototype-specializer
  (:use
   #:cl
   #:specializable)

  ;; Prototype object protocol
  (:export
   #:add-delegation
   #:remove-delegation
   #:map-delegations

   #:clone)

  ;; Prototype class and root object
  (:export
   #:prototype-object

   #:/root/)

  ;; Prototype generic function
  (:export
   #:prototype-generic-function))
