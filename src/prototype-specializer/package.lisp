;;;; package.lisp --- Package definition for the language-extension.prototype-specializer system.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:prototype-specializer
  (:use
   #:cl
   #:alexandria
   #:specializable)

  ;; Conditions
  (:export
   #:delegation-cycle-error
   #:delegation-cycle-error-object
   #:delegation-cycle-error-delegation
   #:delegation-cycle-error-path)

  ;; Prototype object protocol
  (:export
   #:add-delegation
   #:remove-delegation
   #:map-delegations
   #:map-delegations-and-paths

   #:clone)

  ;; Prototype class and root object
  (:export
   #:prototype-object

   #:/root/)

  ;; Prototype generic function
  (:export
   #:prototype-generic-function))
