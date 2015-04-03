;;;; package.lisp --- Package definition for the specializable-graph system.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:specializable.graph
  (:use
   #:cl
   #:alexandria)

  ;; Protocol
  (:export
   #:specializer-label)

  ;; Graph class
  (:export
   #:specializer-graph
   #:specializer-graph-generic-function
   #:specializer-graph-argument-position
   #:specializer-graph-argument
   #:specializer-graph-generalizer)

  ;; Entry point
  (:export
   #:specializer-graph))
