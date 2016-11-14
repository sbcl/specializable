;;;; package.lisp --- Package definition for the specializable-graph system.
;;;;
;;;; Copyright (C) 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:specializable.graph
  (:use
   #:cl
   #:alexandria)

  ;; Specializer labels protocol
  (:export
   #:specializer-html-label)

  ;; Graph class
  (:export
   #:specializer-graph
   #:specializer-graph-generic-function
   #:specializer-graph-argument-position
   #:specializer-graph-argument
   #:specializer-graph-generalizer
   #:specializer-graph-specializers

   #:make-specializer-graph)

  ;; Entry point
  (:export
   #:specializer-graph))
