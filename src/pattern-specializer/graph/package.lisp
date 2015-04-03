;;;; package.lisp --- Package definition for the pattern-specializer-graph system.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:pattern-specializer.graph
  (:use
   #:cl
   #:alexandria

   #:pattern-specializer
   #:pattern-specializer.optima-extensions

   #:specializable.graph))
