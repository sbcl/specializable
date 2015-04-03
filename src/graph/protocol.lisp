;;;; package.lisp --- Package definition for the specializable-graph system.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:specializable.graph)

(defgeneric specializer-label (graph specializer)
  (:documentation
   "Return a label for the node representing SPECIALIZER in GRAPH."))
