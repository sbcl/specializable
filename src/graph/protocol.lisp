;;;; package.lisp --- Package definition for the specializable-graph system.
;;;;
;;;; Copyright (C) 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:specializable.graph)

;;; Specializer labels protocol

(defgeneric specializer-html-label (graph specializer)
  (:documentation
   "Return a label for the node representing SPECIALIZER in GRAPH."))

;;; Graph

(defgeneric specializer-graph-generic-function (graph)
  (:documentation
   "Return the generic function for which GRAPH is computed."))

(defgeneric specializer-graph-argument-position (graph)
  (:documentation
   "Return the index of the mandatory parameter for which GRAPH is computed."))

(defgeneric specializer-graph-argument (graph)
  (:documentation
   "Return the parameter value for which GRAPH is computed."))

(defgeneric specializer-graph-generalizer (graph)
  (:documentation
   "Return the generalizer object for which GRAPH is computed.

    The generalizer object is derived from the argument for which
    GRAPH is computed."))

(defgeneric specializer-graph-specializers (graph)
  (:documentation
   "Return the list of specializers corresponding to the nodes of GRAPH."))

(defgeneric make-specializer-graph (generic-function argument-position argument)
  (:documentation
   "Return a specializer graph for GENERIC-FUNCTION, ARGUMENT-POSITION and ARGUMENT.

    GENERIC-FUNCTION from the methods of which specializers are
    collected. Specializers appearing at ARGUMENT-POSITION in the
    respective method lambda list are taken into account.

    ARGUMENT is a value for the parameter at ARGUMENT-POSITION in the
    generic function lambda-list. The value controls which
    specializers are applicable, as well potentially, the order of the
    applicable specializers."))
