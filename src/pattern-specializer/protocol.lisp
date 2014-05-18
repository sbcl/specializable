;;;; protocol.lisp --- Protocol used by pattern specializers.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer)

;;; Pattern specializer protocol

(defgeneric specializer-accepts-generalizer-p-using-pattern
    (generic-function specializer pattern generalizer)
  (:documentation
   "Like SPECIALIZER-ACCEPTS-GENERALIZER-P but with the ability to
    dispatch on PATTERN."))

;;; Pattern generic function protocol

(defgeneric generic-function-specializer-clusters (generic-function)
  (:documentation
   "Return a list of specializer cluster for
    GENERIC-FUNCTION. Assuming GENERIC-FUNCTION has N required
    parameters, the returned list is of the following form

      ((PARAM_1-CLUSTER_1 ... PARAM_1-CLUSTER_M_1)
       ...
       (PARAM_N-CLUSTER_1 ... PARAM_N-CLUSTER_M_N)

    where each PARAM_I_CLUSTER_J is a sorted list of `pattern-specializer's

      (SPECIALIZER_1 ... SPECIALIZER_L)

    such that for an object O

      SPECIALIZER_I accepts O => SPECIALIZER_K accepts O

    for K >= I."))

(defgeneric generic-function-generalizer-makers (generic-function)
  (:documentation
   "TODO"))

(defgeneric in-same-cluster-p (generic-function specializer1 specializer2)
  (:documentation
   "Return true if for GENERIC-FUNCTION SPECIALIZER1 and SPECIALIZER2
    are in the same cluster."))
