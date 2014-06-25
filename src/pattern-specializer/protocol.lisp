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

(defgeneric specializer-accepts-p-function (specializer)
  (:documentation
   "Return a function a function of one argument that return true if
    SPECIALIZER accepts the supplied object."))

;;; Pattern generic function protocol

(defgeneric generic-function-generalizer-makers (generic-function)
  (:documentation
   "TODO"))
