;;;; protocol.lisp --- Protocol used by type specializers.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:type-specializer)

;;; Type specializer protocol

(defgeneric specializer-accepts-p-function (specializer) ; TODO not specific to type specializers?
  (:documentation
   "Return a function a function of one argument that return true if
    SPECIALIZER accepts the supplied object."))
