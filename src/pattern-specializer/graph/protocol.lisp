;;;; protocol.lisp --- Protocol provided by the pattern-specializer.graph system.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer.graph)

(defgeneric pattern-label (pattern path context recurse)
  (:documentation
   "TODO"))
