;;;; protocol.lisp --- Protocol provided by the specializable system.
;;;;
;;;; Copyright (C) 2014 Christophe Rhodes, Jan Moringen
;;;;
;;;; Author: Christophe Rhodes <csr21@cantab.net>
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package "SPECIALIZABLE")

;; new, not in closette
(defgeneric generalizer-of-using-class (generic-function object))

(defgeneric compute-applicable-methods-using-generalizers (generic-function generalizers))

(defgeneric generalizer-equal-hash-key (generic-function generalizer))

(defgeneric specializer-accepts-generalizer-p (generic-function specializer generalizer))

;; new, not in closette
(defgeneric specializer-accepts-p (specializer object))

;; new, not in closette
(defgeneric specializer< (generic-function specializer1 specializer2 generalizer)
  (:documentation
   "Compare the specificity of SPECIALIZER1 and SPECIALIZER2
    w.r.t. GENERALIZER and return

    =  if SPECIALIZER1 and SPECIALIZER2 are equally specific
       w.r.t. GENERALIZER (TODO does that imply SAME-SPECIALIZER-P)?

    <  if SPECIALIZER1 is more specific than SPECIALIZER2
       w.r.t. GENERALIZER

    >  if SPECIALIZER1 is less specific than SPECIALIZER2
       w.r.t. GENERALIZER

    /= if there is no relation between the respective specificity of
       SPECIALIZER1 and SPECIALIZER2 w.r.t. GENERALIZER

    For example, when SPECIALIZER1, SPECIALIZER2 and GENERALIZER are
    of type CLASS, SPECIALIZER1 is more specific than SPECIALIZER2
    w.r.t. GENERALIZER if GENERALIZER occurs in the CPLs of both
    SPECIALIZER1 and SPECIALIZER2 and the position in the CPL of
    SPECIALIZER1 is smaller."))
