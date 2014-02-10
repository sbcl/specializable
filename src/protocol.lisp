;;;; protocol.lisp --- Protocol provided by the specializable system.
;;;;
;;;; Copyright (C) 2014 Christophe Rhodes, Jan Moringen
;;;;
;;;; Author: Christophe Rhodes <csr21@cantab.net>
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package "SPECIALIZABLE")

;;; Protocol for specializable generic functions
;;;
;;; In order for a generic function to support dispatch with extended
;;; specializers, there have to be methods on the generic functions
;;;
;;;   generalizer-of-using-class generic-function object arg-position
;;;
;;;     Return a generalizer object representing OBJECT (the
;;;     ARG-POSITION-th argument with which GENERIC-FUNCTION is being
;;;     called). This is called once for each required argument.
;;;
;;;   compute-applicable-methods-using-generalizers generic-function generalizers
;;;
;;;     For a list of generalizer objects returned by
;;;     `generalizer-of-using-class', compute and return a list of
;;;     applicable methods.
;;;
;;; Specializers used in methods of the generic function have to
;;; implement the protocol for extended specializers below. Similarly,
;;; for generalizers returned by `generalizers-of-using-class', the
;;; generalizer protocol below has to be implemented.

(defgeneric generalizer-of-using-class (generic-function object arg-position)
  (:documentation
   "Return a generalizer object representing OBJECT (the
    ARG-POSITION-th argument with which GENERIC-FUNCTION is being
    called). ARG-POSITION is the position of object in the (required
    portion of the) lambda-list of GENERIC-FUNCTION.

    This is called once for each pair of required argument OBJECT and
    its position in the lambda-list ARG-POSITION."))

(defgeneric compute-applicable-methods-using-generalizers (generic-function generalizers)
  (:documentation
   "For the list of generalizer objects GENERALIZERS, return two values:

    1. the list of methods in GENERIC-FUNCTION applicable to the
       arguments represented by the generalizer objects.
    2. a Boolean indicating whether the first value is definitive
       meaning that
       a) calling `compute-applicable-methods' is not necessary
       b) the first value can be cached for future calls with
          equivalent GENERALIZERS."))

;;; Generalizer protocol
;;;
;;; It has be efficiently possible to determine the equivalence of two
;;; given generalizer objects.
;;;
;;; To enable this, for generalizers methods on the following generic
;;; function have to be provided:
;;;
;;;   generalizer-equal-hash-key generic-function generalizer
;;;
;;;     Return an object suitable for establishing equivalence of
;;;     GENERALIZER to other generalizers by using `equal'-hashing.
;;;
;;; This equivalence can be used to determine whether a cached
;;; effective-method computation for previous generalizer objects can
;;; be reused for different list of generalizers.

(defgeneric generalizer-equal-hash-key (generic-function generalizer)
  (:documentation
   "Return an object suitable for establishing equivalence of
    GENERALIZER to other generalizers by using `equal'-hashing."))

;;; Protocol for extended specializers
;;;
;;; For extended specializers, it has to be possible
;;;
;;; 1. to test whether they accept arguments supplied in a generic
;;;    function call (based on generalizer objects representing the
;;;    arguments and based on the arguments themselves)
;;; 2. to establish an order according to specificity w.r.t. to a
;;;    given generalizer object
;;;
;;; To achieve this, the methods on the following generic functions
;;; have to be provided for extended specializers:
;;;
;;;   specializer-accepts-generalizer-p generic-function specializer generalizer
;;;
;;;     Indicate whether SPECIALIZER accepts GENERALIZER in the sense
;;;     that if all specializers of a given method accept their
;;;     respective arguments, the method is applicable.
;;;
;;;     Called prior to `specializer-accepts-p' to potentially compute
;;;     a cachable set of applicable methods based on generalizers
;;;     alone.
;;;
;;;   specializer-accepts-p specializer object
;;;
;;;     Return true if SPECIALIZER accepts OBJECT in the sense that if
;;;     all specializers of a given method accept their respective
;;;     arguments, the method is applicable.
;;;
;;;     Only called if the information returned by
;;;     `specializer-accepts-generalizer-p' was not definitive.
;;;
;;;   specializer< generic-function specializer1 specializer2 generalizer
;;;
;;;     Return the ordering relation (one of =, <, >, /=) of
;;;     SPECIALIZER1 and SPECIALIZER2 w.r.t. GENERALIZER.
;;;
;;;     This is called once a set of applicable methods has been
;;;     computed to sort its elements according to specificity.

(defgeneric specializer-accepts-generalizer-p (generic-function specializer generalizer)
  (:documentation
   "Indicate whether SPECIALIZER accepts GENERALIZER (in the sense
    that if all specializers of a given method accept their respective
    arguments, the method is applicable) by returning two values

    1. a Boolean indicating whether SPECIALIZER can principally accept
       GENERALIZER
    2. a Boolean indicating whether the first value is definitive
       meaning that
       a) calling `specializer-accepts-p' is not necessary
       b) computations based on the first value can be cached for
          future calls with an equivalent GENERALIZER."))

;; new, not in closette
(defgeneric specializer-accepts-p (specializer object)
  (:documentation
   "Return true if SPECIALIZER accepts OBJECT in the sense that if all
    specializers of a given method accept their respective arguments,
    the method is applicable."))

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

(defun invert-specializer<-relation (value)
  (case value
    (< '>)
    (> '<)
    (t value)))
