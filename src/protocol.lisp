;;;; protocol.lisp --- Protocol provided by the specializable system.
;;;;
;;;; Copyright (C) 2014, 2015 Christophe Rhodes, Jan Moringen
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
;;; When additional arguments have to be passed to the effective
;;; method, a method can be defined on the generic function
;;;
;;;   compute-effective-arguments-function generic-function num-required
;;;
;;;     Return a function with lambda-list (args generalizers) which
;;;     produces the \"effective argument list\".
;;;
;;; An additional method on the following generic function can be
;;; defined to improve performance of generalizer object computation
;;; by avoiding generic function calls of
;;; `generalizer-of-using-class':
;;;
;;;   compute-argument-generalizing-function generic-function arg-position
;;;
;;;     Return a function of one argument that computes generalizer
;;;     objects.
;;;
;;; Specializers used in methods of the generic function have to
;;; implement the protocol for extended specializers below. Similarly,
;;; for generalizers returned by `generalizer-of-using-class' and via
;;; `compute-argument-generalizing-function', the generalizer protocol
;;; below has to be implemented.

(defgeneric generalizer-of-using-class (generic-function object arg-position)
  (:documentation
   "Return a generalizer object representing OBJECT (the
    ARG-POSITION-th argument with which GENERIC-FUNCTION is being
    called). ARG-POSITION is the position of object in the (required
    portion of the) lambda-list of GENERIC-FUNCTION.

    This function is called once for each pair of required argument
    OBJECT and its position in the lambda-list ARG-POSITION.

    ARG-POSITION can be used to employ different strategies for
    mapping OBJECT to a generalizer depending on the corresponding
    required parameter of GENERIC-FUNCTION. For example, it could be
    the case that OBJECT itself or CLASS-OF OBJECT is a suitable
    generalizer for the first required argument but not for other
    required required arguments."))

(defgeneric compute-argument-generalizing-function (generic-function arg-position)
  (:documentation
   "Return a function that turns an argument into a generalizer
    object.

    When called with the required argument at ARG-POSITION in a call
    of GENERIC-FUNCTION, the function must compute and return the
    generalizer object for that argument.

    This can be accomplishes by calling the generic function
    `generalizer-of-using-class'. There is a default default method
    which does exactly that."))

(defgeneric compute-effective-arguments-function (generic-function num-required)
  (:documentation
   "Return a function for producing the \"effective argument\" list.

    The returned function has the lambda-list (args generalizers) and
    produces the \"effective argument list\" for a call of
    GENERIC-FUNCTION when called with the lists of arguments and
    generalizer objects of a particular generic function call.

    One such effective argument list computation could consist in
    injecting additional arguments for the effective method based on
    the supplied generalizers."))

(defgeneric compute-applicable-methods-using-generalizers (generic-function generalizers)
  (:documentation
   "For the list of generalizer objects GENERALIZERS, return two values:

    1. the list of methods in GENERIC-FUNCTION applicable to the
       arguments represented by the generalizer objects.
    2. a Boolean indicating whether the first value is \"definitive\"
       meaning that
       a) calling `compute-applicable-methods' is not necessary
       b) the first value can be cached for future calls with
          equivalent GENERALIZERS."))

;;; Generalizer protocol
;;;
;;; It has to be efficiently possible to determine the equivalence of
;;; two given generalizer objects.
;;;
;;; To enable this, methods on the following generic function have to
;;; be provided for generalizers:
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
;;; 2. to establish an order according to specificity w.r.t. a
;;;    given generalizer object
;;;
;;; To achieve this, methods on the following generic functions have
;;; to be provided for extended specializers:
;;;
;;;   specializer-accepts-generalizer-p generic-function specializer generalizer
;;;
;;;     Indicate whether SPECIALIZER accepts GENERALIZER in the sense
;;;     that if all specializers of a given method accept their
;;;     respective generalizers, the method is applicable.
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
    generalizers, the method is applicable) by returning two values

    1. a Boolean indicating whether SPECIALIZER can principally accept
       GENERALIZER
    2. a Boolean indicating whether the first value is \"definitive\"
       meaning that
       a) calling `specializer-accepts-p' is not necessary
       b) computations based on the first value can be cached for
          future calls with an equivalent GENERALIZER."))

(defgeneric specializer-accepts-p (specializer object)
  (:documentation
   "Return true if SPECIALIZER accepts OBJECT in the sense that if all
    specializers of a given method accept their respective arguments,
    the method is applicable."))

(defgeneric specializer< (generic-function specializer1 specializer2 generalizer)
  (:documentation
   "Compare the specificity of SPECIALIZER1 and SPECIALIZER2
    w.r.t. GENERALIZER and return any of the following symbols in the
    `common-lisp' package:

    =  if SPECIALIZER1 and SPECIALIZER2 are equally specific
       w.r.t. GENERALIZER (TODO does that imply SAME-SPECIALIZER-P)?

    <  if SPECIALIZER1 is more specific than SPECIALIZER2
       w.r.t. GENERALIZER

    >  if SPECIALIZER1 is less specific than SPECIALIZER2
       w.r.t. GENERALIZER

    // if the set of objects accepted by SPECIALIZER1 is disjoint from
       the set of objects accepted by SPECIALIZER2

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
