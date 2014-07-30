;;;; type-interoperation.lisp --- Tests for interoperation with types.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer.optima-extensions.test)

;;; Basic pattern transformation framework

(def-suite :pattern-specializer.optima-extensions.type-interoperation
    :in :pattern-specializer.optima-extensions)
(in-suite :pattern-specializer.optima-extensions.type-interoperation)

(test pattern-type-specifier.smoke
  "Smoke test for the `pattern-type-specifier' "

  (mapc
   (make-pattern-predicate-test-case-thunk #'pattern-type-specifier 1)

   '((1                   (eql 1))

     (x                   t)

     ((cons 1 2)          (cons (eql 1) (eql 2)))
     ((cons x y)          (cons t t))

     ((class clazz)       clazz)
     ((class clazz a)     clazz)
     ((class clazz (a 1)) clazz)

     ((strukture)         (satisfies strukture-p))
     ((strukture a)       (satisfies strukture-p))
     ((strukture (a 1))   (satisfies strukture-p))

     ((type integer)      integer)
     ((guard x (consp x)) cons)
     ((guard x (< x 5))   t) ; we give up in such cases

     ((not 1)             (not (eql 1)))
     ((not x)             (not t))

     ((and 1 2)           (and (eql 1) (eql 2)))

     ((or 1 2)            (or (eql 1) (eql 2))))))
