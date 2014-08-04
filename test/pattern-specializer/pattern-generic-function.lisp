;;;; pattern-generic-function.lisp --- Tests for pattern generic function.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer.test)

;; TODO use map-permutations for these tests
;; TODO check that path-info s in binding-slot-info s are consistent with path-info s in specializer components
(test required-parameter-info.split

  )

(let* ((info (make-required-parameter-info))
       (s1 (make-instance 'pattern-specializer :pattern '(cons 1 y)))
       (s2 (make-instance 'pattern-specializer :pattern '(cons x 2)))
       (s3 (make-instance 'pattern-specializer :pattern '(cons y x))))

  (required-parameter-info-ensure-binding-slot
   info
   (first
    (pattern-specializer::specializer-component-%paths
     (nth-value 1 (required-parameter-info-add-specializer info s1)))))

  (required-parameter-info-add-specializer info s2)

  (required-parameter-info-ensure-binding-slot
   info
   (first
    (pattern-specializer::specializer-component-%paths
     (nth-value 1 (required-parameter-info-add-specializer info s3)))))

  (required-parameter-info-remove-specializer info s3))
