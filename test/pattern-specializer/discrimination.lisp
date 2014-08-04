;;;; discrimination.lisp --- Tests for discrimination-related functionality.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer.test)

(test specializer-component.splitting

  )

(let ((component (make-specializer-component '()))
      (s1 (make-instance 'pattern-specializer :pattern '(cons y x)))
      (s2 (make-instance 'pattern-specializer :pattern '(cons 1 y)))
      (s3 (make-instance 'pattern-specializer :pattern '(cons x 2))))
  (specializer-component-add-specializer component s1)
  (specializer-component-add-specializer component s2)
  (specializer-component-add-specializer component s3)

  (list
   (multiple-value-list (specializer-component-remove-specializer component s1))
   component))
