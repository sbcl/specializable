;;;; specializable.lisp --- Unit tests of the specializable system.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package "SPECIALIZABLE-TEST")

(in-suite :specializable)

;;; Argument precedence order

(macrolet
    ((define-a-p-o-generic-function (name &rest order)
       `(progn
          (eval-when (:compile-toplevel :load-toplevel :execute)
            (defgeneric ,name (a b)
              (:generic-function-class specializable-generic-function)
              (:argument-precedence-order ,@order)
              (:method-combination list)))
          (defmethod ,name list ((a integer) (b integer))
            '(integer integer))
          (defmethod ,name list ((a integer) (b real))
            '(integer real))
          (defmethod ,name list ((a real)    (b integer))
            '(real integer))
          (defmethod ,name list ((a real)    (b real))
                     '(real real))
          (reinitialize-instance
           #',name :disabled-optimizations '(:standard-discrimination)))))
  (define-a-p-o-generic-function argument-precedence-order.1 a b)
  (define-a-p-o-generic-function argument-precedence-order.2 b a))

(test argument-precedence-order.smoke
  (is (equal '((integer integer) (integer real)
               (real integer)    (real real))
             (argument-precedence-order.1 1 1)))
  (is (equal '((integer integer) (real integer)
               (integer real)    (real real))
             (argument-precedence-order.2 1 1))))
