;;;; cons-specializer.lisp --- Specializer for dispatching on car of conses.
;;;;
;;;; Copyright (C) 2013, 2014 Christophe Rhodes,
;;;;
;;;; Author: Christophe Rhodes <csr21@cantab.net>

(cl:in-package #:cons-specializer)

(defclass cons-specializer (extended-specializer)
  ((car :initarg :car :reader %car)))
(defclass cons-generic-function (specializable-generic-function)
  ()
  (:metaclass sb-mop:funcallable-standard-class))

(define-extended-specializer cons (gf car)
  (make-instance 'cons-specializer :car car))
(defmethod sb-pcl:unparse-specializer-using-class
    ((gf cons-generic-function) (specializer cons-specializer))
  `(cons ,(%car specializer)))
(defmethod sb-pcl::same-specializer-p
    ((s1 cons-specializer) (s2 cons-specializer))
  (eql (%car s1) (%car s2)))

;;; FIXME: make a proper generalizer
(defmethod generalizer-equal-hash-key ((gf cons-generic-function) (g symbol))
  g)
(defmethod generalizer-of-using-class ((gf cons-generic-function) arg arg-position)
  (typecase arg
    ((cons symbol) (car arg))
    (t (call-next-method))))
(defmethod specializer-accepts-generalizer-p ((gf cons-generic-function) (specializer cons-specializer) thing)
  (if (eql (%car specializer) thing)
      (values t t)
      (values nil t)))
(defmethod specializer-accepts-generalizer-p ((gf cons-generic-function) (specializer sb-mop:specializer) (thing symbol))
  (specializer-accepts-generalizer-p gf specializer (find-class 'cons)))

;;; note: this method operates in full knowledge of the object, and so
;;; does not require the generic function as an argument.
(defmethod specializer-accepts-p ((specializer cons-specializer) obj)
  (and (consp obj)
       (eql (car obj) (%car specializer))))

(defmethod specializer< ((gf cons-generic-function) (s1 cons-specializer) (s2 cons-specializer) generalizer)
  (declare (ignore generalizer))
  (if (eql (%car s1) (%car s2))
      '=
      '/=))
(defmethod specializer< ((gf cons-generic-function) (s1 cons-specializer) (s2 class) generalizer)
  (declare (ignore generalizer))
  '<)
(defmethod specializer< ((gf cons-generic-function) (s1 cons-specializer) (s2 sb-mop:eql-specializer) generalizer)
  (declare (ignore generalizer))
  '>)
(defmethod specializer< ((gf cons-generic-function) (s1 sb-mop:specializer) (s2 cons-specializer) generalizer)
  (ecase (specializer< gf s2 s1 generalizer)
    ((<) '>)
    ((>) '<)))
;;; note: the need for this method is tricky: we need to translate
;;; from generalizers that our specializers "know" about to those that
;;; ordinary generic functions and specializers might know about.
(defmethod specializer< ((gf cons-generic-function) (s1 sb-mop:specializer) (s2 sb-mop:specializer) (generalizer symbol))
  (specializer< gf s1 s2 (find-class 'cons)))
