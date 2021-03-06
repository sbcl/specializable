;;;; signum-specializer.lisp --- signum specializer examples.
;;;;
;;;; Copyright (C) 2013, 2014 Christophe Rhodes,
;;;;
;;;; Author: Christophe Rhodes <csr21@cantab.net>

(cl:defpackage #:signum-specializer.example
  (:use
   #:cl
   #:specializable))

(cl:in-package #:signum-specializer.example)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass signum-specializer (extended-specializer)
    ((signum :initarg :signum :reader %signum)))

  (defclass signum-generic-function (specializable-generic-function)
    ()
    (:metaclass sb-mop:funcallable-standard-class))

  (define-extended-specializer-syntax signum
    (:class signum-specializer)
    (:parser (generic-function signum)
      (declare (ignore generic-function))
      (make-instance 'signum-specializer :signum signum))
    (:unparser (generic-function specializer)
      (declare (ignore generic-function))
      (list (%signum specializer)))))

(defmethod sb-pcl::same-specializer-p
    ((s1 signum-specializer) (s2 signum-specializer))
  (= (%signum s1) (%signum s2)))

(defmethod generalizer-equal-hash-key ((gf signum-generic-function)
                                       (g signum-specializer))
  (%signum g))
(defmethod generalizer-of-using-class ((gf signum-generic-function)
                                       object arg-position)
  (typecase object
    (real (make-instance 'signum-specializer :signum (signum object)))
    (t (call-next-method))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod sb-pcl:specializer-type-specifier
      ((proto-generic-function signum-generic-function)
       (proto-method standard-method)
       (specializer signum-specializer))
    (case (%signum specializer)
      (-1 '(real  *  (0)))
      (0  '(real  0   0))
      (1  '(real (0)  *)))))
(defmethod specializer-accepts-generalizer-p
    ((gf signum-generic-function)
     (specializer signum-specializer)
     (thing signum-specializer))
  (if (= (%signum specializer) (%signum thing))
      (values t t)
      (values nil t)))
(defmethod specializer-accepts-generalizer-p ((gf signum-generic-function)
                                              (specializer sb-mop:specializer)
                                              (thing signum-specializer))
  (specializer-accepts-generalizer-p gf specializer (class-of (%signum thing))))

;;; note: this method operates in full knowledge of the object, and so
;;; does not require the generic function as an argument.
(defmethod specializer-accepts-p ((specializer signum-specializer) obj)
  (and (realp obj)
       (= (signum obj) (%signum specializer))))

(defmethod specializer< ((gf signum-generic-function)
                         (s1 signum-specializer)
                         (s2 signum-specializer) generalizer)
  (declare (ignore generalizer))
  (if (= (%signum s1) (%signum s2))
      '=
      '//))
(defmethod specializer< ((gf signum-generic-function)
                         (s1 signum-specializer)
                         (s2 class)
                         generalizer)
  '<)
(defmethod specializer< ((gf signum-generic-function)
                         (s1 signum-specializer)
                         (s2 sb-mop:eql-specializer)
                         generalizer)
  '>)
(defmethod specializer< ((gf signum-generic-function)
                         (s1 sb-mop:specializer)
                         (s2 signum-specializer)
                         generalizer)
  (invert-specializer<-relation (specializer< gf s2 s1 generalizer)))
;;; note: the need for this method is tricky: we need to translate
;;; from generalizers that our specializers "know" about to those that
;;; ordinary generic functions and specializers might know about.
(defmethod specializer< ((gf signum-generic-function)
                         (s1 sb-mop:specializer)
                         (s2 sb-mop:specializer)
                         (generalizer signum-specializer))
  (specializer< gf s1 s2 (class-of (%signum generalizer))))


;;; tests / examples

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric fact (n)
    (:generic-function-class signum-generic-function)))
(defmethod fact ((n (signum 0))) 1)
(defmethod fact ((n (signum 1))) (* n (fact (1- n))))
(assert (eql (fact 6) 720))
(assert (eql (fact 6.0) 720.0))
(defmethod no-applicable-method ((gf (eql #'fact)) &rest args)
  (declare (ignore args))
  'gotcha)
(assert (eql (fact -6) 'gotcha))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric signum-class-specializers (x)
    (:generic-function-class signum-generic-function)
    (:method-combination list)))

(defmethod signum-class-specializers list ((x float)) 'float)
(defmethod signum-class-specializers list ((x integer)) 'integer)
(defmethod signum-class-specializers list ((x (signum 1))) 1)
(assert (equal (signum-class-specializers 1.0) '(1 float)))
(assert (equal (signum-class-specializers 1) '(1 integer)))
(assert (equal (signum-class-specializers -1.0) '(float)))
(assert (equal (signum-class-specializers -1) '(integer)))
