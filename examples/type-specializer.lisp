;;;; type-specializer.lisp --- type specializer examples.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:type-specializer.example
  (:use
   #:cl
   #:alexandria
   #:type-specializer))

(cl:in-package #:type-specializer.example)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric dispatch (a)
    (:generic-function-class type-generic-function)
    (:method-combination list)))

(defmethod dispatch list ((a (type integer)))
  (list a 'integer))
(assert (equal (dispatch 0) '((0 integer))))

(defmethod dispatch list ((a (type (integer 1 10))))
  (list a '(integer 1 10)))
(assert (equal (dispatch 0) '((0 integer))))
(assert (equal (dispatch 1) '((1 (integer 1 10)) (1 integer))))

(defmethod dispatch list ((a (type (cons (eql 0) integer))))
  (list a '(cons (eql 0) integer)))
(assert (equal (dispatch '(0 . 0)) '(((0 . 0) (cons (eql 0) integer)))))
(assert (equal (dispatch '(0 . 1)) '(((0 . 1) (cons (eql 0) integer)))))

(defmethod dispatch list ((a (type (cons integer (eql 0)))))
  (list a '(cons integer (eql 0))))
(assert (set-equal (dispatch '(0 . 0))
                   '(((0 . 0) (cons (eql 0) integer))
                     ((0 . 0) (cons integer (eql 0))))
                   :test #'equal))
(assert (equal (dispatch '(0 . 1)) '(((0 . 1) (cons (eql 0) integer)))))

(defmethod dispatch list ((a (type (cons (eql 0) (eql 0)))))
  (list a '(cons (eql 0) (eql 0))))
(let ((result (dispatch '(0 . 0))))
  (assert (equal (first result) '((0 . 0) (cons (eql 0) (eql 0)))))
  (assert (set-equal (rest result)
                     '(((0 . 0) (cons (eql 0) integer))
                       ((0 . 0) (cons integer (eql 0))))
                     :test #'equal)))
