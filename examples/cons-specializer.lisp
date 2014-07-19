;;;; cons-specializer.lisp --- cons specializer examples.
;;;;
;;;; Copyright (C) 2013, 2014 Christophe Rhodes,
;;;;
;;;; Author: Christophe Rhodes <csr21@cantab.net>

(cl:defpackage #:cons-specializer.example
  (:use
   #:cl
   #:cons-specializer))

(cl:in-package #:cons-specializer.example)

;;; Walk example

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric walk (form)
    (:generic-function-class cons-generic-function)))

(defmethod walk ((form symbol))
  `(lookup ,form))
(defmethod walk ((form cons))
  `(call (flookup ,(car form)) (list ,@(mapcar #'walk (cdr form)))))
(defmethod walk ((form (cons quote)))
  (cadr form))
(defmethod walk ((form (cons let)))
  (let ((bindings (cadr form)))
    `(with-bindings ,bindings ,@(mapcar #'walk (cddr form)))))

(assert (equal (walk t) '(lookup t)))
(assert (equal (walk nil) '(lookup nil)))
(assert (equal (walk '(foo bar)) '(call (flookup foo) (list (lookup bar)))))
(assert (equal (walk '(quote bar)) 'bar))
(assert (equal (walk '(let foo bar)) '(with-bindings foo (lookup bar))))

;;; Multiple class specializers example

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric multiple-class-specializers (x)
    (:generic-function-class cons-generic-function)
    (:method-combination list)))

(defmethod multiple-class-specializers list ((x t)) 't)
(defmethod multiple-class-specializers list ((x cons)) 'cons)
(defmethod multiple-class-specializers list ((x (cons foo))) '(cons foo))
(defmethod multiple-class-specializers list ((x (cons bar))) '(cons bar))
(defmethod multiple-class-specializers list ((x list)) 'list)
(defmethod multiple-class-specializers list ((x null)) 'null)
(defmethod multiple-class-specializers list ((x (eql nil))) '(eql nil))

(assert (equal (multiple-class-specializers nil) '((eql nil) null list t)))
(assert (equal (multiple-class-specializers t) '(t)))
(assert (equal (multiple-class-specializers (cons nil nil)) '(cons list t)))
(assert (equal (multiple-class-specializers (cons 'foo nil)) '((cons foo) cons list t)))
(assert (equal (multiple-class-specializers (list 'bar nil t 3)) '((cons bar) cons list t)))

;;; Keyword args example

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric keyword-args (x &key key1)
    (:generic-function-class cons-generic-function)))

(defmethod keyword-args ((x integer) &key key1 key2)
  (declare (ignore key1 key2))
  (1+ x))
(defmethod keyword-args ((x float) &key key1 key3)
  (declare (ignore key1 key3))
  (+ x 2.0f0))
(defmethod keyword-args :after ((x double-float) &key &allow-other-keys)
  nil)
(assert (= (keyword-args 1) 2))
(assert (= (keyword-args 1 :key1 t) 2))
(assert (= (keyword-args 1 :key2 t) 2))
(assert (= (keyword-args 1 :key1 t :key2 t) 2))
(assert (nth-value 1 (ignore-errors (keyword-args 1 :key1 t :key3 t))))
(assert (nth-value 1 (ignore-errors (keyword-args 1 :key3 t))))
(assert (= (keyword-args 1 :key3 t :allow-other-keys t) 2))

(assert (= (keyword-args 1.0f0) 3.0f0))
(assert (= (keyword-args 1.0f0 :key1 t) 3.0f0))
(assert (= (keyword-args 1.0f0 :key3 t) 3.0f0))
(assert (= (keyword-args 1.0f0 :key1 t :key3 t) 3.0f0))
(assert (nth-value 1 (ignore-errors (keyword-args 1.0f0 :key1 t :key2 t))))
(assert (nth-value 1 (ignore-errors (keyword-args 1.0f0 :key2 t))))
(assert (= (keyword-args 1.0f0 :key2 t :allow-other-keys t) 3.0f0))

(assert (= (keyword-args 1.0d0) 3.0d0))
(assert (= (keyword-args 1.0d0 :key1 t) 3.0d0))
(assert (= (keyword-args 1.0d0 :key3 t) 3.0d0))
(assert (= (keyword-args 1.0d0 :key1 t :key3 t) 3.0d0))
(assert (= (keyword-args 1.0d0 :key1 t :key2 t) 3.0d0))
(assert (= (keyword-args 1.0d0 :key2 t) 3.0d0))
(assert (= (keyword-args 1.0d0 :key2 t :allow-other-keys t) 3.0d0))
