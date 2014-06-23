;;;; cons-specializer.lisp --- Unit tests for the cons specializer.
;;;;
;;;; Copyright (C) 2013, 2014 Christophe Rhodes
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Christophe Rhodes <csr21@cantab.net>
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:cons-specializer.test)

(in-suite :specializable.cons-specializer)

;;; Walk test

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

(test walk

  (mapc
   (lambda (spec)
     (destructuring-bind (input expected) spec
       (is (equal expected (walk input)))))

   '((t             (lookup t))
     (nil           (lookup nil))
     ((foo bar)     (call (flookup foo) (list (lookup bar))))
     ((quote bar)   bar)
     ((let foo bar) (with-bindings foo (lookup bar))))))

;;; Multiple class specializers test

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

(test multiple-class-specializers

  (mapc
   (lambda (spec)
     (destructuring-bind (input expected) spec
       (is (equal expected (multiple-class-specializers input)))))

   '((nil           ((eql nil) null list t))
     (t             (t))
     ((nil . nil)   (cons list t))
     ((foo . nil)   ((cons foo) cons list t))
     ((bar nil t 3) ((cons bar) cons list t)))))

;;; Keyword args test

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

(test keyword-args

  (mapc
   (lambda (spec)
     (destructuring-bind (args expected) spec
      (flet ((call ()
               (apply #'keyword-args args)))
        (case expected
          (error (signals error (call))) ; TODO more specific condition?
          (t     (is (equal expected (call))))))))

   '(((1)                                 2)
     ((1 :key1 t)                         2)
     ((1 :key2 t)                         2)
     ((1 :key1 t :key2 t)                 2)
     ((1 :key1 t :key3 t)                 error) ; key3 invalid for integer method
     ((1 :key3 t)                         error) ; likewise
     ((1 :key3 t :allow-other-keys t)     2)

     ((1.0f0)                             3.0f0)
     ((1.0f0 :key1 t)                     3.0f0)
     ((1.0f0 :key3 t)                     3.0f0)
     ((1.0f0 :key1 t :key3 t)             3.0f0)
     ((1.0f0 :key1 t :key2 t)             error) ; key2 invalid for float method
     ((1.0f0 :key2 t)                     error) ; likewise
     ((1.0f0 :key2 t :allow-other-keys t) 3.0f0)

     ((1.0d0)                             3.0d0)
     ((1.0d0 :key1 t)                     3.0d0)
     ((1.0d0 :key3 t)                     3.0d0)
     ((1.0d0 :key1 t :key3 t)             3.0d0)
     ((1.0d0 :key1 t :key2 t)             3.0d0)
     ((1.0d0 :key2 t)                     3.0d0)
     ((1.0d0 :key2 t :allow-other-keys t) 3.0d0))))
