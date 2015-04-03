;;;; simplifier.lisp --- Symbolic simplifier based on pattern specializers.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.DE>

(cl:defpackage #:pattern-specializer.examples.simplifier
  (:use
   #:cl
   #:pattern-specializer)

  (:import-from #:optima
   #:guard))

(cl:in-package #:pattern-specializer.examples.simplifier)

;;; Toy arithmetic simplifier

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric simplify (expr)
    (:generic-function-class pattern-generic-function)))

(defmethod simplify ((expr t))
  expr)

(defmethod simplify ((expr (pattern (list '* 0 x))))
  (declare (ignore x))
  0)

(defmethod simplify ((expr (pattern (list '* 1 x))))
  x)

(defmethod simplify ((expr (pattern (list '+ 0 x))))
  x)

(defmethod simplify ((expr (pattern (list '/ x 1))))
  x)

(defmethod simplify ((expr (pattern (list* (and op (type symbol)) args))))
  (assert op)
  (list* op (mapcar #'simplify args)))

(defmethod simplify ((expr (pattern (list* (and op (type symbol))
                                           (guard args (every #'numberp args))))))
  (apply op args))

(defmethod simplify ((expr (pattern (list* (and op (type (member + *)))
                                           (guard args (and (notany #'consp args)
                                                            (some #'constantp args)
                                                            (notevery #'constantp args)))))))
  `(,op ,(simplify `(,op ,@(remove-if-not #'constantp args)))
        ,(simplify `(,op ,@(remove-if #'constantp args)))))

;;; Example

(time
 (sb-sprof:with-profiling ()
  (loop :repeat 1000 :do
     (let ((e '(* x 2 (* y (+ 1 (- 5 6))) x)))
       (do ((expr  nil expr1)
            (expr1 e   (simplify expr1)))
           ((equalp expr expr1)
            expr1)
         (print (list expr :---> expr1)))))))
