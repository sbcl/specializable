;;;; lambda-calculus.lisp --- Untyped lambda calculus based on pattern specializers.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.DE>

;;;; Partially based on idea from
;;;;
;;;; [1] Benjamin C. Pierce (2002): Types and Programming languages

(cl:defpackage #:pattern-specializer.examples.lambda-calculus
  (:use
   #:cl
   #:pattern-specializer)

  (:shadow
   #:abs)

  (:import-from #:optima
   #:guard))

(cl:in-package #:pattern-specializer.examples.lambda-calculus)

;;; Syntax
;;;
;;;       ┌― app  ┌― const
;;;       │       │
;;; term ―┼― val ―┴― abs
;;;       │
;;;       └― var

(defstruct term) ; abstract

(defstruct (val (:include term))) ; abstract

(defstruct (const (:include val) (:constructor make-const (value)))
  (value nil))  ; TODO val?

(defstruct (var (:include term) (:constructor make-var (name)))
  (name nil :type symbol))

(defstruct (abs (:include val) (:constructor make-abs (var body)))
  (var nil :type var)
  (body nil :type term))

(defstruct (app (:include term) (:constructor make-app (fun arg)))
  (fun nil :type term)
  (arg nil :type val))

;;; Parse

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric parse (form)
    (:generic-function-class pattern-generic-function)))

(defmethod parse ((form integer))
  (make-const form))

(defmethod parse ((form symbol))
  (make-var form))

(defmethod parse ((form (pattern (list 'λ (guard name (symbolp name)) body))))
  (make-abs (parse name) (parse body)))

(defmethod parse ((form (pattern (list func arg))))
  (make-app (parse func) (parse arg)))

;;; Substitution

(defgeneric substitute1 (term var val))

(defmethod substitute1 ((term val) (var var) (val val))
  term)

;; [1 Page 69]
(defmethod substitute1 ((term var) (var var) (val val))
  (if (equalp term var) val term))

;; [1 Page 69]
(defmethod substitute1 ((term abs) (var var) (val val))
  ;; TODO capture
  (make-abs (abs-var term) (substitute1 (abs-body term) var val)))

;; [1 Page 69]
(defmethod substitute1 ((term app) (var var) (val val))
  (make-app (substitute1 (app-fun term) var val)
            (substitute1 (app-arg term) var val)))

;;; Evaluation

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric eval1 (term)
    (:generic-function-class pattern-generic-function)))

(defmethod eval1 ((term val))
  term)

;; Reduce function to value
;;
;;      t₁ -> t₁'
;; ―――――――――――――――――
;;  t₁ t₂ -> t₁' t₂
;;
;; [1 Page 72; Figure 5.3]
(defmethod eval1 ((term (pattern (app fun arg))))
  (eval1 (make-app (eval1 fun) arg)))

;; Reduce argument to value
;;
;;      t₂ -> t₂'
;; ―――――――――――――――――
;;  v₁ t₂ -> v₁ t₂'
;;
;; [1 Page 72; Figure 5.3]
(defmethod eval1 ((term (pattern (app (fun (and fun (val))) arg))))
  (eval1 (make-app fun (eval1 arg))))

;; Application
;;
;; (λx.t₁₂) v₂ -> [x -> v₂] t₁₂
;;
;; [1 Page 72; Figure 5.3]
(defmethod eval1 ((term (pattern (app (fun (abs var body)) (arg (and arg (val)))))))
  (let ((arg-value (eval1 arg)))
    (eval1 (substitute1 body var arg-value))))

;;; Test

;; Evaluation stops at values which can be constants or abstractions.
(assert (equalp (eval1 (make-const 1)) (make-const 1)))
(assert (equalp (eval1 (make-abs (make-var 'x) (make-const 1)))
                (make-abs (make-var 'x) (make-const 1))))

;; Applications of abstractions are reduced.
(assert (equalp (eval1 (make-app (make-abs (make-var 'x) (make-var 'x)) (make-const 1)))
                (make-const 1)))

;; Parsing and evaluation
(assert (equalp (eval1 (parse '(((λ z (λ y z)) 5) 6))) (make-const 5)))
(assert (equalp (eval1 (parse '(((λ z (λ y y)) 5) 6))) (make-const 6)))

;; Local Variables:
;; coding: utf-8
;; End:
