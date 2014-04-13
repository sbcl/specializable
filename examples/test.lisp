;;;; test.lisp ---
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:cl-user)

(defgeneric test-match/1 (thing &key &allow-other-keys)
  (:generic-function-class pattern-specializer:pattern-generic-function))

(defmethod test-match/1 ((thing (pattern-specializer:pattern (cons n "bla"))))
  (list (list :cons-n-string-bla n)
        (when (next-method-p)
          (call-next-method))))

(defmethod test-match/1 ((thing (pattern-specializer:pattern (cons n "bli"))))
  (list (list :cons-n-string-bli n)
        (when (next-method-p)
          (call-next-method))))

(defmethod test-match/1 :around ((thing (pattern-specializer:pattern (cons 1 "bli"))))
  (list :around-cons-1-string-bli
        (when (next-method-p)
          (call-next-method))))

(defmethod test-match/1 ((thing (pattern-specializer:pattern (cons 1 "bli"))))
  :cons-1-string-bli)

(defmethod test-match/1 ((thing (pattern-specializer:pattern (cons n m))))
  (list :cons-n-m n m))

(test-match/1 (cons 5 "bla"))
(test-match/1 (cons 1 "bli"))
(test-match/1 (cons 1 "blu"))

(defgeneric test-match/2 (thing1 thing2 &key foo)
  (:generic-function-class pattern-specializer:pattern-generic-function))

(defmethod test-match/2 ((thing1 (pattern-specializer:pattern (cons 1 "bla")))
                         (thing2 (pattern-specializer:pattern (cons 2 "bla")))
                         &key foo)
  :cons-1-string-bla-cons-2-string-bla)

(test-match/2 (cons 1 "bla") (cons 2 "bla"))
(test-match/2 (cons 1 "bli") (cons 2 "bla"))
(test-match/2 (cons 1 "blu") (cons 2 "bla"))



(defgeneric test-match/3 (thing1 thing2 thing3
                          &rest bla)
  (:generic-function-class pattern-specializer:pattern-generic-function))

(defmethod test-match/3 ((thing1 (pattern-specializer:pattern (cons 1 my-var)))
                         (thing2 t)
                         (thing3 (pattern-specializer:pattern (cons 3 "bla")))
                         &rest bla)
  (list thing1 thing2 :cons-3-string-bla my-var bla))

(test-match/3 (cons 1 "bla") :bar (cons 3 "bla"))
(test-match/3 (cons 1 "blu") :bar (cons 3 "bla"))
(test-match/3 (cons 1 "bli") (cons 2 "bla"))
