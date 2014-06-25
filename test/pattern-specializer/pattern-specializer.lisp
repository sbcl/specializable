;;;; pattern-specializer.lisp --- Tests for pattern specializers.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer.test)

;;; Utilities

(defmacro define-pattern-generic-function (name lambda-list &rest options)
  `(define-specializable-generic-function
     pattern-generic-function ,name ,lambda-list ,@options))

(defmacro with-pattern-generic-function ((name lambda-list &rest options)
                                         &body body)
  `(with-specializable-generic-function
       (pattern-generic-function ,name ,lambda-list ,@options)
     ,@body))

;;; make-method-lambda-using-specializers tests

(def-suite :pattern-specializer.make-method-lambda-using-specializers
    :in :pattern-specializer)
(in-suite :pattern-specializer.make-method-lambda-using-specializers)

(test check-name-clash.required
  "Test name clashes between pattern variables and required
   parameters."

  (with-pattern-generic-function (check-name-clash.required (a))

    (finishes
      (eval '(defmethod check-name-clash.required ((a (pattern b)))
              (declare (ignore b)))))
    (signals pattern-variable-name-error
      (eval '(defmethod check-name-clash.required ((a (pattern a)))
              (declare (ignore a)))))))

(test check-name-clash.optional
  "Test name clashes between pattern variables and optional
   parameters."

  (with-pattern-generic-function (check-name-clash.optional (a &optional b))

    (signals pattern-variable-name-error
      (eval '(defmethod check-name-clash.optional ((a (pattern b))
                                                   &optional b)
              (declare (ignore a b)))))
    (signals pattern-variable-name-error
      (eval '(defmethod check-name-clash.optional ((a (pattern bs))
                                                   &optional (b nil bs))
              (declare (ignore a b bs)))))))

(test check-name-clash.key
  "Test name clashes between pattern variables and keyword
   parameters."

  (with-pattern-generic-function (check-name-clash.key (a &key k))

    (finishes
      (eval '(defmethod check-name-clash.key ((a (pattern b))
                                              &key k)
              (declare (ignore b k)))))
    (finishes
      (eval '(defmethod check-name-clash.key ((a (pattern k))
                                              &key ((:k no-problem)))
              (declare (ignore k no-problem)))))
    (signals pattern-variable-name-error
      (eval '(defmethod check-name-clash.key ((a (pattern k))
                                              &key k)
              (declare (ignore k)))))
    (signals pattern-variable-name-error
      (eval '(defmethod check-name-clash.key ((a (pattern k))
                                              &key ((:k no-problem) nil k))
              (declare (ignore k no-problem)))))))

(test check-name-clash.rest
  "Test name clashes between pattern variables and rest parameters."

  (with-pattern-generic-function (check-name-clash.rest (a &rest r))

    (finishes
      (eval '(defmethod check-name-clash.rest ((a (pattern b)) &rest r)
              (declare (ignore a b r)))))
    (signals pattern-variable-name-error
      (eval '(defmethod check-name-clash.rest ((a (pattern r)) &rest r)
              (declare (ignore a r)))))))

(test check-name-clash.pattern
  "Test name clashes between pattern variables in different
   specializers."

  (with-pattern-generic-function (check-name-clash.pattern (a b))

    (signals pattern-variable-name-error
      (eval '(defmethod check-name-clash.pattern ((a (pattern (cons c c)))
                                                  b)
              (declare (ignore a b c)))))
    (signals pattern-variable-name-error
      (eval '(defmethod check-name-clash.pattern ((a (pattern c))
                                                  (b (pattern (cons c d))))
              (declare (ignore a c b d)))))))

;;; Next method tests

(def-suite :pattern-specializer.next-method
    :in :pattern-specializer)
(in-suite :pattern-specializer.next-method)

(test next-method.1

  ;; (cons 1 nil) --.                 (cons b (type symbol)
  ;;                  (cons b nil) --                       -- (cons b c)
  ;; (cons 2 nil) --                  (cons b (type list))
  (with-pattern-generic-function
      (next-method.1 (a)
       (:method ((a (pattern (type integer))))
         (append (list (list 'integer a))
                 (when (next-method-p)
                   (call-next-method))))

       (:method ((a (pattern (type real))))
         (append (list (list 'real a))
                 (when (next-method-p)
                   (call-next-method))))

       (:method ((a cons))
         (append (list (list 'cons a))
                 (when (next-method-p)
                   (call-next-method))))

       (:method ((a (pattern (cons b c))))
         (append (list (list '(cons var var) (list a b c)))
                 (when (next-method-p)
                   (call-next-method))))

       (:method ((a (pattern (cons b (type symbol)))))
         (append (list (list '(cons var (type symbol)) (list a b)))
                 (when (next-method-p)
                   (call-next-method))))

       (:method ((a (pattern (cons b (type list)))))
         (append (list (list '(cons var (type list)) (list a b)))
                 (when (next-method-p)
                   (call-next-method))))

       (:method ((a (pattern (cons b nil))))
         (append (list (list '(cons var nil) (list a b)))
                 (when (next-method-p)
                   (call-next-method))))

       (:method ((a (pattern (cons 1 nil))))
         (append (list (list '(cons var nil) (list a)))
                 (when (next-method-p)
                   (call-next-method))))

       (:method ((a (pattern (cons 2 nil))))
         (append (list (list '(cons var nil) (list a)))
                 (when (next-method-p)
                   (call-next-method)))))

    (next-method.1 1)
    (next-method.1 (cons 3 2))
    (next-method.1 (cons 1 'a))))

;;;

(defstruct class-pattern.super
  a)
(defstruct (class-pattern.sub (:include class-pattern.super))
  b)

(test ?

  (with-pattern-generic-function
      (class-pattern (i)
        (:method ((i (pattern 1)))
          )

        (:method ((i (pattern (type number))))
          )

        (:method ((i (pattern (type integer))))
          )

        (:method ((i (pattern (type float))))
          )

        ;; same as next specializer under current rules
        #+no(:method ((i (pattern (type class-pattern.super))))
              )

        (:method ((i (pattern (class-pattern.super))))
          )

        (:method ((i (pattern (class-pattern.sub))))
          )

        (:method ((i (pattern (class-pattern.sub a))))
          (declare (ignore a)))

        (:method ((i (pattern (class-pattern.sub b))))
          (declare (ignore b)))

        (:method ((i (pattern (class-pattern.sub (b (type real)))))))

        (:method ((i (pattern (class-pattern.sub (b (type integer)))))))

        (:method ((i (pattern (class-pattern.sub a b))))
          (declare (ignore a b)))

        (:method ((i (pattern (type class-pattern.super))))
          )

        (:method ((i (pattern b)))
          (declare (ignore b))))))

(test christophe.1

  (with-pattern-generic-function (christophe.1 (a)
      ;; (:method ((a (pattern (cons x 1)))))
      (:method ((a (pattern (cons 0 y))))
        (declare (ignore y)))
      (:method ((a (pattern (cons 0 (type real))))))
      (:method ((a (pattern (cons 0 (type integer))))))

      (:method ((a (pattern (cons (type integer) (type integer))))))
      (:method ((a (pattern (cons 3 4)))))
      (:method ((a (pattern (cons 3 5)))))

      (:method ((a (pattern (type array)))))
      (:method ((a (pattern (type simple-array)))))
      (:method ((a (pattern (type string)))))
      (:method ((a (pattern (type simple-string))))))

    (christophe.1 (cons 0 1))))


(test christophe.2
  "Test for counterexample for original topological sorting algorithm
   suggested by Christophe Rhodes."

  (with-pattern-generic-function (christophe.2 (a)
      (:method-combination list)
      (:method list ((a (pattern (cons x 1))))
        (list :x-1 :a a :x x))
      (:method list ((a (pattern (cons 0 y))))
        (list :0-y :a a :y y))
      (:method list ((a (pattern (cons 1 y))))
        (list :1-y :a a :y y))
      (:method list ((a (pattern (cons x y))))
        (list :x-y :a a :x x :y y))

      (:method list ((a (pattern (type array)))))
      (:method list ((a (pattern (type simple-array)))))
      (:method list ((a (pattern (type string)))))
      (:method list ((a (pattern (type simple-string)))))
      (:method list ((a (pattern (type simple-base-string))))))

    (christophe.2 (cons 0 1))))

(test christophe.3

  (with-pattern-generic-function (christophe.3 (a)
      (:method-combination list)
      (:method list ((a (pattern (cons x y))))
        (declare (ignore x y)))

      (:method list ((a (pattern (cons x (type integer)))))
        (declare (ignore x)))
      (:method list ((a (pattern (cons x 1))))
        (declare (ignore x)))

      (:method list ((a (pattern (cons (type integer) y))))
        (declare (ignore y)))
      (:method list ((a (pattern (cons 0 y))))
        (declare (ignore y)))

      (:method list ((a (pattern (cons (type integer) (type symbol))))))

      ;; "unmoralizing" nodes
      ;; (:method list ((a (pattern (cons 0 1)))))
      ;; (:method list ((a (pattern (cons 0 (type integer))))))
      ;; (:method list ((a (pattern (cons 0 (type symbol))))))
      )

    (christophe.3 (cons 0 1))))
