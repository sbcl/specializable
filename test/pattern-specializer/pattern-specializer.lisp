;;;; pattern-specializer.lisp --- Tests for pattern specializers.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer.test)

(defmacro with-pattern-generic-function (name lambda-list (&rest options)
                                         &body body)
  `(unwind-protect
        (progn
          (eval-when (:compile-toplevel :load-toplevel :execute)
            (defgeneric ,name ,lambda-list
              (:generic-function-class pattern-generic-function)
              ,@options))
          (locally (declare (sb-ext::muffle-conditions style-warning)) ; TODO
            ,@body))
     (fmakunbound ',name)))

(def-suite :make-method-lambda-using-specializers
    :in :pattern-specializer)
(in-suite :make-method-lambda-using-specializers)

#+no (test duplicate-variables
  (with-pattern-generic-function foo (a b)
      ()
    (eval '(defmethod foo ((a (pattern (cons c d))) (b (pattern (cons c e))))))
    (is (= 1 2))
    (foo (cons 1 2) (cons 3 4))))

#+no (defgeneric name (a b)
  (:generic-function-class pattern-generic-function)
  (:method ((a (pattern (cons c d))) (b (pattern (cons f e))))))

;;; Next method tests

(def-suite :next-method
    :in :pattern-specializer)
(in-suite :next-method)

(optima.core:parse-pattern '(cons b (type symbol)))

;;
;;
;; (cons 1 nil) --.                 (cons b (type symbol)
;;                  (cons b nil) --                       -- (cons b c)
;; (cons 2 nil) --                  (cons b (type list))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric next-method.1 (a)
    (:generic-function-class pattern-generic-function)))

(defmethod next-method.1 ((a (pattern (type integer))))
  (append (list (list 'integer a))
          (when (next-method-p)
            (call-next-method))))

(defmethod next-method.1 ((a (pattern (type real))))
  (append (list (list 'real a))
          (when (next-method-p)
            (call-next-method))))

(defmethod next-method.1 ((a cons))
  (append (list (list 'cons a))
          (when (next-method-p)
            (call-next-method))))

(defmethod next-method.1 ((a (pattern (cons b c))))
  (append (list (list '(cons var var) (list a b c)))
          (when (next-method-p)
            (call-next-method))))

(defmethod next-method.1 ((a (pattern (cons b (type symbol)))))
  (append (list (list '(cons var (type symbol)) (list a b)))
          (when (next-method-p)
            (call-next-method))))

(defmethod next-method.1 ((a (pattern (cons b (type list)))))
  (append (list (list '(cons var (type list)) (list a b)))
          (when (next-method-p)
            (call-next-method))))

(defmethod next-method.1 ((a (pattern (cons b nil))))
  (append (list (list '(cons var nil) (list a b)))
          (when (next-method-p)
            (call-next-method))))

(defmethod next-method.1 ((a (pattern (cons 1 nil))))
  (append (list (list '(cons var nil) (list a)))
          (when (next-method-p)
            (call-next-method))))

(defmethod next-method.1 ((a (pattern (cons 2 nil))))
  (append (list (list '(cons var nil) (list a)))
          (when (next-method-p)
            (call-next-method))))

(next-method.1 1)
(next-method.1 (cons 3 2))
(next-method.1 (cons 1 'a))

;;;

(defstruct class-pattern.super
  a)
(defstruct (class-pattern.sub (:include class-pattern.super))
  b)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric class-pattern (a)
    (:generic-function-class pattern-generic-function)))

(defmethod class-pattern ((a (pattern 1)))
  )

(defmethod class-pattern ((a (pattern (type number))))
  )

(defmethod class-pattern ((a (pattern (type integer))))
  )

(defmethod class-pattern ((a (pattern (type float))))
  )

;; same as next specializer under current rules
#+no(defmethod class-pattern ((a (pattern (type class-pattern.super))))
  )

(defmethod class-pattern ((a (pattern (class-pattern.super))))
  )

(defmethod class-pattern ((a (pattern (class-pattern.sub))))
  )

(defmethod class-pattern ((a (pattern (class-pattern.sub a))))
  )

(defmethod class-pattern ((a (pattern (class-pattern.sub b))))
  )

(defmethod class-pattern ((a (pattern (class-pattern.sub (b (type real))))))
  )

(defmethod class-pattern ((a (pattern (class-pattern.sub (b (type integer))))))
  )

(defmethod class-pattern ((a (pattern (class-pattern.sub a b))))
  )

(defmethod class-pattern ((a (pattern (type class-pattern.super))))
  )

(defmethod class-pattern ((a (pattern b)))
  )

(pattern-more-specific-p
 (optima.core:parse-pattern '(class-pattern.sub (b (type real))))
 (optima.core:parse-pattern '(class-pattern.sub (b (type integer)))))

(pattern-more-specific-p
 (optima.core:parse-pattern '(cons a b))
 (optima.core:parse-pattern '(type cons)))

(pattern-more-specific-p
 (optima.core:parse-pattern '(cons nil nil))
 (optima.core:parse-pattern '(type (cons null null))))
