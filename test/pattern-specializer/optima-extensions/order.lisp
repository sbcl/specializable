;;;; order.lisp --- Tests for ordering of pattern w.r.t. specificity.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer.optima-extensions.test)

(def-suite :pattern-specializer.optima-extensions.pattern-more-specific-p
    :in :pattern-specializer.optima-extensions)
(in-suite :pattern-specializer.optima-extensions.pattern-more-specific-p)

;;; Classes for class patterns

(defclass foo ()
  ((bar :initarg :bar)
   (baz :initarg :baz)))
(sb-mop:finalize-inheritance (find-class 'foo)) ; TODO optima bug?

(defclass fez (foo) ())
(sb-mop:finalize-inheritance (find-class 'fez)) ; TODO optima bug?

;;; Actual tests

;; TODO add `guard' and `not' patterns

(defun process-case (case) ; TODO use existing helpers
  (destructuring-bind (pattern1 pattern2 expected) case
    (let* ((pattern1/parsed (parse-pattern pattern1))
           (pattern2/parsed (parse-pattern pattern2))
           (result (pattern-more-specific-p pattern1/parsed pattern2/parsed)))
      (is (eq expected result)
          "~S < ~S => ~S [not ~S]" pattern1 pattern2 result expected))))

(test constant-pattern
  (mapc #'process-case
        '((1        1        =)
          (1        2        //)
          ("foo"    "foo"    =)
          ("foo"    "bar"    //)
          ('(1 . 2) '(1 . 2) =)
          ('(1 . 2) '(1 . 3) //))))

(test variable-pattern
  (mapc #'process-case
        '((x          x          =)
          (x          y          =)
          (1          y          <)
          ((cons x y) y          <)
          (x          1          >)
          (x          (cons x y) >))))

(test guard-pattern
  (mapc #'process-case
        '(;;
          ((guard x (evenp x))           (guard x (evenp x))                 =)
          ((guard x (evenp x))           (guard y (evenp y))                 =)
          ((guard x (evenp x))           (guard x (oddp x))                  /=)
          ((guard x (evenp x))           (guard y (oddp y))                  /=)

          #+TODO ((guard (and x 1) (oddp x))    (guard (and 1 x) (oddp x))          =)

          ;; type
          ((type integer)                (type string)                       /=)

          ;; constant


          ;; variable

          ;; cons
          ((cons nil 1)                  (guard it (consp it))               <)
          ((cons nil nil)                (type (cons null null))             =)
          ((cons 1 1)                    (type (cons null null))             //)
          ((cons 1 1)                    (type cons)                         <)
          ((cons a 1)                    (type cons)                         <)
          ((cons 1 b)                    (type cons)                         <)
          ((cons a b)                    (type cons)                         =)
          ((cons a (cons b c))           (type (cons t cons))                =)

          ((guard it (consp it))         (cons nil 1)                        >)
          ((type (cons null null))       (cons nil nil)                      =)
          ((type (cons null null))       (cons 1 1)                          //)
          ((type cons)                   (cons 1 1)                          >)
          ((type cons)                   (cons a 1)                          >)
          ((type cons)                   (cons 1 b)                          >)
          ((type cons)                   (cons a b)                          =)
          ((cons a (cons b c))           (type (cons t cons))                =)

          ;;
          ((cons nil 1)                  (type array)                        //)
          ((cons nil 1)                  (guard it (arrayp it))              //)
          ((cons nil 1)                  (type vector)                       //)
          ((cons nil 1)                  (guard it (vectorp it))             //)
          ((cons nil 1)                  (type string)                       //)
          ((cons nil 1)                  (guard it (stringp it))             //)
          ((type string)                 (type array)                        <)
          ((guard it (stringp it))       (guard it (arrayp it))              <)
          ((type string)                 (type vector)                       <)
          ((guard it (stringp it))       (guard it (vectorp it))             <)
          ((type string)                 (type string)                       =)
          ((guard it (stringp it))       (guard it (stringp it))             =)

          ;; class
          ((type foo)                    (type foo)                          =)
          ((type foo)                    (class foo)                         =)
          ((type foo)                    (type fez)                          >)
          ((type foo)                    (class fez)                         >)
          ((type foo)                    (class foo bar)                     >)

          ;; and
          ((and op (type symbol))        (and op (type (member + *)))        >)

          ((list* (and op (type symbol)) args) (list* (and op (type (member + *))) (guard args (notany #'consp args))) >)

          ((guard op (typep op 'symbol)) (guard op (typep op '(member + *))) >))))

(test and-pattern
  ;; Cannot test patterns of the form (and SUB-PATTERN) since optima
  ;; reduces these to just SUB-PATTERN.
  (mapc #'process-case
        '((x            (and x y)    =)
          (x            (and 1 y)    >)
          (x            (and x 1)    >)
          (x            (and 1 2)    >)
          (1            (and x y)    <)
          (1            (and 1 y)    =)
          (1            (and x 1)    =)
          (1            (and 1 1)    =)
          (1            (and 1 2)    >)
          ((cons x y)   (and 1 2)    >)

          ((and x y)    x            =)
          ((and 1 y)    x            <)
          ((and x 1)    x            <)
          ((and 1 2)    x            <)
          ((and x y)    1            >)
          ((and 1 y)    1            =)
          ((and x 1)    1            =)
          ((and 1 1)    1            =)
          ((and 1 2)    1            <)
          ((and 1 2)    (cons x y)   <)

          ((and x y)    (and x y)    =)
          ((and x y)    (and x z)    =)
          ((and x y)    (and 1 y)    >)
          ((and x y)    (and x 1)    >)
          ((and x y)    (and 1 2)    >)
          ((and 1 y)    (and x y)    <)
          ((and x 1)    (and x y)    <)
          ((and 1 2)    (and x y)    <)
          ((and 1 y)    (and 1 1)    =)
          ((and x 1)    (and 1 1)    =)
          ((and 1 2)    (and 1 1)    <)
          ((and 1 1)    (and 1 y)    =)
          ((and 1 1)    (and x 1)    =)
          ((and 1 1)    (and 1 2)    >)
          ((and 1 1)    (and 1 1)    =)

          ((cons 1 2)   (and (cons 1 y) (cons x 2)) =))))

(test or-pattern
  ;; Cannot test patterns of the form (or SUB-PATTERN) since optima
  ;; reduces these to just SUB-PATTERN.
  (mapc #'process-case
        '((x          (or x y)    =)
          (x          (or 1 y)    =)
          (x          (or x 1)    =)
          (x          (or 1 2)    >)
          (1          (or x y)    <)
          (1          (or 1 y)    <)
          (1          (or x 1)    <)
          (1          (or 1 1)    =)
          (1          (or 1 2)    <)
          ((cons x y) (or 1 2)    //)

          ((or x y)   x           =)
          ((or 1 y)   x           =)
          ((or x 1)   x           =)
          ((or 1 2)   x           <)
          ((or x y)   1           >)
          ((or 1 y)   1           >)
          ((or x 1)   1           >)
          ((or 1 1)   1           =)
          ((or 1 2)   1           >)
          ((or 1 2)   (cons x y)  //)

          ((or x y)   (or x y)    =)
          ((or x y)   (or x z)    =)
          ((or x y)   (or 1 y)    =)
          ((or x y)   (or x 1)    =)
          ((or x y)   (or 1 2)    >)
          ((or 1 y)   (or x y)    =)
          ((or x 1)   (or x y)    =)
          ((or 1 2)   (or x y)    <)
          ((or 1 y)   (or 1 1)    >)
          ((or x 1)   (or 1 1)    >)
          ((or 1 2)   (or 1 1)    >)
          ((or 1 1)   (or 1 y)    <)
          ((or 1 1)   (or x 1)    <)
          ((or 1 1)   (or 1 2)    <)
          ((or 1 1)   (or 1 1)    =)

          ((cons 1 2) (or (cons 1 y) (cons x 2)) <))))

(test cons-pattern

  (mapc #'process-case
        '(((cons 1 1) 1          //)
          ((cons 1 1) x          <)

          ((cons 1 1) (cons 1 1) =)
          ((cons 1 2) (cons 1 1) //)
          ((cons 1 x) (cons 1 1) >)
          ((cons 2 1) (cons 1 1) //)
          ((cons x 1) (cons 1 1) >)

          ('(1 . 2)   (cons 1 2) =)
          ('(1 . 3)   (cons 1 2) //)
          ('(1 . x)   (cons 1 2) //)
          ('(1 . x)   (cons 1 x) <)

          ((cons 1 2) '(1 . 2)   =)
          ((cons 1 2) '(1 . 3)   //)
          ((cons 1 2) '(1 . x)   //)
          ((cons 1 x) '(1 . x)   >))))

;;; class-pattern

(test class-pattern

  (mapc #'process-case
        '(((class real)       (class real)              =)
          ((class real)       (class string)            //)
          ((class real)       (class integer)           >)

          ((class integer)    (class real)              <)
          ((class integer)    (class string)            //)
          ((class integer)    (class integer)           =)

          ((class foo)        (class foo)               =)
          ((class foo)        (class foo :bar x)        >)
          ((class foo)        (class foo :bar 1)        >)
          ((class foo)        (class foo :baz x)        >)
          ((class foo)        (class foo :baz 1)        >)
          ((class foo)        (class foo :bar x :baz 1) >)
          ((class foo)        (class foo :bar 1 :baz 1) >)
          ((class foo)        (class fez)               >)
          ((class foo)        (class fez :bar x)        >)
          ((class foo)        (class fez :bar 1)        >)
          ((class foo)        (class fez :baz x)        >)
          ((class foo)        (class fez :baz 1)        >)
          ((class foo)        (class fez :bar x :baz 1) >)
          ((class foo)        (class fez :bar 1 :baz 1) >)

          ((class foo :bar x) (class foo)               <)
          ((class foo :bar x) (class foo :bar x)        =)
          ((class foo :bar x) (class foo :bar 1)        >)
          ((class foo :bar x) (class foo :baz x)        /=)
          ((class foo :bar x) (class foo :baz 1)        /=)
          ((class foo :bar x) (class foo :bar x :baz 1) >)
          ((class foo :bar x) (class foo :bar 1 :baz 1) >)
          ((class foo :bar x) (class fez)               /=)
          ((class foo :bar x) (class fez :bar x)        >)
          ((class foo :bar x) (class fez :bar 1)        >)
          ((class foo :bar x) (class fez :baz x)        /=)
          ((class foo :bar x) (class fez :baz 1)        /=)
          ((class foo :bar x) (class fez :bar x :baz 1) >)
          ((class foo :bar x) (class fez :bar 1 :baz 1) >)

          ((class foo :bar 1) (class foo)               <)
          ((class foo :bar 1) (class foo :bar x)        <)
          ((class foo :bar 1) (class foo :bar 1)        =)
          ((class foo :bar 1) (class foo :baz x)        /=)
          ((class foo :bar 1) (class foo :baz 1)        /=)
          ((class foo :bar 1) (class foo :bar x :baz 1) /=)
          ((class foo :bar 1) (class foo :bar 1 :baz 1) >)
          ((class foo :bar 1) (class fez)               /=)
          ((class foo :bar 1) (class fez :bar x)        /=)
          ((class foo :bar 1) (class fez :bar 1)        >)
          ((class foo :bar 1) (class fez :baz x)        /=)
          ((class foo :bar 1) (class fez :baz 1)        /=)
          ((class foo :bar 1) (class fez :bar x :baz 1) /=)
          ((class foo :bar 1) (class fez :bar 1 :baz 1) >)

          ((class fez)        (class foo)               <)
          ((class fez)        (class foo :bar x)        /=)
          ((class fez)        (class foo :bar 1)        /=)
          ((class fez)        (class foo :baz x)        /=)
          ((class fez)        (class foo :baz 1)        /=)
          ((class fez)        (class foo :bar x :baz 1) /=)
          ((class fez)        (class foo :bar 1 :baz 1) /=)
          ((class fez)        (class fez)               =)
          ((class fez)        (class fez :bar x)        >)
          ((class fez)        (class fez :bar 1)        >)
          ((class fez)        (class fez :baz x)        >)
          ((class fez)        (class fez :baz 1)        >)
          ((class fez)        (class fez :bar x :baz 1) >)
          ((class fez)        (class fez :bar 1 :baz 1) >)

          ((class foo :bar x) (class foo)               <)

          ((class foo :baz 1) (class foo)               <)

          ((class foo :bar x) (class foo)               <))))

;;; Errors and regressions
;;; TODO move to suitable tests where possible

#+TODO (PATTERN-SPECIALIZER::PATTERN-MORE-SPECIFIC-P
        (AND B (GUARD B (CONSTANTP B)))
        (GUARD B (CONSTANTP B)))
;; produces infinite recursion

;;; Guard pattern stuff

;; this works:
;; (pattern-specializer::pattern-more-specific-p (optima.core:parse-pattern '(optima:guard op (typep op 'symbol)))
;;                                               (optima.core:parse-pattern '(optima:guard op (typep op '(member + *)))))

;; this doesn't:
;; (pattern-more-specific-p (optima.core:parse-pattern '(and op (type symbol)))
;;                          (optima.core:parse-pattern '(and op (type (member + *)))))
;; should be < but is /=

;; follows from previous case:
;; (pattern-more-specific-p (optima.core:parse-pattern '(list* (and op (type symbol)) args))
;;                          (optima.core:parse-pattern '(list* (and op (type (member + *))) (guard args (notany #'consp args)))))
