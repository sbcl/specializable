;;;; optima-extensions.lisp --- Tests for extensions of the optima library.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer.test)

(def-suite :optima-extensions.pattern-more-specific-p
    :in :pattern-specializer)
(in-suite :optima-extensions.pattern-more-specific-p)

;; TODO add `guard' and `not' patterns

(defun process-case (case)
  (destructuring-bind (pattern1 pattern2 expected) case
    (let* ((pattern1/parsed (optima.core:parse-pattern pattern1))
           (pattern2/parsed (optima.core:parse-pattern pattern2))
           (result (pattern-specializer::pattern-more-specific-p ; TODO export
                    pattern1/parsed pattern2/parsed)))
      (is (eq expected result)
          "~S < ~S => ~S [not ~S]" pattern1 pattern2 result expected))))

(test constant-pattern
  (mapc #'process-case
        '((1 1         =)
          (1 2         /=)
          ("foo" "foo" =)
          ("foo" "bar" /=))))

(test variable-pattern
  (mapc #'process-case
        '((x          x          =)
          (x          y          =)
          (1          y          <)
          ((cons x y) y          <)
          (x          1          >)
          (x          (cons x y) >))))

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
          (1            (and 1 2)    /=)
          ((cons x y)   (and 1 2)    /=)

          ((and x y)    x            =)
          ((and 1 y)    x            <)
          ((and x 1)    x            <)
          ((and 1 2)    x            <)
          ((and x y)    1            >)
          ((and 1 y)    1            =)
          ((and x 1)    1            =)
          ((and 1 1)    1            =)
          ((and 1 2)    1            /=)
          ((and 1 2)    (cons x y)   /=)

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
          ((and 1 2)    (and 1 1)    /=)
          ((and 1 1)    (and 1 y)    =)
          ((and 1 1)    (and x 1)    =)
          ((and 1 1)    (and 1 2)    /=)
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
          ((cons x y) (or 1 2)    /=)

          ((or x y)   x           =)
          ((or 1 y)   x           =)
          ((or x 1)   x           =)
          ((or 1 2)   x           <)
          ((or x y)   1           >)
          ((or 1 y)   1           >)
          ((or x 1)   1           >)
          ((or 1 1)   1           =)
          ((or 1 2)   1           >)
          ((or 1 2)   (cons x y)  /=)

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
        '(((cons 1 1) 1          /=)
          ((cons 1 1) x          <)

          ((cons 1 1) (cons 1 1) =)
          ((cons 1 2) (cons 1 1) /=)
          ((cons 1 x) (cons 1 1) >)
          ((cons 2 1) (cons 1 1) /=)
          ((cons x 1) (cons 1 1) >))))

;;; class-pattern

(defclass foo ()
  ((bar :initarg :bar)
   (baz :initarg :baz)))
(sb-mop:finalize-inheritance (find-class 'foo)) ; TODO optima bug?

(defclass fez (foo) ())
(sb-mop:finalize-inheritance (find-class 'fez)) ; TODO optima bug?

(test class-pattern

  (mapc #'process-case
        '(((class real)       (class real)              =)
          ((class real)       (class string)            /=)
          ((class real)       (class integer)           >)

          ((class integer)    (class real)              <)
          ((class integer)    (class string)            /=)
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
(pattern-specializer::pattern-more-specific-p (optima.core:parse-pattern '(optima:guard op (typep op 'symbol)))
                                              (optima.core:parse-pattern '(optima:guard op (typep op '(member + *)))))

;; this doesn't:
(pattern-more-specific-p (optima.core:parse-pattern '(and op (type symbol)))
                         (optima.core:parse-pattern '(and op (type (member + *)))))
;; should be < but is /=

;; follows from previous case:
(pattern-more-specific-p (optima.core:parse-pattern '(list* (and op (type symbol)) args))
                         (optima.core:parse-pattern '(list* (and op (type (member + *))) (guard args (notany #'consp args)))))


;;; Test

(test pattern-variables-and-paths.smoke

  (mapc
   (lambda (spec)
     (destructuring-bind (pattern expected) spec
       (is (equal expected (pattern-variables-and-paths
                            (optima.core:parse-pattern input))))))

   '(((or a b 1 (not c))
      ((a optima.core:or-pattern 0) (b optima.core:or-pattern 1)))

     ((list* c (cons a b))
      ((c optima.core:cons-pattern 0)
       (a optima.core:cons-pattern 1 optima.core:cons-pattern 0)
       (b optima.core:cons-pattern 1 optima.core:cons-pattern 1)))

     ((pathname name type)
      ((name optima.core:class-pattern name)
       (type optima.core:class-pattern type)))

     ((pathname (optima:guard name (stringp name)) type)
      ((name optima.core:class-pattern optima:guard optima.core:and-pattern 0)
       (name optima.core:class-pattern optima:guard optima.core:and-pattern 1
             optima.core:structure-pattern 0)
       (type optima.core:class-pattern type))))))
