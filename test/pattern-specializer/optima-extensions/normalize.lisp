;;;; optima-extensions.lisp --- Tests for normalization operation.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer.optima-extensions.test)

(def-suite :pattern-specializer.optima-extensions.normalize
    :in :pattern-specializer.optima-extensions)
(in-suite :pattern-specializer.optima-extensions.normalize)

(test pattern-normalize.smoke

  (mapc
   (make-pattern-transform-test-case-thunk (curry 'pattern-normalize :fancy))

   '((4                                                 4)
     (_                                                 _)
     (x                                                 x)

     ((cons x 1)                                        (cons x 1))
     ((cons 0 y)                                        (cons 0 y))

     ((or (not (cons x 1)) (cons 0 y))                  (and 1 2))


     ((and (and 1 2) (not (and 1 2)))                   (not _))
     ((and 1 (and x (and (or 1 2 (and x 2)) y)))        1)
     ((and 1 2 (or 3 4) (or 5 6))                       (not _))
     ((and 3 (or 4 (not (and 1 (or 5 (not 2))))))       (and 1 2))

     ((or (and 1 2) (not (and 1 2)))                    _)
     ((or 1 2 (and 3 4) (and 5 (or 6 (and 7 (not 8))))) (or 1 2)))))

(test normalize-pattern.behavior-unchanged
  "Try to ensure that normalizing a pattern does not change the set of
   values it matches."

  (mapc
   (lambda (pattern)
     (let* ((parsed     (parse-pattern pattern))
            (normalized (pattern-normalize :fancy (parse-pattern pattern)))
            (matcher
             (flet ((make-clause (parsed-pattern)
                      `(optima:match arg
                         (,(unparse-pattern
                            (pattern-anonymize-variables parsed-pattern))
                          :match))))
               (compile nil `(lambda (arg) ; TODO ignore unused variables
                               (declare (ignorable arg))
                               (values ,(make-clause parsed)
                                       ,(make-clause normalized)))))))
       (for-all ((value (gen-tree1)))
         (multiple-value-bind (result/unchanged result/normalized)
             (funcall matcher value)
           (is (eq result/unchanged result/normalized)
               "~@<~S -> ~S => ~S~@:_~
                   ~S -> ~S => ~S~@:>"
               value (unparse-pattern parsed)     result/unchanged
               value (unparse-pattern normalized) result/normalized)))))

   '(4
     _
     x

     (cons x 1)
     (cons 0 y)
     (cons (and x (not 1)) (not (and 1 (not 2))))

     (and (and 1 2) (not (and 1 2)))
     (and 1 (and x (and (or 1 2 (and x 2)) y)))
     (and 1 2 (or 3 4) (or 5 6))
     (and 3 (or 4 (not (and 1 (or 5 (not 2))))))

     (or (and 1 2) (not (and 1 2)))
     (or 1 2 (and 3 4) (and 5 (or 6 (and 7 (not 8))))))))

;; (let ((p (normalize-pattern (optima.core:parse-pattern '(class class
;;                                                        (bar (or 4 (not (and 1 (or 5 (not 2))))))
;;                                                          (baz (and 6 7)))))))
;;   (pattern-more-specific-p p p))
;;
;; (let ((p (normalize-pattern (optima.core:parse-pattern '(class class
;;                                                          (bar 4)
;;                                                          (baz 6))))))
;;   (print :compare)
;;   (pattern-more-specific-p p p))

;; (pattern-more-specific-p
;;  (make-and-pattern (optima.core:parse-pattern '(cons x 1)))
;;  (make-and-pattern (optima.core:parse-pattern '(cons 0 y))))
;;
;; (pattern-more-specific-p
;;  (make-and-pattern (optima.core:parse-pattern '(cons x 1)))
;;  (make-and-pattern (optima.core:parse-pattern '(cons x 1))))

;; (pattern-more-specific-p
;;  (PATTERN-SPECIALIZER.OPTIMA-EXTENSIONS::NORMALIZE-PATTERN
;;   (optima.core:parse-pattern
;;    '(OR (AND (CONS 3 OPTIMA::_) (CONS OPTIMA::_ 4))
;;         (NOT (AND (CONS 3 OPTIMA::_) (CONS OPTIMA::_ 5))))))
;;  *top-pattern*)
;;
;; (PATTERN-SPECIALIZER.OPTIMA-EXTENSIONS::NORMALIZE-PATTERN
;;  (optima.core:parse-pattern
;;   '(and (AND (CONS 3 OPTIMA::_) (CONS OPTIMA::_ 4))
;;         (AND (CONS 3 OPTIMA::_) (CONS OPTIMA::_ 5)))))
;;
;; (pattern-more-specific-p
;;  (make-not-pattern (optima.core:parse-pattern '(CONS OPTIMA::_ 4)))
;;  (optima.core:parse-pattern '(CONS OPTIMA::_ 5)))
