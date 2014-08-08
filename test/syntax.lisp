;;;; syntax.lisp --- Tests for specializer syntax protocol.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:specializable-test)

(in-suite :specializable)

(defclass foo-specializer (extended-specializer)
  ((spec :initarg :spec
         :reader  specializer-spec)))

(defun print-specializer (specializer)
  (let* ((syntax-name (specializable::specializer-syntax-name specializer))
         (printer (specializable::extended-specializer-syntax-printer
                   (specializable::find-extended-specializer-syntax syntax-name))))
    (with-output-to-string (stream)
      (funcall printer stream specializer))))

(test define-extended-specializer-syntax.smoke
  "Smoke test for `define-extended-specializer-syntax'."

  (macrolet ((test-case ((name &body body)
                         (specifier   expected-specializer)
                         (specializer expected-specifier expected-string))
               `(unwind-protect
                     (let ((gf (sb-pcl:class-prototype
                                (find-class 'specializable-generic-function))))
                       ;; Define.
                       (eval '(define-extended-specializer-syntax ,name
                               ,@body))
                       ;; Parse.
                       (funcall ,expected-specializer
                                (sb-pcl:parse-specializer-using-class
                                 gf ,specifier))
                       ;; Unparse.
                       ,(case expected-specifier
                          (error
                           `(signals ,expected-specifier
                              (sb-pcl:unparse-specializer-using-class
                               gf ,specializer)))
                          (t
                           `(funcall ,expected-specifier
                                     (sb-pcl:unparse-specializer-using-class
                                      gf ,specializer))))
                       ;; Print.
                       ,(case expected-string
                          (error
                           `(signals ,expected-string
                              (print-specializer ,specializer)))
                          (t
                           `(is (string= ,expected-string
                                         (print-specializer ,specializer))))))
                  ;; Remove syntax after test.
                  (setf (specializable::find-extended-specializer-syntax ',name) nil)
                  (remove-method
                   #'specializable::specializer-syntax-name
                   (first (compute-applicable-methods
                           #'specializable::specializer-syntax-name
                           (list (make-instance 'foo-specializer))))))))
    ;; Syntax (foo THING) without unparser.
    (test-case
     (foo
      (:class foo-specializer)
      (:parser (gf spec)
        (declare (ignore gf))
        (make-instance 'foo-specializer :spec spec)))
     ('(foo 1)
      (lambda (specializer)
        (is (typep specializer 'foo-specializer))
        (is (equal 1 (specializer-spec specializer)))))
     ((make-instance 'foo-specializer :spec 1)
      error
      error))
    ;; Syntax (foo &rest THINGS) with unparser.
    (test-case
     (foo
      (:class foo-specializer)
      (:parser (gf &rest spec)
        (declare (ignore gf))
        (make-instance 'foo-specializer :spec (reverse spec)))
      (:unparser (gf specializer)
        (declare (ignore gf))
        (reverse (specializer-spec specializer)))
      (:printer (stream specializer)
        (format stream "~{~A~^ ~}" (reverse (specializer-spec specializer)))))
     ('(foo 1 2)
      (lambda (specializer)
        (is (typep specializer 'foo-specializer))
        (is (equal '(2 1) (specializer-spec specializer)))))
     ((make-instance 'foo-specializer :spec '(1 2))
      (lambda (designator)
        (is (equal '(foo 2 1) designator)))
      "2 1"))))

(test define-extended-specializer-syntax.syntax
  "Test syntax checking of `define-extended-specializer-syntax'
   macro."

  (macrolet ((test-case (expected-condition name &body body)
               `(signals ,expected-condition
                  (macroexpand '(define-extended-specializer-syntax ,name
                                 ,@body)))))
    ;; Missing :CLASS option
    (test-case error foo)
    ;; Missing :PARSER option
    (test-case error foo (:class foo))
    ;; Invalid name.
    (test-case error 1
      (:class foo)
      (:parser (gf spec)
        (declare (ignore gf spec))))
    ;; Invalid option.
    (test-case error foo
      (:class foo)
      (:parser (gf spec)
        (declare (ignore gf spec)))
      (:no-such-option))
    ;; Duplicate :PARSER.
    (test-case error foo
      (:class foo)
      (:parser (gf spec)
        (declare (ignore gf spec)))
      (:parser (gf spec)
        (declare (ignore gf spec))))
    ;; Duplicate :UNPARSER.
    (test-case error foo
      (:class foo)
      (:parser (gf spec)
        (declare (ignore gf spec)))
      (:unparser (gf specializer)
        (declare (ignore gf specializer)))
      (:unparser (gf specializer)
        (declare (ignore gf specializer))))
    ;; Duplicate :PRINTER.
    (test-case error foo
      (:class foo)
      (:parser (gf spec)
        (declare (ignore gf spec)))
      (:printer (stream specializer)
        (declare (ignore stream specializer)))
      (:printer (stream specializer)
        (declare (ignore stream specializer))))))
