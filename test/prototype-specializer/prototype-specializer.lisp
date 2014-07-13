;;;; prototype-specializer.lisp --- Unit tests for the prototype specializer.
;;;;
;;;; Copyright (C) 2013, 2014 Christophe Rhodes
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Christophe Rhodes <csr21@cantab.net>
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:prototype-specializer.test)

;;; Utilities

(defmacro with-prototype-generic-function ((name lambda-list &rest options)
                                           &body body)
  `(with-specializable-generic-function
       (prototype-generic-function ,name ,lambda-list ,@options)
     ,@body))

;; Test suite

(in-suite :specializable.prototype-specializer)

(test print.smoke

  (is (search ">/ROOT/" (princ-to-string /root/)))
  (is (search "[" (princ-to-string (clone /root/)))))

(test clone.smoke

  (is (typep (clone /root/) 'prototype-object))
  (is (typep (clone (clone /root/)) 'prototype-object)))

(test delegation.smoke

  (flet ((delegations (object)
           (let ((delegations '()))
             (map-delegations (lambda (delegation)
                                (push delegation delegations))
                              object)
             delegations)))
    (let ((object (clone /root/)))
      (is (set-equal (list object) (delegations object)))
      (signals error ; TODO specific condition
        (add-delegation object object))

      (add-delegation object /root/)
      (is (set-equal (list object /root/) (delegations object)))
      (signals error ; TODO specific condition
        (add-delegation object object))

      (remove-delegation object)
      (is (set-equal (list object) (delegations object))))))

(test defmethod.specializer-instance
  "Test splicing a PROTOTYPE-OBJECT instance into a DEFMETHOD form."
  ;; (as opposed to a symbol naming such an instance in the lexical
  ;; environment)
  (let ((object (clone /root/)))
    (with-prototype-generic-function
        (foo (bar))
      (eval `(defmethod foo ((bar ,object)) :object))
      (is (eq :object (foo object))))))

(test remove-method.smoke

  (let ((object (clone /root/)))
    (with-prototype-generic-function
        (foo (bar)
         (:let object object)
         (:method ((bar /root/)))
         (:method ((bar object)))
         #+TODO-not-possible (:method ((bar integer)))
         #+TODO-broken (:method ((bar (eql 5)))))

      (dolist (method (sb-mop:generic-function-methods #'foo))
        (finishes (remove-method #'foo method)))
      (is (emptyp (sb-mop:generic-function-methods #'foo))))))

(test call-method.smoke

  (let ((object (clone /root/)))
    (with-prototype-generic-function
        (foo (bar)
         (:let object object)
         (:method ((bar /root/))
           :root)
         (:method ((bar object))
           :object)
         #+TODO-broken (:method ((bar (eql 5)))
           5))

      (signals error #+later no-applicable-method-error
        (foo :no-such-method))
      (is (eq :root (foo /root/)))
      (is (eq :object (foo object)))
      #+TODO-broken (is (eq 5 (foo 5)))

      ;; Cloning should inherit roles.
      (let ((object2 (clone object)))
        (is (eq :object (foo object2)))))))
