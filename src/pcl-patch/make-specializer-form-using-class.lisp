;;;; make-specializer-form-using-class.lisp --- Hot-patch for SBCL's PCL variant.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:sb-pcl)

;;; `make-specializer-form-using-class'
;;;
;;; To free every new custom generic function class from having to
;;; implement iteration over specializers in
;;; `make-method-specializers-form', we provide a default method
;;;
;;;   make-method-specializers-form standard-g-f standard-method
;;;
;;; which performs this iteration and calls the generic function
;;;
;;;   make-specializer-form-using-class proto-g-f proto-m specializer-names env
;;;
;;; on which custom generic function classes can install methods to
;;; handle their custom specializers. The generic function uses OR
;;; method combination to allow the following idiom:
;;;
;;;   (defmethod make-specializer-form-using-class or
;;;       (proto-generic-function MY-GENERIC-FUNCTION)
;;;       (proto-method standard-method)
;;;       (specializer-name cons)
;;;       (environment t))
;;;     (when (typep specializer-name '(cons (eql MY-SPECIALIZER)))
;;;       MY-SPECIALIZER-FORM))
;;;
;;; The OR method combination lets everything but (my-specializer …)
;;; fall through to the next methods which will, at some point, handle
;;; class and eql specializers and eventually reach an error signaling
;;; method for invalid specializers.

(defmethod make-method-specializers-form
    ((proto-generic-function standard-generic-function)
     (proto-method standard-method)
     (specializer-names t)
     (environment t))
  (flet ((make-parse-form (name)
           (make-specializer-form-using-class
            proto-generic-function proto-method name environment)))
    `(list ,@(mapcar #'make-parse-form specializer-names))))

;; TODO same approach for parse-specializer-using-class?
(export 'make-specializer-form-using-class)

(defgeneric make-specializer-form-using-class (proto-generic-function proto-method specializer-name environment)
  (:method-combination or)
  #+sb-doc
  (:documentation
   "Return a form which, when evaluated in lexical environment
    ENVIRONMENT, parses the specializer SPECIALIZER-NAME and returns
    the appropriate specializer object.

    Both PROTO-GENERIC-FUNCTION and PROTO-METHOD may be
    uninitialized. However their types and prototype can be
    inspected."))

;; Default behavior is signaling an error for not otherwise handled
;; specializers.
(defmethod make-specializer-form-using-class or
    ((proto-generic-function standard-generic-function)
     (proto-method standard-method)
     (specializer-name t)
     environment)
  (error 'simple-reference-error
         :format-control
         "~@<~S is not a valid parameter specializer name.~@:>"
         :format-arguments (list specializer-name)
         :references (list '(:ansi-cl :macro defmethod)
                           '(:ansi-cl :glossary "parameter specializer name"))))

(defmethod make-specializer-form-using-class or
    ((proto-generic-function standard-generic-function)
     (proto-method standard-method)
     (specializer-name specializer)
     environment)
  specializer-name)

(defmethod make-specializer-form-using-class or
    ((proto-generic-function standard-generic-function)
     (proto-method standard-method)
     (specializer-name symbol)
     environment)
  `(find-class ',specializer-name))

(defmethod make-specializer-form-using-class or
    ((proto-generic-function standard-generic-function)
     (proto-method standard-method)
     (specializer-name cons)
     environment)
  ;; In case of unknown specializer or known specializer with syntax
  ;; error, TYPECASE may fall through to default method with error
  ;; signaling.
  (typecase specializer-name
    ((cons (eql eql) (cons t null))
     `(intern-eql-specializer ,(second specializer-name)))
    ((cons (eql class-eq) (cons t null))
     `(class-eq-specializer (find-class ',(second specializer-name))))))
