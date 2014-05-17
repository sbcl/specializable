;;;; pcl-patch.lisp --- Hot-patch for SBCL's PCL variant.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:sb-pcl)

;;; `make-method-lambda-using-specializers'

(export '(make-method-lambda-using-specializers))

(defgeneric make-method-lambda-using-specializers (gf method qualifiers specializers method-lambda env)
  (:method ((gf standard-generic-function) (method standard-method) qualifiers specializers method-lambda env)
    (declare (type (cons (eql lambda) (cons list)) method-lambda))
    ;; Default behavior: delegate to MAKE-METHOD-LAMBDA.
    (let* ((lambda-list (second method-lambda))
           (*method-lambda-list* (append
                                  (mapcar #'list (subseq lambda-list 0 (length specializers)) specializers)
                                  (subseq lambda-list (length specializers)))))
      (make-method-lambda gf method method-lambda env)))
  #+sb-doc
  (:documentation
   "TODO
return three values:
1. the method lambda
2. initargs for the method instance
3. a (possibly modified) method lambda-list or nil"))

(defun expand-defmethod (name
                         proto-gf
                         proto-method
                         qualifiers
                         lambda-list
                         body
                         env)
  (multiple-value-bind (parameters unspecialized-lambda-list specializers)
      (parse-specialized-lambda-list lambda-list)
    (declare (ignore parameters))
    (let ((*method-name* `(,name ,@qualifiers ,specializers))
          (method-lambda `(lambda ,unspecialized-lambda-list ,@body)))
      (multiple-value-bind (method-function-lambda initargs new-lambda-list)
          (make-method-lambda-using-specializers
           proto-gf proto-method qualifiers specializers method-lambda env)
        (let ((initargs-form (make-method-initargs-form
                              proto-gf proto-method method-function-lambda
                              initargs env))
              (specializers-form (make-method-specializers-form
                                  proto-gf proto-method specializers env)))
          `(progn
             ;; Note: We could DECLAIM the ftype of the generic function
             ;; here, since ANSI specifies that we create it if it does
             ;; not exist. However, I chose not to, because I think it's
             ;; more useful to support a style of programming where every
             ;; generic function has an explicit DEFGENERIC and any typos
             ;; in DEFMETHODs are warned about. Otherwise
             ;;
             ;;   (DEFGENERIC FOO-BAR-BLETCH (X))
             ;;   (DEFMETHOD FOO-BAR-BLETCH ((X HASH-TABLE)) ..)
             ;;   (DEFMETHOD FOO-BRA-BLETCH ((X SIMPLE-VECTOR)) ..)
             ;;   (DEFMETHOD FOO-BAR-BLETCH ((X VECTOR)) ..)
             ;;   (DEFMETHOD FOO-BAR-BLETCH ((X ARRAY)) ..)
             ;;   (DEFMETHOD FOO-BAR-BLETCH ((X LIST)) ..)
             ;;
             ;; compiles without raising an error and runs without
             ;; raising an error (since SIMPLE-VECTOR cases fall through
             ;; to VECTOR) but still doesn't do what was intended. I hate
             ;; that kind of bug (code which silently gives the wrong
             ;; answer), so we don't do a DECLAIM here. -- WHN 20000229
             ,(make-defmethod-form name qualifiers specializers-form
                                   (or new-lambda-list unspecialized-lambda-list)
                                   (if proto-method
                                       (class-name (class-of proto-method))
                                       'standard-method)
                                   initargs-form)))))))

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
     (environment t))
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
     (environment t))
  specializer-name)

(defmethod make-specializer-form-using-class or
    ((proto-generic-function standard-generic-function)
     (proto-method standard-method)
     (specializer-name symbol)
     (environment t))
  `(find-class ',specializer-name))

(defmethod make-specializer-form-using-class or
    ((proto-generic-function standard-generic-function)
     (proto-method standard-method)
     (specializer-name cons)
     (environment t))
  ;; In case of unknown specializer or known specializer with syntax
  ;; error, TYPECASE may fall through to default method with error
  ;; signaling.
  (typecase specializer-name
    ((cons (eql eql) (cons t null))
     `(intern-eql-specializer ,(second specializer-name)))
    ((cons (eql class-eq) (cons t null))
     `(class-eq-specializer (find-class ',(second specializer-name))))))
