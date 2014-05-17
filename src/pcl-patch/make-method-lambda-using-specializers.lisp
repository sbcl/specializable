;;;; make-method-lambda-using-specializers.lisp --- Hot-patch for SBCL's PCL variant.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:sb-pcl)

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
