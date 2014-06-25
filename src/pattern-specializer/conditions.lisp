;;;; conditions.lisp --- Conditions signals in the context of pattern specializers.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer)

(define-condition pattern-variable-name-error (simple-error)
  ((specializer :initarg :specializer
                :type    (cons non-negative-integer
                               (cons sb-pcl:specializer null))
                :reader  pattern-variable-name-error-specializer)
   (name        :initarg :name
                :type    symbol
                :reader  pattern-variable-name-error-name))
  (:default-initargs
   :specializer (required-argument :specializer)
   :name        (required-argument :name))
  (:report
   (lambda (condition stream)
     (with-accessors ((name        pattern-variable-name-error-name)
                      (specializer pattern-variable-name-error-specializer)) condition
       (destructuring-bind (argument-position . specializer) specializer
         (format stream "~@<The pattern variable ~S in the specializer~
                         ~@:_~@:_~2@T~S~@:_~@:_~
                         in the ~:R formal parameter position ~
                         clashes~@[ with ~?~].~@:>"
                 name specializer argument-position
                 (simple-condition-format-control   condition)
                 (simple-condition-format-arguments condition)))))))

(defun pattern-variable-name-error (argument-position specializer name
                                    &optional format-control &rest format-arguments)
  (error 'pattern-variable-name-error
         :specializer      (cons argument-position specializer)
         :name             name
         :format-control   format-control
         :format-arguments format-arguments))
