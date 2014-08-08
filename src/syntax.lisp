;;;; syntax.lisp --- Simple syntax for extended specializers.
;;;;
;;;; Copyright (C) 2014, 2015 Christophe Rhodes, Jan Moringen
;;;;
;;;; Author: Christophe Rhodes <csr21@cantab.net>
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package "SPECIALIZABLE")

(defun extended-specializer-name-p (name)
  (and (symbolp name) (get name 'extended-specializer-syntax)))

(deftype extended-specializer-name ()
  `(satisfies extended-specializer-name-p))

(defun no-unparser (generic-function specializer)
  (declare (ignore generic-function))
  (error "~@<There is no unparser for specializer ~A~@:>"
         specializer))

(defun no-printer (stream specializer)
  (declare (ignore stream))
  (error "~@<There is no printer for specializer ~A~@:>"
         specializer))

(defstruct (extended-specializer-syntax
             (:constructor make-extended-specializer-syntax
                           (parser &optional unparser printer))
             (:copier nil))
  (parser   (sb-int:missing-arg) :type function :read-only t)
  (unparser #'no-unparser        :type function :read-only t)
  (printer  #'no-printer         :type function :read-only t))

(defun find-extended-specializer-syntax (name &optional (errorp t))
  (declare (type symbol name))
  (or (get name 'extended-specializer-syntax)
      (when errorp
        (error "~@<~S has not been declared as an extended specializer ~
               name.~@:>"
               name))))

(defun (setf find-extended-specializer-syntax) (new-value name)
  (declare (type (or null extended-specializer-syntax) new-value)
           (type symbol name))
  (if new-value
      (setf (get name 'extended-specializer-syntax) new-value)
      (remprop name 'extended-specializer-syntax)))

;; Not part of public protocol.
(defgeneric specializer-syntax-name (specializer))

(defmacro define-extended-specializer-syntax (name &body body)
  (unless (typep name '(and symbol (not null)))
    (error "~@<~S is not a valid specializer syntax name.~@:>"
           name))
  (flet ((duplicate (name)
           (error "~@<The ~S option cannot be supplied more than
                  once.~@:>"
                  name))
         (missing (name)
           (error "~@<The ~S option has to be supplied at least ~
                   once.~@:>"
                  name))
         (make-method (class)
           `(defmethod specializer-syntax-name ((specializer ,class))
              ',name))
         (make-parser (spec)
           (destructuring-bind ((generic-function-var &rest args)
                                &body body)
               spec
             `(lambda (,generic-function-var ,@args)
                ,@body)))
         (make-unparser (spec)
           (destructuring-bind ((generic-function-var specializer-var)
                                &body body)
               spec
             `(lambda (,generic-function-var ,specializer-var)
                ,@body)))
         (make-printer (spec)
           (destructuring-bind ((stream-var specializer-var) &body body)
               spec
             `(lambda (,stream-var ,specializer-var)
                ,@body))))
    (let ((classes)
          (parser)
          (unparser)
          (printer))
      (dolist (option body)
        (typecase option
          ((cons (eql :class))
           (push (second option) classes))
          ((cons (eql :parser) cons)
           (when parser (duplicate :parser))
           (setf parser (rest option)))
          ((cons (eql :unparser) cons)
           (when unparser (duplicate :unparser))
           (setf unparser (rest option)))
          ((cons (eql :printer) cons)
           (when printer (duplicate :printer))
           (setf printer (rest option)))
          (t
           (error "~@<Invalid option: ~S~@:>" option))))
      (unless classes (missing :class))
      (unless parser (missing :parser))
      `(progn
         ,@(mapcar #'make-method classes)
         (setf (find-extended-specializer-syntax ',name)
               (make-extended-specializer-syntax
                ,(make-parser parser)
                ,@(when unparser `(,(make-unparser unparser)))
                ,@(when printer `(,(make-printer printer)))))))))

(defun make-extended-specializer (generic-function specializer-specifier)
  (declare (type (cons symbol) specializer-specifier))
  (destructuring-bind (name &rest args) specializer-specifier
    (apply (extended-specializer-syntax-parser
            (find-extended-specializer-syntax name))
           generic-function args)))
