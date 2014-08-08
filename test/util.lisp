;;;; util.lisp --- Utilities for writing tests.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:specializable-test)

;;; Utilities for defining specializable generic functions

(defmacro define-specializable-generic-function (class name lambda-list
                                                 &rest options)
  (flet ((intercept-option-p (option)
           (member (first option) '(:method :let))))
    (let ((other-options  (remove-if #'intercept-option-p options))
          (method-options (remove :method options :test-not #'eq :key #'first))
          (let-options    (remove :let    options :test-not #'eq :key #'first)))
      (flet ((make-defmethod (option)
               (destructuring-bind (option-name lambda-list &body body) option
                 (declare (ignore option-name))
                 `(defmethod ,name ,lambda-list ,@body)))
             (make-binding (option)
               (destructuring-bind (option-name name value) option
                 (declare (ignore option-name))
                 ``(,',name ,,value))))
        `(progn
           (eval ; Define immediately, event at non-toplevel
            `(defgeneric ,',name ,',lambda-list
               (:generic-function-class ,',class)
               ,@',other-options))
           (eval ; Without `eval', these would get `macroexpand'ed
                 ; before the generic function is defined.
            `(let (,,@(mapcar #'make-binding let-options))
               ,@',(mapcar #'make-defmethod method-options))))))))

(defmacro with-specializable-generic-function ((class name lambda-list
                                                &rest options)
                                               &body body)
  `(unwind-protect
        (progn
          (define-specializable-generic-function
            ,class ,name ,lambda-list ,@options)
          (locally (declare (sb-ext::muffle-conditions style-warning)) ; TODO only undefined-function
            ,@body))
     (fmakunbound ',name)))

;;; Utilities for testing example files

(defun test-example (relative-pathname &optional (system :specializable))
  (let ((pathname (asdf:system-relative-pathname
                   system (merge-pathnames relative-pathname "examples/")))
        (phase))
    (handler-bind
        ((sb-kernel:redefinition-warning #'muffle-warning)
         ((or error warning)
          (lambda (condition)
            (fail "~@<~Aing ~S signaled ~S:~@:_~A~@:>"
                  phase pathname (type-of condition) condition))))
      (setf phase :compile)
      (finishes (compile-file pathname))
      (setf phase :load)
      (finishes (load pathname)))))
