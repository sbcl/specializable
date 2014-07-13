;;;; debug.lisp --- Debugging functions for pattern-specializer.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer)

(defvar *debug* nil)

(defmacro defun/debug (name lambda-list &body body)
  (multiple-value-bind (body declarations documentation) (parse-body body)
    `(defun ,name ,lambda-list
       ,@(when documentation `(,documentation))
       ,declarations
       (unless *debug*
         (return-from ,name))
       ,@body)))

(defun remove-debug-forms (form)
  (subst-if nil (lambda (form) (typep form '(cons (eql debug-clause-matching)))) form))

(defun/debug debug-clause (pattern specializers paths variables form)
  (format t "~S clause for~%~
             |  ~S~%~
             |~%~
             ~{| * ~S~%~}~
             |~%~
             ~{~{~&| > [~D] ~:[<unused>        ~:;~:*~16A~]~%~}~}~
             |~%~
             ~<| ~@;~S~:>~%~%"
          'make-generalizer-maker-form (unparse-pattern pattern)
          specializers
          (map 'list #'list
               (iota (length paths))
               (let* ((length (length paths))
                      (specs  (make-list length)))
                 (loop :for (name position) :in variables
                    :do (let ((position (position position paths :key (compose #'path-info-path #'first))))
                          (setf (elt specs position) name)))
                 specs))
          (list (remove-debug-forms form))))

(defun/debug debug-try-match (arg)
  (format t "Trying to match ~S~%" arg))

(defun/debug debug-clause-matching (pattern specializers paths variables bindings)
  (format t "Matching ~S~%~
             |~%~
             | Bindings~%~
             ~{~{~&| > [~D] ~:[<unused>        ~:;~:*~16A~] => ~S~%~}~}~
             |~%~
             | Specializers~%~
             ~{~&| * ~A~%~}"
          (unparse-pattern pattern)
          (map 'list #'list
               (iota (length paths))
               (let* ((length (length paths))
                      (specs  (make-list length)))
                 (loop :for (name position) :in variables
                    :do (let ((position (position position paths :key (compose #'path-info-path #'first))))
                          (setf (elt specs position) name)))
                       specs)
               bindings)
          specializers))

(defun/debug debug-no-match ()
  (format t "No matching clause~%"))

(defun/debug debug-generalizer/match (generalizer)
  (format t "Generalizer from MATCH:~%~
             ~2@T~S~%~%"
          generalizer))

(defun/debug debug-generalizer/next (generalizer)
  (format t "Generalizer from NEXT:~%~
             ~2@T~S~%~%"
          generalizer))

(defun/debug debug-generalizer-maker-form (form)
  (format t "~S:~%  ~S~%"
          'make-generalizer-maker (remove-debug-forms form)))

(defun/debug debug-method-lambda (specializers new-lambda-expression)
  (format t "~S~%~
             | ~S:~%~
             |~%~
             ~<| ~@;~S~:>~%~%"
          'make-method-lambda-using-specializers
          specializers (list new-lambda-expression)))
