;;;; debug.lisp --- Debugging functions for type-specializer.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:type-specializer)

(defvar *debug* t)

;;; Utilities

(defmacro defun/debug (name lambda-list &body body)
  (multiple-value-bind (body declarations documentation) (parse-body body)
    `(defun ,name ,lambda-list
       ,@(when documentation `(,documentation))
       ,declarations
       (when (member *debug* '(:compile-time t))
         ,@body)
       ,(first lambda-list))))

(defun remove-debug-forms (form)
  (subst-if nil (lambda (form) (typep form '(cons (eql debug-clause-matching)))) form))

;;; Compile-time

(defun/debug debug-method-lambda (specializers new-lambda-expression)
  (format t "~S~%~
             |~%~
             ~<| ~@;~S~:>~%~
             |~%~
             ~<| ~@;~S~:>~2%"
          'make-method-lambda-using-specializers
          (list specializers)
          (list new-lambda-expression)))

(defun/debug debug-make-generalizer-maker-form
    (components accept-next-a-g-f-p)
  (format t "~S for components~%~
             |~%~
             | ~:[without~:;WITH~] next argument generalizing function~%~
             |~%~
             ~{~@<| ~@;~
               * ~{~S~%~
                   ~{~2@T* ~S~%~}~
                 ~}~
             ~:>~%~}"
          'make-generalizer-maker-form
          accept-next-a-g-f-p
          (mapcar (lambda (component)
                    (list component
                          (specializer-component-specializers component)))
                  components)))

(defun/debug debug-make-generalizer-maker-form/clause
    (form specializers type)
  (format t "~S clause~%~
             |~%~
             | Specializers (most-specific first)~%~
             ~{~<| ~@;~@{~D ~S~}~:>~%~}~
             |~%~
             | Augmented type~%~
             ~<| ~@;~S~:>~%~
             |~%~
             ~<| ~@;~S~:>~%~%"
          'make-generalizer-maker-form
          (mapcar #'list (iota (length specializers)) specializers)
          (list type)
          (list (remove-debug-forms form)))
  form)

(defun/debug debug-generalizer-maker-form (form)
  (format t "~S:~%  ~S~%"
          'make-generalizer-maker (remove-debug-forms form)))

;;; Method-invocation-time

(defun/debug debug-try-match (arg next-a-g-f)
  (format t "Trying to match argument~%~
             |~%~
             ~<| ~@;~S~:>~%~
             |~%~
             | ~:[without~:;WITH~] next argument generalization function~
             ~2%"
          (list arg) next-a-g-f))

(defun/debug debug-clause-matching (type specializers)
  (format t "Matching ~S~%~
             |~%~
             | Specializers~%~
             ~{~&| * ~A~%~}~
             ~%"
          type specializers))

(defun/debug debug-no-match ()
  (format t "NO MATCH and no NEXT~2%"))

(defun/debug debug-generalizer/match (generalizer)
  (when generalizer
    (format t "MATCH generalizer:~%~
               ~@<| ~@;~S~:>~2%"
            generalizer)))

(defun/debug debug-generalizer/next (generalizer)
  (when generalizer
    (format t "NO MATCH; generalizer from NEXT:~%~
               ~@<| ~@;~S~:>~2%"
            generalizer)))
