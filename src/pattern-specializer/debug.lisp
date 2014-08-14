;;;; debug.lisp --- Debugging functions for pattern-specializer.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer)

(defvar *debug* t)

;;; Utilities

(defmacro defun/debug (name lambda-list &body body)
  (multiple-value-bind (body declarations documentation) (parse-body body)
    `(defun ,name ,lambda-list
       ,@(when documentation `(,documentation))
       ,declarations
       (when *debug* ,@body)
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

(defun/debug debug-augment-pattern-for-discriminating-function
    (pattern paths)
  (format t "~S~%~
             |~%~
             ~<| ~@;~S~:>~%~
             |~%~
             ~{~<| ~@;* ~S~%~:>~}~2%"
          'augment-pattern-for-discriminating-function
          (list pattern)
          (mapcar #'list paths)))

(defun/debug debug-make-generalizer-maker-form
    (components binding-slot-infos accept-next-a-g-f-p)
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
    (form specializers binding-slot-infos variables pattern)
  (format t "~S clause~%~
             |~%~
             | Specializers (most-specific first)~%~
             ~{~<| ~@;~@{~D ~S~}~:>~%~}~
             |~%~
             | Augmented pattern~%~
             ~<| ~@;~S~:>~%~
             |~%~
             | Binding slots~%~
             ~{~{~&| ~D -> ~:[<unused>        ~:;~:*~16A~]~@[ [used in ~{~D~^, ~}]~]~%~}~}~
             |~%~
             ~<| ~@;~S~:>~%~%"
          'make-generalizer-maker-form
          (mapcar #'list (iota (length specializers)) specializers)
          (list (unparse-pattern pattern))
          (loop :for i :from 0
             :for info :across binding-slot-infos
             :collect (let* ((variable (find-if (lambda (variable)
                                                 (binding-slot-info-find-path
                                                  info (second variable)))
                                               variables))
                             (binding-specializers
                              (reduce #'append (binding-slot-info-paths info)
                                      :key #'path-info-specializers))
                             (specializer-indices
                              (remove nil (mapcar (lambda (specializer)
                                                    (position specializer specializers))
                                                  binding-specializers))))
                        (list i (first variable) specializer-indices)))
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

(defun/debug debug-clause-matching (pattern specializers paths variables bindings)
  (format t "Matching ~S~%~
             |~%~
             | Bindings~%~
             ~{~{~&| > [~D] ~:[<unused>        ~:;~:*~16A~] => ~S~%~}~}~
             |~%~
             | Specializers~%~
             ~{~&| * ~A~%~}~
             ~%"
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
