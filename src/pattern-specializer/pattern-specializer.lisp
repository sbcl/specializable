;;;; pattern-specializer.lisp --- Pattern specializer and generalizer.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer)

;;; `pattern-generalizer[-with-next]' structures
;;;
;;; When it is known that the "next" generalizer (the result of
;;; calling `class-of' being a typical example) is never used,
;;; `pattern-generalizer' instances are used.
;;;
;;; Otherwise `pattern-generalizer-with-next' instances are used and
;;; the `next' slot is initialized by calling the next argument
;;; generalizing function.

(declaim (inline make-pattern-generalizer))
(defstruct (pattern-generalizer
             (:constructor make-pattern-generalizer (specializers key variables))
             (:copier nil))
  (specializers nil :type list         :read-only t)
  (key          nil :type t            :read-only t)
  (variables    nil :type simple-array :read-only t))

(defmethod specializable:generalizer-equal-hash-key
    ((generic-function specializable:specializable-generic-function)
     (generalizer pattern-generalizer))
  (pattern-generalizer-key generalizer))

(declaim (inline make-pattern-generalizer-with-next))
(defstruct (pattern-generalizer-with-next
             (:include pattern-generalizer)
             (:constructor make-pattern-generalizer-with-next
                           (specializers key variables next))
             (:copier nil))
  (next (required-argument :next) :type t :read-only t))

(defmethod specializable:generalizer-equal-hash-key
    ((generic-function specializable:specializable-generic-function)
     (generalizer pattern-generalizer-with-next))
  (cons (pattern-generalizer-key generalizer)
        (specializable:generalizer-equal-hash-key
         generic-function
         (pattern-generalizer-with-next-next generalizer))))

;;; `pattern-specializer' class

(defclass pattern-specializer (specializable:extended-specializer)
  ((pattern        :initarg  :pattern
                   :reader   specializer-pattern)
   (parsed-pattern))
  (:default-initargs
   :pattern (required-argument :pattern)))

(defmethod print-object ((object pattern-specializer) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (specializer-pattern object) stream)))

(defun specializer-parsed-pattern (specializer) ; TODO defmethod?
  (if (slot-boundp specializer 'parsed-pattern)
      (slot-value specializer 'parsed-pattern)
      (setf (slot-value specializer 'parsed-pattern)
            (parse-pattern (specializer-pattern specializer)))))

;; TODO remove? logic conversion does normalization
(defun specializer-normalized-pattern (specializer) ; TODO cache?
  (pattern-specializer.optima-extensions::pattern-normalize
   '(:literal :cnf/strict) (specializer-parsed-pattern specializer)))

(defclass augment-pattern-specializer (pattern-specializer)
  ())

(defclass early-pattern-specializer (pattern-specializer)
  ())

(defclass late-pattern-specializer (pattern-specializer)
  ((accepts-p-function :type     function)))

(defvar *parse-specializer-kind* :late)

(specializable:define-extended-specializer pattern (generic-function pattern)
  (declare (ignore generic-function))
  (make-instance (ecase *parse-specializer-kind*
                   (:late  'late-pattern-specializer)
                   (:early 'early-pattern-specializer))
                 :pattern pattern))

;; Parsing is handled by `define-extended-specializer' above

(defmethod unparse-specializer-using-class
    ((gf specializable:specializable-generic-function)
     (specializer pattern-specializer))
  `(pattern ,(specializer-pattern specializer)))

(defmethod make-specializer-form-using-class or
    ((proto-generic-function specializable:specializable-generic-function) ; TODO should work for all specializable generic functions
     (proto-method specializable:specializable-method)
     (specializer-name cons)
     (environment t))
  (when (typep specializer-name '(cons (eql pattern)))
    `(sb-pcl:parse-specializer-using-class ; TODO packages
      (sb-pcl:class-prototype (find-class ',(type-of proto-generic-function)))
      ',specializer-name)))

;;; Equality and ordering

(defmethod sb-pcl::same-specializer-p ((specializer1 pattern-specializer)
                                       (specializer2 pattern-specializer))
  (let ((pattern1 (specializer-parsed-pattern specializer1))
        (pattern2 (specializer-parsed-pattern specializer2)))
    (eq (pattern-more-specific-p pattern1 pattern2) '=)))

;; TODO should (pattern SOME-CLASS) be `same-specializer-p' to SOME-CLASS?

(defmethod specializable:specializer<
    ((generic-function specializable:specializable-generic-function)
     (specializer1 pattern-specializer)
     (specializer2 pattern-specializer)
     (generalizer t))
  (pattern-more-specific-p (specializer-parsed-pattern specializer1)
                           (specializer-parsed-pattern specializer2)))

;; TODO necessary?
(defmethod specializable:specializer<
    ((generic-function specializable:specializable-generic-function)
     (specializer1 t)
     (specializer2 pattern-specializer)
     (generalizer t))
  '/=)

;; TODO necessary?
(defmethod specializable:specializer<
    ((generic-function specializable:specializable-generic-function)
     (specializer1 pattern-specializer)
     (specializer2 t)
     (generalizer t))
  '/=)

(defmethod specializable:specializer<
    ((generic-function specializable:specializable-generic-function)
     (specializer1 class)
     (specializer2 pattern-specializer)
     (generalizer t))
  (multiple-value-bind (result definitivep)
      (specializable:specializer-accepts-generalizer-p
       generic-function specializer2 specializer1)
    (cond
      ((and result definitivep) '<)
      (result                   '>)
      (t                        '/=)))) ; TODO correct?

;; TODO can this be avoided? does it make sense?
(defmethod specializable:specializer<
    ((generic-function specializable:specializable-generic-function)
     (specializer1 class)
     (specializer2 class)
     (generalizer pattern-generalizer-with-next))
  (let ((next (pattern-generalizer-with-next-next generalizer)))
    (cond
      ((typep next 'class)
       (specializable:specializer< generic-function specializer1 specializer2 next))
      ((multiple-value-bind (result1 definitivep1)
           (subtypep specializer1 specializer2)
         (multiple-value-bind (result2 definitivep2)
             (subtypep specializer2 specializer1)
           (cond
             ((not (and definitivep1 definitivep2)))
             ((and result1 result2) '=)
             (result1               '>)
             (result2               '<)
             (t                     '/=)))))))) ; TODO correct?

;;; Accepting objects and generalizers

;; Forward definition. Actual definition is below.
(defclass pattern-generic-function (specializable:specializable-generic-function)
  ()
  (:metaclass funcallable-standard-class))

(defmethod specializer-type-specifier
    ((proto-generic-function pattern-generic-function)
     (proto-method standard-method)
     (specializer pattern-specializer))
  (pattern-type-specifier (specializer-parsed-pattern specializer)))

(defmethod specializer-accepts-p-function ((specializer late-pattern-specializer))
  (if (slot-boundp specializer 'accepts-p-function)
      (slot-value specializer 'accepts-p-function)
      (setf (slot-value specializer 'accepts-p-function)
            (make-predicate (specializer-parsed-pattern specializer)))))

(defmethod specializable:specializer-accepts-p
    ((specializer late-pattern-specializer) object)
  (funcall (sb-ext:truly-the function
             (specializer-accepts-p-function specializer))
           object))

(defmethod specializable:specializer-accepts-generalizer-p
    ((gf specializable:specializable-generic-function)
     (specializer late-pattern-specializer)
     (generalizer pattern-generalizer))
  (values (find specializer (pattern-generalizer-specializers generalizer)) t))

(defmethod specializable:specializer-accepts-generalizer-p
    ((gf specializable:specializable-generic-function)
     (specializer t)
     (generalizer pattern-generalizer-with-next))
  (specializable:specializer-accepts-generalizer-p
   gf specializer (pattern-generalizer-with-next-next generalizer)))

(defmethod specializable:specializer-accepts-generalizer-p
    ((gf specializable:specializable-generic-function)
     (specializer late-pattern-specializer)
     (generalizer t))
  (specializer-accepts-generalizer-p-using-pattern
   gf specializer (specializer-parsed-pattern specializer) generalizer))

;; TODO we can do this with `pattern-more-specific-p' by turning class
;; and eql specializers into patterns
(defmethod specializer-accepts-generalizer-p-using-pattern
    ((gf specializable:specializable-generic-function)
     (specializer late-pattern-specializer)
     (pattern constant-pattern)
     (generalizer t))
  (specializable:specializer-accepts-generalizer-p
   gf
   (make-instance 'sb-pcl:eql-specializer
                  :object (optima.core:constant-pattern-value pattern))
   generalizer))

(defmethod specializer-accepts-generalizer-p-using-pattern
    ((gf specializable:specializable-generic-function)
     (specializer late-pattern-specializer)
     (pattern variable-pattern)
     (generalizer t))
  (values t t))

(defmethod specializer-accepts-generalizer-p-using-pattern
    ((gf specializable:specializable-generic-function)
     (specializer late-pattern-specializer)
     (pattern optima.core:class-pattern)
     (generalizer t))
  (multiple-value-bind (result definitivep)
      (specializable:specializer-accepts-generalizer-p
       gf (find-class (optima.core:class-pattern-class-name pattern)) generalizer)
    (if result
        (values t (and definitivep (pattern-subpatterns-unrestricted-p pattern)))
        (values nil definitivep))))

(defmethod specializer-accepts-generalizer-p-using-pattern
    ((gf specializable:specializable-generic-function)
     (specializer late-pattern-specializer)
     (pattern optima.core:guard-pattern)
     (generalizer t))
  (values t nil)) ; TODO

(defmethod specializer-accepts-generalizer-p-using-pattern
    ((gf specializable:specializable-generic-function)
     (specializer late-pattern-specializer)
     (pattern optima.core:not-pattern)
     (generalizer t))
  (multiple-value-bind (result definitivep)
      (specializer-accepts-generalizer-p-using-pattern
       gf specializer (optima.core:not-pattern-subpattern pattern) generalizer)
    (values (not result) definitivep)))

(defmethod specializer-accepts-generalizer-p-using-pattern
    ((gf specializable:specializable-generic-function)
     (specializer late-pattern-specializer)
     (pattern and-pattern)
     (generalizer t))
  (let ((definitivep t))
    (values
     (block nil
       (mapc (lambda (subpattern)
               (multiple-value-bind (result definitivep)
                   (specializer-accepts-generalizer-p-using-pattern
                    gf specializer subpattern generalizer)
                 (unless result
                   (setf definitivep t) ; TODO correct?
                   (return nil))
                 (unless definitivep
                   (setf definitivep nil))))
             (pattern-subpatterns pattern)))
     definitivep)))

(defmethod specializer-accepts-generalizer-p-using-pattern
    ((gf specializable:specializable-generic-function)
     (specializer late-pattern-specializer)
     (pattern optima.core:or-pattern)
     (generalizer t))
  (error "not implemented"))

(defmethod specializer-accepts-generalizer-p-using-pattern
    ((gf specializable:specializable-generic-function)
     (specializer late-pattern-specializer)
     (pattern optima.core:cons-pattern)
     (generalizer t))
  (multiple-value-bind (result definitivep) (subtypep generalizer 'cons)
    (if result
        (values t (and definitivep (pattern-subpatterns-unrestricted-p pattern)))
        (values nil definitivep))))
