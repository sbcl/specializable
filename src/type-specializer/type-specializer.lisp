;;;; type-specializer.lisp --- Type specializer and generalizer.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:type-specializer)

;;; `type-generalizer[-with-next]' structures
;;;
;;; When it is known that the "next" generalizer (the result of
;;; calling `class-of' being a typical example) is never used,
;;; `type-generalizer' instances are used.
;;;
;;; Otherwise `type-generalizer-with-next' instances are used and
;;; the `next' slot is initialized by calling the next argument
;;; generalizing function.

(declaim (inline make-type-generalizer))
(defstruct (type-generalizer
             (:constructor make-type-generalizer (specializers key))
             (:copier nil))
  (specializers nil :type list :read-only t)
  (key          nil :type t    :read-only t))

(defmethod specializable:generalizer-equal-hash-key
    ((generic-function specializable:specializable-generic-function)
     (generalizer type-generalizer))
  (type-generalizer-key generalizer))

(declaim (inline make-type-generalizer-with-next))
(defstruct (type-generalizer-with-next
             (:include type-generalizer)
             (:constructor make-type-generalizer-with-next
                           (specializers key next))
             (:copier nil))
  (next (required-argument :next) :type t :read-only t))

(defmethod specializable:generalizer-equal-hash-key
    ((generic-function specializable:specializable-generic-function)
     (generalizer type-generalizer-with-next))
  (cons (type-generalizer-key generalizer)
        (specializable:generalizer-equal-hash-key
         generic-function
         (type-generalizer-with-next-next generalizer))))

;;; `type-specializer' class

(defclass basic-type-specializer (specializable:extended-specializer)
  ((type :initarg  :type
         :reader   specializer-type))
  (:default-initargs
   :type (required-argument :type)))

(defmethod print-object ((object basic-type-specializer) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (specializer-type object) stream)))

(defclass augment-type-specializer (basic-type-specializer)
  ())

(defclass type-specializer (basic-type-specializer)
  ((accepts-p-function :type     function)))

(specializable:define-extended-specializer-syntax type
  (:class type-specializer)
  (:parser (generic-function type)
    (declare (ignore generic-function))
    (make-instance 'type-specializer
                   :type type))
  (:unparser (generic-function specializer)
    (declare (ignore generic-function))
    (specializer-type specializer))
  (:printer (stream specializer)
    (princ `(type ,(specializer-type specializer)) stream)))

;;; Equality and ordering

;; TODO rename to compare-types
(defun compare-types (type1 type2)
  (multiple-value-bind (<-1 definitivep-1)
      (subtypep type1 type2)
    (multiple-value-bind (<-2 definitivep-2)
        (subtypep type2 type1)
      (cond
        ((not (and definitivep-1 definitivep-2))
         '/=)
        ((and <-1 <-2)
         '=)
        (<-1
         '<)
        (<-2
         '>)
        ((multiple-value-bind (subtypep validp)
             (subtypep `(and ,type1 ,type2) nil)
           (and subtypep validp))
         '//)
        (t
         '/=)))))

;; TODO remove?
(defmethod sb-pcl::same-specializer-p ((specializer1 type-specializer)
                                       (specializer2 type-specializer))
  (let ((type1 (specializer-type specializer1))
        (type2 (specializer-type specializer2)))
    (eq (compare-types type1 type2) '=)))

;; TODO should (type SOME-CLASS) be `same-specializer-p' to SOME-CLASS?

(defmethod specializable:specializer<
    ((generic-function specializable:specializable-generic-function)
     (specializer1 type-specializer)
     (specializer2 type-specializer)
     (generalizer t))
  (compare-types (specializer-type specializer1)
                        (specializer-type specializer2)))

(defmethod specializable:specializer<
    ((generic-function specializable:specializable-generic-function)
     (specializer1 sb-pcl:specializer)
     (specializer2 type-specializer)
     (generalizer t))
  (compare-types
   (specializer-type-specifier generic-function (sb-pcl:class-prototype (find-class 'standard-method)) specializer1)
   (specializer-type specializer2)))                                 ; TODO correct?

(defmethod specializable:specializer<
    ((generic-function specializable:specializable-generic-function)
     (specializer1 type-specializer)
     (specializer2 sb-pcl:specializer)
     (generalizer t))
  (multiple-value-bind (result definitivep) ; TODO does not return second value. oops
      (specializable:specializer<
       generic-function specializer2 specializer1 generalizer)
    (values (specializable:invert-specializer<-relation result)
            definitivep)))

;; TODO can this be avoided? does it make sense?
(defmethod specializable:specializer<
    ((generic-function specializable:specializable-generic-function)
     (specializer1 sb-pcl:specializer)
     (specializer2 sb-pcl:specializer)
     (generalizer type-generalizer-with-next))
  (let ((next (type-generalizer-with-next-next generalizer)))
    (cond
      (next
       (specializable:specializer<
        generic-function specializer1 specializer2 next))
      ((and (typep specializer1 'class) (typep specializer2 'class))
       (compare-types specializer1 specializer2))
      (t
       (call-next-method)))))

;;; Accepting objects and generalizers

;; Forward definition. Actual definition is below.
(defclass type-generic-function (specializable:specializable-generic-function)
  ()
  (:metaclass funcallable-standard-class))

(defmethod specializer-type-specifier
    ((proto-generic-function type-generic-function)
     (proto-method standard-method)
     (specializer type-specializer))
  (specializer-type specializer)) ; TODO use parsed type?

(defmethod specializer-accepts-p-function ((specializer type-specializer))
  (if (slot-boundp specializer 'accepts-p-function)
      (slot-value specializer 'accepts-p-function)
      (setf (slot-value specializer 'accepts-p-function)
            (alexandria:of-type (specializer-type specializer))))) ; TODO compile the predicate for performance?

(defmethod specializable:specializer-accepts-p
    ((specializer type-specializer) object)
  (funcall (sb-ext:truly-the function
             (specializer-accepts-p-function specializer))
           object))

(defmethod specializable:specializer-accepts-generalizer-p
    ((gf specializable:specializable-generic-function)
     (specializer type-specializer)
     (generalizer type-generalizer))
  (values (find specializer (type-generalizer-specializers generalizer)) t))

(defmethod specializable:specializer-accepts-generalizer-p
    ((gf specializable:specializable-generic-function)
     (specializer sb-pcl:specializer)
     (generalizer type-generalizer-with-next))
  (specializable:specializer-accepts-generalizer-p
   gf specializer (type-generalizer-with-next-next generalizer)))

(defmethod specializable:specializer-accepts-generalizer-p
    ((gf specializable:specializable-generic-function)
     (specializer type-specializer)
     (generalizer sb-pcl:specializer))
  (values
   (member
    (compare-types
     (specializer-type specializer)
     (specializer-type-specifier gf (sb-pcl:class-prototype (find-class 'standard-method)) generalizer))
    '(> =) :test #'eq)
   t ; TODO
   ))
