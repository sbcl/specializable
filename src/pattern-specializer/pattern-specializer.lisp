;;;; pattern-specializer.lisp --- Implementation of pattern specializers.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer)

;;; `pattern-generalizer' class

(defstruct (pattern-generalizer
            (:constructor make-pattern-generalizer (specializers key variables &optional next))
            (:copier nil))
  (specializers nil :type list :read-only t)
  (key          nil :type t    :read-only t)
  (variables    nil :type list :read-only t)
  (next         nil :type t)) ; TODO can this be read-only?

(defmethod specializable:generalizer-equal-hash-key
    ((generic-function specializable:specializable-generic-function)
     (generalizer pattern-generalizer))
  (let ((key (pattern-generalizer-key generalizer)))
    (if-let ((next (pattern-generalizer-next generalizer))) ; TODO compute lazily?
      (cons key (specializable:generalizer-equal-hash-key
                 generic-function next))
      key)))

;;; `pattern-specializer' class

(defclass pattern-specializer (specializable:extended-specializer)
  ((pattern            :initarg  :pattern
                       :reader   specializer-pattern)
   (accepts-p-function))
  (:default-initargs
   :pattern (required-argument :pattern)))

(defmethod print-object ((object pattern-specializer) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (specializer-pattern object) stream)))

(defun specializer-parsed-pattern (specializer)
  (optima.core:parse-pattern (specializer-pattern specializer)))

(defun specializer-pattern-variables (specializer)
  (optima.core:pattern-variables (specializer-parsed-pattern specializer)))

(specializable:define-extended-specializer pattern (generic-function pattern)
  (declare (ignore generic-function))
  (make-instance 'pattern-specializer :pattern pattern))

;; Parsing is handled by `define-extended-specializer' above

(defmethod unparse-specializer-using-class
    ((gf specializable:specializable-generic-function) (specializer pattern-specializer))
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

(defmethod specializable:specializer< ((generic-function specializable:specializable-generic-function)
                                       (specializer1 pattern-specializer)
                                       (specializer2 pattern-specializer)
                                       (generalizer t))
  (pattern-more-specific-p
   (specializer-parsed-pattern specializer1)
   (specializer-parsed-pattern specializer2)))

;; TODO necessary?
(defmethod specializable:specializer< ((generic-function specializable:specializable-generic-function)
                                       (specializer1 t)
                                       (specializer2 pattern-specializer)
                                       (generalizer t))
  '/=)

;; TODO necessary?
(defmethod specializable:specializer< ((generic-function specializable:specializable-generic-function)
                                       (specializer1 pattern-specializer)
                                       (specializer2 t)
                                       (generalizer t))
  '/=)

(defmethod specializable:specializer< ((generic-function specializable:specializable-generic-function)
                                       (specializer1 class)
                                       (specializer2 pattern-specializer)
                                       (generalizer t))
  (multiple-value-bind (result definitivep)
      (specializable:specializer-accepts-generalizer-p
       generic-function specializer2 specializer1)
    (cond
      ((and result definitivep) '<)
      (result                   '>)
      (t                        '/=))))

;; TODO can this be avoided? does it make sense?
(defmethod specializable:specializer< ((generic-function specializable:specializable-generic-function)
                                       (specializer1 class)
                                       (specializer2 class)
                                       (generalizer pattern-generalizer))
  (let ((next (pattern-generalizer-next generalizer)))
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
             (t                     '/=))))))))

;;; Accepting objects and generalizers

(defmethod specializer-accepts-p-function ((specializer pattern-specializer))
  (if (slot-boundp specializer 'accepts-p-function)
      (slot-value specializer 'accepts-p-function)
      (setf (slot-value specializer 'accepts-p-function)
            (make-checking-function
             (specializer-parsed-pattern specializer)))))

(defmethod specializable:specializer-accepts-p ((specializer pattern-specializer) object)
  (funcall (sb-ext:truly-the function (specializer-accepts-p-function specializer))
           object))

(defmethod specializable:specializer-accepts-generalizer-p ((gf specializable:specializable-generic-function)
                                                            (specializer pattern-specializer)
                                                            (generalizer pattern-generalizer))
  (values (find specializer (pattern-generalizer-specializers generalizer)) t))

(defmethod specializable:specializer-accepts-generalizer-p ((gf specializable:specializable-generic-function)
                                                            (specializer t)
                                                            (generalizer pattern-generalizer))
  (if-let ((next (pattern-generalizer-next generalizer))) ; TODO needed?
    (specializable:specializer-accepts-generalizer-p gf specializer next)
    (values nil t)))

(defmethod specializable:specializer-accepts-generalizer-p ((gf specializable:specializable-generic-function)
                                                            (specializer pattern-specializer)
                                                            (generalizer t))
  (specializer-accepts-generalizer-p-using-pattern
   gf specializer (specializer-parsed-pattern specializer) generalizer))

;; TODO we can do this with `pattern-more-specific-p' by turning class
;; and eql specializers into patterns
(defmethod specializer-accepts-generalizer-p-using-pattern
    ((gf specializable:specializable-generic-function)
     (specializer pattern-specializer)
     (pattern optima.core:variable-pattern)
     (generalizer t))
  (values t t))

(defmethod specializer-accepts-generalizer-p-using-pattern
    ((gf specializable:specializable-generic-function)
     (specializer pattern-specializer)
     (pattern optima.core:and-pattern)
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
             (optima.core:complex-pattern-subpatterns pattern)))
     definitivep)))

(defmethod specializer-accepts-generalizer-p-using-pattern
    ((gf specializable:specializable-generic-function)
     (specializer pattern-specializer)
     (pattern optima.core:or-pattern)
     (generalizer t))
  (error "not implemented"))

(defmethod specializer-accepts-generalizer-p-using-pattern
    ((gf specializable:specializable-generic-function)
     (specializer pattern-specializer)
     (pattern optima.core:not-pattern)
     (generalizer t))
  (multiple-value-bind (result definitivep)
      (specializer-accepts-generalizer-p-using-pattern
       gf specializer (optima.core:not-pattern-subpattern pattern) generalizer)
    (values (not result) definitivep)))

(defmethod specializer-accepts-generalizer-p-using-pattern
    ((gf specializable:specializable-generic-function)
     (specializer pattern-specializer)
     (pattern optima.core:cons-pattern)
     (generalizer t))
  (multiple-value-bind (result definitivep) (subtypep generalizer 'cons)
    (if result
        (values t (and definitivep (subpatterns-unrestricted-p pattern)))
        (values nil definitivep))))

(defmethod specializer-accepts-generalizer-p-using-pattern
    ((gf specializable:specializable-generic-function)
     (specializer pattern-specializer)
     (pattern optima.core:class-pattern)
     (generalizer t))
  (multiple-value-bind (result definitivep)
      (specializable:specializer-accepts-generalizer-p
       gf (find-class (optima.core:class-pattern-class-name pattern)) generalizer)
    (if result
        (values t (and definitivep (subpatterns-unrestricted-p pattern)))
        (values nil definitivep))))

(defmethod specializer-accepts-generalizer-p-using-pattern
    ((gf specializable:specializable-generic-function)
     (specializer pattern-specializer)
     (pattern optima.core:guard-pattern)
     (generalizer t))
  (values t nil)) ; TODO

;; TODO why did i need this again?
(defmethod class-name ((class (eql (find-class 'pattern-specializer))))
  'pattern-specializer)
;; at least this one is for slime
(defmethod class-name ((class pattern-specializer))
  'pattern-specializer)

;;; pattern-method

;; Forward definition. Actual definition is below.
(defclass pattern-generic-function (specializable:specializable-generic-function)
  ()
  (:metaclass funcallable-standard-class))

(defclass pattern-method (standard-method)
  ())

(defmethod method-pattern-specializers ((gf pattern-generic-function)
                                        (method pattern-method))
  (remove-if-not (of-type 'pattern-specializer)
                 (mapcar (curry #'parse-specializer-using-class gf) ; TODO necessary?
                         (method-specializers method))))

(defmethod make-method-lambda-using-specializers
    ((gf pattern-generic-function) (method pattern-method) qualifiers specializers
     lambda-expression environment)

  ;; This transforms LAMBDA-EXPRESSION of the form
  ;;
  ;;   (lambda (arg1 arg2 …) BODY)
  ;;
  ;; into
  ;;
  ;;   (lambda (arg1 arg2 …
  ;;            &key
  ;;            ((:PATTERN-VAR1 PATTERN-VAR1)) ((:PATTERN-VAR2 PATTERN-VAR2)) …
  ;;            &allow-other-keys)
  ;;     BODY)
  ;;
  ;; where BODY contains uses of PATTERN-VAR1, PATTERN-VAR2, …
  (destructuring-bind (operator lambda-list &body body) lambda-expression
    (declare (ignore operator))
    (multiple-value-bind (required optional rest keyword allow-other-keys-p)
        (parse-ordinary-lambda-list lambda-list :normalize nil)
      (flet ((make-keyword-parameter (variable)
               (list `((,(make-keyword variable) ,variable)))))
        (let* ((variables (mappend #'specializer-pattern-variables ; TODO this stuff is repeated in make-method-matching-form
                                   (remove-if-not (of-type 'pattern-specializer)
                                                  (mapcar (curry #'parse-specializer-using-class gf)
                                                          specializers))))
               (new-lambda-list `(,@required
                                  ,@(when optional
                                      `(&optional ,@optional))
                                  ,@(when rest
                                      `(&rest ,rest))
                                  ,@(when (or keyword variables)
                                      `(&key ,@keyword
                                             ,@(mapcan #'make-keyword-parameter variables)))
                                  ,@(when allow-other-keys-p
                                      '(&allow-other-keys))))
               (new-lambda-expression `(lambda ,new-lambda-list ,@body)))
          (call-next-method
           gf method qualifiers specializers new-lambda-expression environment))))))

;;; pattern-generic-function

(defclass pattern-generic-function (specializable:specializable-generic-function)
  ((specializer-clusters :type     list)
   (generalizer-makers   :type     list #|of function|#))
  (:metaclass funcallable-standard-class)
  (:default-initargs
   :method-class (find-class 'pattern-method))) ; TODO is pattern-method even needed?

(defmethod reinitialize-instance :after ((instance pattern-generic-function)
                                         &key)
  (slot-makunbound instance 'specializer-clusters)
  (slot-makunbound instance 'generalizer-makers))

(defmethod generic-function-specializer-clusters ((generic-function pattern-generic-function))
  (if (slot-boundp generic-function 'specializer-clusters) ; TODO ensure-slot-value
      (slot-value generic-function 'specializer-clusters)
      (setf (slot-value generic-function 'specializer-clusters)
            ;; TODO copied from make-generalizer-makers
            (when-let* ((methods (generic-function-methods generic-function))
                        (arity (when-let ((first-method (first methods)))
                                 (length (method-specializers first-method)))) ; TODO improve
                        )
              (loop :for i :below arity
                 :collect (let* ((specializers (mapcar (lambda (method)
                                                         (nth i (method-specializers method)))
                                                       methods))
                                 (non-pattern-specializers
                                  (remove-if (of-type 'pattern-specializer) specializers))
                                 (pattern-specializers
                                  (set-difference specializers non-pattern-specializers)))
                            (specializer-clusters generic-function pattern-specializers)))))))

(defmethod generic-function-generalizer-makers ((generic-function pattern-generic-function))
  (if (slot-boundp generic-function 'generalizer-makers)
      (slot-value generic-function 'generalizer-makers)
      (setf (slot-value generic-function 'generalizer-makers)
            (make-generalizer-makers generic-function))))

(defmethod specializable:compute-effective-arguments-function ((generic-function pattern-generic-function)
                                                               num-required)
  (declare (type fixnum num-required))
  (let ((no-bindings (load-time-value (make-array 0 :element-type t :adjustable nil) t)))
    (flet ((maybe-generalizer-bindings (generalizer) ; TODO can be done before invocation time
             (if (typep generalizer 'pattern-generalizer)
                 (pattern-generalizer-variables generalizer)
                 no-bindings)))
     (cond
       ((= 1 num-required)
        (lambda (args generalizers)
          (declare (type list args generalizers))
          (list* (maybe-generalizer-bindings (first generalizers)) args)))
       (t
        (lambda (args generalizers)
          (declare (type list args generalizers))
          (append (mapcar #'maybe-generalizer-bindings generalizers) args))))))) ; TODO does not work yet

(defmethod specializable:generalizers-of-using-class ((generic-function pattern-generic-function)
                                                      args num-required)
  (declare (type list args)
           (type fixnum num-required)
           (optimize speed))
  (print (let ((nexts))
     (loop
        :for i :of-type fixnum :from 0 :below num-required
        :for maker :in (generic-function-generalizer-makers generic-function)
        :for arg :in args
        :do (pop nexts)
        :collect
        (cond
          ((funcall (the function maker) arg)) ; TODO probably broken since a generalizer is always returned
          ((not nexts) (first (setf nexts (nthcdr i (call-next-method)))))
          (t           (first nexts)))))))

(defmethod specializable:generalizer-of-using-class ((generic-function pattern-generic-function)
                                                     object arg-position)
  (or (funcall (nth arg-position (generic-function-generalizer-makers generic-function)) object)
      (call-next-method)))

;;; Specializer clustering

(defmethod in-same-cluster-p ((generic-function t) (specializer1 t) (specializer2 t))
  nil)

(defmethod in-same-cluster-p ((generic-function specializable:specializable-generic-function)
                              (specializer1 pattern-specializer)
                              (specializer2 pattern-specializer))
  (let ((pattern1 (specializer-parsed-pattern specializer1))
        (pattern2 (specializer-parsed-pattern specializer2)))
    (member (pattern-more-specific-p pattern1 pattern2) '(= < >))))

(defmethod in-same-cluster-p ((generic-function specializable:specializable-generic-function)
                              (specializer1 pattern-specializer)
                              (specializer2 class))
  (specializable:specializer-accepts-generalizer-p
   generic-function specializer1 specializer2))

(defmethod in-same-cluster-p ((generic-function specializable:specializable-generic-function)
                              (specializer2 class)
                              (specializer1 pattern-specializer))
  (specializable:specializer-accepts-generalizer-p
   generic-function specializer1 specializer2))

(defun specializer-clusters (generic-function specializers)
  (let ((clusters '()))
    (dolist (specializer specializers)
      (dolist (cluster clusters (push (list (list specializer)) clusters))
        (when (every (lambda (entry)
                       (in-same-cluster-p
                        generic-function specializer (first entry)))
                     cluster)
          (dolist (entry cluster (nconcf cluster (list (list specializer))))
            (when (sb-pcl::same-specializer-p specializer (first entry))
              (nconcf entry (list specializer))
              (return)))
          (return))))
    (mapcar (lambda (cluster)
              (stable-sort cluster (lambda (entry1 entry2)
                                     (eq '< (specializable:specializer<
                                             generic-function entry1 entry2 :TODO)))
                           :key #'first))
            clusters)))

;;; Generalizers maker

(defun make-generalizer-maker-form (generic-function specializers clusters)
  (labels ((cluster-element-clause (element rest)
             (let* ((specializer (first element))
                    (variables (specializer-pattern-variables specializer)))
               `(,(specializer-pattern specializer)
                  (make-pattern-generalizer
                   '(,@(mappend #'identity (list* element rest)))
                   ',(specializer-pattern specializer)
                   (list ,@(loop :for variable in (remove-if-not #'symbol-package variables) ; TODO hack
                              :collect (make-keyword variable)
                              :collect variable))))))
           (cluster-clauses (cluster)
             (loop :for (element . rest) :on cluster
                :collect (cluster-element-clause element rest))))
    `(lambda (arg)
       (optima:match arg
         ,@(mappend #'cluster-clauses clusters)
         (t ,(make-pattern-generalizer '() nil '()))))))

(defun make-generalizer-maker (generic-function specializers clusters)
  (let* ((non-pattern-specializers
          (remove-if (of-type 'pattern-specializer) specializers))
         (pattern-specializers
          (set-difference specializers non-pattern-specializers)))
    (values (compile nil (make-generalizer-maker-form
                          generic-function pattern-specializers clusters))
            non-pattern-specializers)))

(defun make-generalizer-makers (generic-function)
  (let* ((clusters (generic-function-specializer-clusters generic-function))
         (methods  (generic-function-methods generic-function))
         (arity    (when-let ((first-method (first methods)))
                     (length (method-specializers first-method)))) ; TODO improve
         (any-non-pattern-specializers-p nil))
    (values
     (loop :for i :below arity
        :collect (multiple-value-bind
                       (generalizer-maker non-pattern-specializers-p)
                     (make-generalizer-maker
                      generic-function
                      (mapcar (lambda (method)
                                (nth i (method-specializers method)))
                              methods)
                      (nth i clusters))
                   (when non-pattern-specializers-p
                     (setf any-non-pattern-specializers-p t))
                   generalizer-maker))
     any-non-pattern-specializers-p)))
