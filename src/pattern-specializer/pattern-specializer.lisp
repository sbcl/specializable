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
  (specializers nil :type list   :read-only t)
  (key          nil :type t      :read-only t)
  (variables    nil :type vector :read-only t)
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

;;; TODO section title; TODO should not be in optima extensions; actually like some of the above code

(defun emit-make-binding-vector (clusters)
  `(make-array ,(length clusters) :initial-element nil))

(defun emit-initialize-binding-vector (clusters binding-vector-var)
  (loop :for cluster :in clusters :collect
     (let ((index (variable-cluster-index cluster))
           (name  (variable-info-name
                   (first (variable-cluster-variables cluster)))))
       `(setf (aref ,binding-vector-var ,index) ,name))))

;; TODO use `optima:[multiple-value][e]match' to bind variables
;;   necessary since a specific pattern (e.g. (cons 1 x))  may have been selected
;;   but a method body without substructure (e.g. just y) is executed
;; TODO solve this by injecting the variables into the more specific patterns?
;; TODO choose correct variable name for respective method body
(defun emit-bindings (clusters binding-vector-var) ; TODO make-bindings-form
  (flet ((emit-binding (cluster)
           (let ((index (variable-cluster-index cluster))
                 (name  (variable-info-name
                         (first (variable-cluster-variables cluster)))))
             `(,name (aref ,binding-vector-var ,index)))))
    (mapcar #'emit-binding clusters)))

(defmethod make-method-lambda-using-specializers
    ((gf pattern-generic-function) (method pattern-method) qualifiers specializers
     lambda-expression environment)

  ;; Check for duplicate variables in different PATTERN specializers
  ;; among SPECIALIZERS.
  (print specializers)
  (ignore-errors
   (print (list :duplicates (duplicate-variables (mapcar #'variable-cluster-variables
                                                         (mappend #'specializer-cluster-variable-clusters
                                                                  (first (generic-function-specializer-clusters gf))))))))

  ;; This transforms LAMBDA-EXPRESSION of the form
  ;;
  ;;   (lambda (LAMBDA-LIST) BODY)
  ;;
  ;; into
  ;;
  ;;   (lambda (bindings LAMBDA-LIST)
  ;;     (let ((PATTERN-VAR1 <extract from BINDINGS>)
  ;;           (PATTERN-VAR2 <extract from BINDINGS>)
  ;;           …)
  ;;       BODY)
  ;;
  ;; where BODY contains uses of PATTERN-VAR1, PATTERN-VAR2, …
  (destructuring-bind (operator lambda-list &body body) lambda-expression
    (declare (ignore operator))
    (labels ((do-variable-cluster (variable-cluster variables)
               (list )
               )
             (foo (specializer-cluster specializer)
               (let ((specializer-variables (variables-and-paths->variable-infos
                                             (pattern-variables-and-paths
                                              (optima.core:parse-pattern (second specializer))))))
                 #+no (log:info specializer-variables)
                 (print (remove-if-not (lambda (variable-cluster)
                                         (position-if (lambda (specializers)
                                                        (some #'null specializers))
                                                      (variable-cluster-variables variable-cluster)
                                                      :key #'variable-info-specializers))
                                       (print (add-variables-to-clusters
                                               (specializer-cluster-variable-clusters specializer-cluster)
                                               specializer-variables))))))
             (variable-clusters-for-argument (specializer-clusters specializer)
               (when (typep specializer '(cons (eql pattern)))
                 (let ((interesting-specializer-clusters
                        (remove-if-not (lambda (cluster)
                                         (some (curry #'sb-pcl::same-specializer-p
                                                      (make-instance 'pattern-specializer
                                                                     :pattern (second specializer)))
                                               (specializer-cluster-specializers cluster)))
                                       specializer-clusters)))
                  (remove-duplicates    ; TODO should not be necessary
                   (mappend #'foo interesting-specializer-clusters (circular-list specializer))
                   :key #'variable-cluster-index
                   )))))

      (let* ((specializer-clusters (generic-function-specializer-clusters gf))
             (variable-clusters (mappend #'variable-clusters-for-argument
                                         specializer-clusters specializers))
             (bindings-var (gensym "BINDINGS"))
             (new-lambda-list `(,bindings-var ,@lambda-list))
             (new-lambda-expression
              `(lambda ,new-lambda-list
                 ;; TODO bindings are dynamic-extent?
                 (let ,(emit-bindings variable-clusters bindings-var)
                   ,@body))))
        #+no (log:info "All variables ~A" variable-clusters)
        (print new-lambda-expression)
        ;; Inject a T at the front of SPECIALIZERS to account for the
        ;; injected BINDINGS-VAR parameter.
        (call-next-method
         gf method qualifiers (list* t specializers) new-lambda-expression environment)))))

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
                            (specializer-clusters2 generic-function pattern-specializers)))))))

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

;; TODO the following two cases are probably untested
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

(defstruct specializer-cluster
  (specializers       '()      :type list)
  (%variable-clusters :unbound :type (or (eql :unbound) list)))

(defun specializer-cluster-sort! (cluster generic-function generalizer)
  (flet ((specializer< (specializer1 specializer2)
           (eq '< (specializable:specializer<
                   generic-function specializer1 specializer2 generalizer))))
    (with-accessors ((specializers specializer-cluster-specializers)) cluster
      (setf specializers (stable-sort specializers #'specializer<))))
  cluster)

(defun specializer-cluster-variable-clusters (cluster)
  (let ((value (specializer-cluster-%variable-clusters cluster)))
    (case value
      (:unbound (setf (specializer-cluster-%variable-clusters cluster)
                      (specializers->variable-clusters
                       (specializer-cluster-specializers cluster))
                      #+no (patterns->variable-clusters
                       (mapcar #'specializer-parsed-pattern
                               (specializer-cluster-specializers cluster)))))
      (t        value))))

(defun specializer-clusters2 (generic-function specializers)
  (let ((clusters '()))
    (flet ((belongs-into-cluster-p (specializer cluster)
             (every #+TODO-was some (curry #'in-same-cluster-p generic-function specializer)
                   (specializer-cluster-specializers cluster))))
      (dolist (specializer specializers)
        (dolist (cluster (or (remove-if-not (curry #'belongs-into-cluster-p specializer)
                                            clusters)
                             (let ((cluster (make-specializer-cluster)))
                               (push cluster clusters)
                               (list cluster))))
          (push specializer (specializer-cluster-specializers cluster)))
        #+TODO-was (let ((cluster (or (find-if (curry #'belongs-into-cluster-p specializer)
                                    clusters)
                           (let ((cluster (make-specializer-cluster)))
                             (push cluster clusters)
                             cluster))))
          (push specializer (specializer-cluster-specializers cluster)))))
    (mapc (rcurry #'specializer-cluster-sort! generic-function :todo) clusters)
    clusters))

;;; Generalizers maker

;; TODO precompute a better hash-key here
(defun make-generalizer-maker-form2 (generic-function specializers clusters)
  ;; This generates a lambda expression of the form
  ;;
  ;;   (lambda (arg)
  ;;     (match arg
  ;;       (MOST-SPECIFIC-PATTERN-IN-CLUSTER
  ;;         (let ((bindings (make-array NUMBER-OF-PATTERN-VARS :initial-element nil)))
  ;;           (setf (aref bindings 0) PATTERN-VAR1)
  ;;           (setf (aref bindings 1) PATTERN-VAR2)
  ;;           …
  ;;           (make-pattern-generalizer
  ;;            ;; Specializers of applicable methods, most specific
  ;;            ;; first.
  ;;            '(SPECIALIZER-OBJECTS)
  ;;            'PATTERNS-AS-HASH-KEY
  ;;            bindings))))
  ;;       CLAUSES-FOR-LESS-SPECIFIC-PATTERNS-IN-CLUSTER))
  ;;
  (labels ((cluster-subset-clause (specializers specializer-cluster)
             (destructuring-bind (first &rest rest) specializers ; TODO rename first -> most-specific?
               (declare (ignore rest))
               (let* ((key                   (mappend #'specializer-pattern specializers))
                      (all-variable-clusters (specializer-cluster-variable-clusters
                                              specializer-cluster))
                      (variable-clusters     (remove-if-not ; TODO add some abstraction for this
                                              (lambda (variable-cluster)
                                                (some (lambda (variable-info)
                                                        (member first (variable-info-specializers variable-info)))
                                                      (variable-cluster-variables variable-cluster)))
                                              all-variable-clusters)))
                 (with-unique-names (bindings)
                   `(,(specializer-pattern first) ; TODO duplicate
                     #+no (log:info "matching case ~S" ',(specializer-pattern first))
                     (let ((,bindings ,(emit-make-binding-vector all-variable-clusters)))
                       ,@(emit-initialize-binding-vector variable-clusters bindings)
                       (make-pattern-generalizer
                        '(,@specializers) ',key ,bindings
                        (class-of arg) ; TODO can be avoided when there are no non-pattern specializers?
                        )))))))
           (cluster-clauses (cluster)
             (maplist (rcurry #'cluster-subset-clause cluster)
                      (specializer-cluster-specializers cluster))))
    `(lambda (arg)
       #+no (log:info "matching ~S" arg)
       (optima:match arg
         ,@(mappend #'cluster-clauses clusters)
         (otherwise ; TODO can/should this even happen?
          #+no (log:info "catch-all case")
          nil #+no (make-pattern-generalizer '() nil (vector)))))))

(defun make-generalizer-maker (generic-function specializers clusters)
  (let* ((non-pattern-specializers
          (remove-if (of-type 'pattern-specializer) specializers)) ; TODO repeated elsewhere
         (pattern-specializers
          (set-difference specializers non-pattern-specializers)))
    (values (compile nil (make-generalizer-maker-form2
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
