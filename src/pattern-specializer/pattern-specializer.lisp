;;;; pattern-specializer.lisp --- Implementation of pattern specializers.
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
  ((pattern            :initarg  :pattern
                       :reader   specializer-pattern)
   (parsed-pattern)
   (accepts-p-function :type     function))
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

(defun specializer-normalized-pattern (specializer) ; TODO cache?
  (let ((normalized (pattern-specializer.optima-extensions::normalize-pattern
                     (specializer-parsed-pattern specializer))))
    (typecase normalized
      (optima.core:and-pattern normalized)
      (t                       (optima.core:make-and-pattern normalized)))))

(specializable:define-extended-specializer pattern (generic-function pattern)
  (declare (ignore generic-function))
  (make-instance 'pattern-specializer :pattern pattern))

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
  (pattern-more-specific-p
   (specializer-normalized-pattern specializer1)
   (specializer-normalized-pattern specializer2)))

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

(defmethod specializer-accepts-p-function ((specializer pattern-specializer))
  (if (slot-boundp specializer 'accepts-p-function)
      (slot-value specializer 'accepts-p-function)
      (setf (slot-value specializer 'accepts-p-function)
            (make-predicate (specializer-parsed-pattern specializer)))))

(defmethod specializable:specializer-accepts-p ((specializer pattern-specializer)
                                                object)
  (funcall (sb-ext:truly-the function
             (specializer-accepts-p-function specializer))
           object))

(defmethod specializable:specializer-accepts-generalizer-p
    ((gf specializable:specializable-generic-function)
     (specializer pattern-specializer)
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
     (specializer pattern-specializer)
     (generalizer t))
  (specializer-accepts-generalizer-p-using-pattern
   gf specializer (specializer-parsed-pattern specializer) generalizer))

;; TODO we can do this with `pattern-more-specific-p' by turning class
;; and eql specializers into patterns
(defmethod specializer-accepts-generalizer-p-using-pattern
    ((gf specializable:specializable-generic-function)
     (specializer pattern-specializer)
     (pattern constant-pattern)
     (generalizer t))
  (specializable:specializer-accepts-generalizer-p
   gf
   (make-instance 'sb-pcl:eql-specializer
                  :object (optima.core:constant-pattern-value pattern))
   generalizer))

(defmethod specializer-accepts-generalizer-p-using-pattern
    ((gf specializable:specializable-generic-function)
     (specializer pattern-specializer)
     (pattern variable-pattern)
     (generalizer t))
  (values t t))

(defmethod specializer-accepts-generalizer-p-using-pattern
    ((gf specializable:specializable-generic-function)
     (specializer pattern-specializer)
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
     (specializer pattern-specializer)
     (pattern optima.core:guard-pattern)
     (generalizer t))
  (values t nil)) ; TODO

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
     (specializer pattern-specializer)
     (pattern optima.core:or-pattern)
     (generalizer t))
  (error "not implemented"))

(defmethod specializer-accepts-generalizer-p-using-pattern
    ((gf specializable:specializable-generic-function)
     (specializer pattern-specializer)
     (pattern optima.core:cons-pattern)
     (generalizer t))
  (multiple-value-bind (result definitivep) (subtypep generalizer 'cons)
    (if result
        (values t (and definitivep (pattern-subpatterns-unrestricted-p pattern)))
        (values nil definitivep))))

;;; pattern-method

(defclass pattern-method (standard-method)
  ())

;; TODO move generic part to specializable
(defun check-parameter-variable-name-clashes (lambda-list specializers)
  (multiple-value-bind (required optional rest keyword)
      (parse-ordinary-lambda-list lambda-list)
    (let ((seen (make-hash-table :test #'eq)))
      (labels
          ((required-name-p (name)
             (position name required))
           (optional-name-p (name)
             (or (find name optional :key #'first)
                 (find name optional :key #'third)))
           (rest-name-p (name)
             (eq name rest))
           (keyword-name-p (name)
             (or (find name keyword :key #'cadar)
                 (find name keyword :key #'third)))
           (check-specializer (arg-position specializer)
             (flet ((check-variable (name)
                      (flet ((loose (&rest args)
                               (apply #'pattern-variable-name-error
                                      arg-position specializer name args)))
                        ;; Check against LAMBDA-LIST.
                        (when-let ((position (required-name-p name)))
                          (loose "the name of the ~:R required formal parameter"
                                 (1+ position)))
                        (when-let ((optional (optional-name-p name)))
                          (loose "the ~:[suppliedp ~
                                      variable~:;name~] of the ~S ~
                                      parameter ~S"
                                 (eq name (first optional)) '&optional optional))
                        (when (rest-name-p name)
                          (loose "the ~S parameter ~S" '&rest name))
                        (when-let ((key (keyword-name-p name)))
                          (loose "the ~S parameter ~S" '&key key))
                        ;; TODO check against already seen variables
                        (when-let ((previous (gethash name seen)))
                          (apply #'loose
                                 "the pattern variable ~S in the ~
                                      specializer~
                                      ~@:_~@:_~2@T~S~@:_~@:_~
                                      in the ~:R formal parameter ~
                                      position"
                                 previous))
                        (setf (gethash name seen)
                              (list name (specializer-pattern specializer)
                                    arg-position)))))
               (mapc (compose #'check-variable #'first) ; TODO use specializer-pattern-variables?
                     (pattern-variables-and-paths
                      (specializer-parsed-pattern specializer))))))
        (loop
           :for specializer :in specializers
           :for i :from 1
           :do (when (typep specializer 'pattern-specializer)
                 (check-specializer i specializer)))))))

;; For a generic function with a lambda-list of the form
;;
;;   (required-1 required-2 … required-N …)
;;
;; this transforms LAMBDA-EXPRESSION of the form
;;
;;   (lambda (LAMBDA-LIST) BODY)
;;
;; into
;;
;;   (lambda (bindings-1 bindings-2 … bindings-N LAMBDA-LIST
;;            &aux
;;            (PATTERN-VAR_11 <extract from bindings-1>)
;;            (PATTERN-VAR_12 <extract from bindings-1>)
;;            …
;;            (PATTERN-VAR_21 <extract from bindings-2>)
;;            (PATTERN-VAR_22 <extract from bindings-2>)
;;            …
;;            (PATTERN-VAR_N1 <extract from bindings-N>)
;;            (PATTERN-VAR_N2 <extract from bindings-N>)
;;            …)
;;     BODY)
;;
;; where BODY contains uses of PATTERN-VAR_11, PATTERN-VAR_12, …
(defmethod make-method-lambda-using-specializers
    ((generic-function pattern-generic-function)
     (method pattern-method) qualifiers specializers
     lambda-expression environment)
  (destructuring-bind (operator lambda-list &body body) lambda-expression
    (declare (ignore operator))
    (labels ((make-bindings (bindings-var parameter-info specializer)
               "Return binding clauses for use in a `let' form that
                bind variables used in the method body to values
                extracted from BINDINGS-VAR for the required parameter
                described by PARAMETER-INFO and SPECIALIZER."
               (unless (typep specializer 'pattern-specializer)
                 (return-from make-bindings '()))

               ;; Add SPECIALIZER to PARAMETER-INFO temporarily. .
               ;; This registers pattern variables in the pattern of
               ;; SPECIALIZER in PARAMETER-INFO, forcing the creation
               ;; of binding slots, if necessary. Values for these
               ;; will have to be supplied by the discriminating
               ;; function when the method is applicable.
               ;;
               ;; An identical specializer will be added "properly"
               ;; when the method object is added to GENERIC-FUNCTION.
               (let* ((component (nth-value
                                  1 (required-parameter-info-add-specializer
                                     parameter-info specializer :paths-only t)))
                      (paths     (specializer-component-%paths component))
                      (bindings  '()))
                 ;; Map over pattern variables (and their paths (with
                 ;; `and-pattern' and `guard-pattern' path components
                 ;; removed)) in the pattern of SPECIALIZER and find
                 ;; the bindings slots in which theirs respective
                 ;; values will be stored (register/created above).
                 (mapc-variables-and-paths
                  (lambda (name path)
                    (let* ((path      (path-for-bindings path))
                           (path-info (find path paths
                                            :test #'equal
                                            :key  #'path-info-path)) ; TODO encapsulate this somewhere?
                           (slot      (required-parameter-info-ensure-binding-slot
                                       parameter-info path-info)))
                      (push `(,name (locally
                                        (declare (optimize (sb-c::insert-array-bounds-checks 0)))
                                      (svref (sb-ext:truly-the (simple-array t (*)) ,bindings-var) ,slot)))
                            bindings)))
                  (specializer-parsed-pattern specializer))
                 bindings)))
      (let* ((specializers/parsed (mapcar (curry #'parse-specializer-using-class generic-function)
                                          specializers))
             (required-parameters (generic-function-required-parameter-infos
                                   generic-function))
             (binding-var-names   (map-into (make-list (length required-parameters))
                                            (curry #'gensym "BINDINGS")))
             ;; We do not specify dimensions for the binding vectors
             ;; since these may change when other methods get defined.
             (binding-var-types   (make-list (length required-parameters)
                                             :initial-element 'simple-array))
             (new-lambda-list     `(,@binding-var-names ,@lambda-list))
             (new-lambda-expression
              `(lambda (,@new-lambda-list
                        &aux
                        ,@(mappend #'make-bindings
                                   binding-var-names
                                   required-parameters specializers/parsed))
                 (declare (ignorable ,@binding-var-names))
                 ,@body)))

        ;; Check name clashes of pattern variables with each other and
        ;; with names introduced by the "ordinary" portion of the
        ;; lambda-list.
        (check-parameter-variable-name-clashes lambda-list specializers/parsed)

        ;; Inject the types of the binding variable vectors in front
        ;; of SPECIALIZERS to account for the injected parameters.
        (call-next-method generic-function method qualifiers
                          (append binding-var-types specializers)
                          new-lambda-expression environment)))))

;;; pattern-generic-function

(defstruct (binding-slot-info (:copier nil))
  (paths '() :type list #|of path-info|#))

(defmethod print-object ((object binding-slot-info) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~D)" (length (binding-slot-info-paths object)))))

(defun binding-slot-info-unused-p (info)
  (every (compose #'emptyp #'path-info-specializers)
         (binding-slot-info-paths info)))

(defun binding-slot-info-find-path (info path)
  (let ((path (etypecase path
                (path-info (path-info-path path))
                (list      path)))) ; TODO is list correct?
    (find path (binding-slot-info-paths info)
          :test #'equal :key #'path-info-path)))

(defun binding-slot-info-find-specializer (info specializer)
  (find-if (lambda (path-info)
             (member specializer (path-info-specializers path-info)))
           (binding-slot-info-paths info)))

;; TODO document
(defstruct (required-parameter-info (:copier nil))
  (pattern-specializers '() :type list #|of pattern-specializer|#)
  (other-specializers   '() :type list #|of sb-pcl:specializer|#)
  (components           '() :type list #|of specializer-component|#)
  (binding-slots        (make-array 0 :adjustable t :fill-pointer 0) :type (vector binding-slot-info)))

(defmethod print-object ((object required-parameter-info) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~D[~D]/~D ~S ~D"
            :specializers
            (length (required-parameter-info-pattern-specializers object))
            (length (required-parameter-info-components object))
            (length (required-parameter-info-other-specializers object))
            :slots
            (length (required-parameter-info-binding-slots object)))))

(defun required-parameter-info-components-for (info specializer)
  (declare (type pattern-specializer specializer))
  (remove-if-not (rcurry #'specializer-component-contains-p specializer)
                 (required-parameter-info-components info)))

(defun required-parameter-info-ensure-component-for (info specializer)
  (declare (type required-parameter-info info)
           (type pattern-specializer specializer))
  (with-accessors ((components required-parameter-info-components)) info
    (flet ((remove-forthcoming (component)
             (removef (specializer-component-specializers component) ; TODO more elegant way?
                      :forthcoming)
             (dolist (path-info (specializer-component-%paths component))
               (removef (path-info-specializers path-info) :forthcoming))
             component))
      (or (when-let ((containing (mapcar
                                  #'remove-forthcoming
                                  (required-parameter-info-components-for
                                   info specializer))))
            (destructuring-bind (keep &rest removes) containing
              (when removes
                (mapc (curry #'specializer-component-add-specializer keep)
                      (mappend #'specializer-component-specializers removes))
                (setf components (set-difference components removes)))
              keep))
          (let ((component (make-specializer-component '())))
            (push component components)
            component)))))

(defun required-parameter-info-add-specializer (info specializer
                                                &key paths-only)
  (typecase specializer
    (pattern-specializer
     (unless paths-only
       (push specializer (required-parameter-info-pattern-specializers info)))
     (let ((component (required-parameter-info-ensure-component-for
                       info specializer)))
       (specializer-component-add-specializer
        component specializer :paths-only paths-only)
       (values info component)))
    (t
     (push specializer
           (required-parameter-info-other-specializers info))
     info)))

(defun required-parameter-info-remove-specializer (info specializer)
  (typecase specializer
    (pattern-specializer
     (removef (required-parameter-info-pattern-specializers info)
              specializer)
     (let* ((components (required-parameter-info-components-for
                        info specializer))
            (component  (first components))) ; only one binding after assert is gone
       (assert (length= 1 components)) ; TODO remove
       (when (nth-value 1 (specializer-component-remove-specializer
                           component specializer))
         (removef (required-parameter-info-components info) component))))
    (t
     (removef (required-parameter-info-other-specializers info)
              specializer))))

(defun required-parameter-info-ensure-binding-slot (info path-info)
  (declare (type path-info path-info))
  (let* ((slots (required-parameter-info-binding-slots info))
         (slot  (or (find-if (rcurry #'binding-slot-info-find-path path-info) slots)
                    (find-if #'binding-slot-info-unused-p slots)
                    (let ((slot (make-binding-slot-info )))
                      (vector-push-extend slot slots)
                      slot))))
    (pushnew path-info (binding-slot-info-paths slot))
    (position slot slots)))

;; TODO document
(defclass pattern-generic-function (specializable:specializable-generic-function)
  ((required-parameter-infos :type   list #|of required-parameter-info|#
                             :reader generic-function-required-parameter-infos))
  (:metaclass funcallable-standard-class)
  (:default-initargs
   :method-class (find-class 'pattern-method))) ; TODO is pattern-method even needed?

(defun pattern-generic-function-ensure-required-parameter-infos (generic-function)
  (let ((num-required (sb-pcl::arg-info-number-required
                       (sb-pcl::gf-arg-info generic-function)))) ; TODO make a function somewhere
    (unless (and (slot-boundp generic-function 'required-parameter-infos)
                 (= num-required
                    (length (slot-value generic-function 'required-parameter-infos))))
      (setf (slot-value generic-function 'required-parameter-infos)
            (map-into (make-list num-required) #'make-required-parameter-info)))))

(defmethod generic-function-required-parameter-infos :before
    ((generic-function pattern-generic-function))
  (pattern-generic-function-ensure-required-parameter-infos generic-function))

(defmethod add-method :after ((generic-function pattern-generic-function)
                              (method           pattern-method))
  (mapc #'required-parameter-info-add-specializer
        (generic-function-required-parameter-infos generic-function)
        (method-specializers method)))

(defmethod remove-method :after ((generic-function pattern-generic-function)
                                 (method           pattern-method))
  (mapc #'required-parameter-info-remove-specializer
        (generic-function-required-parameter-infos generic-function)
        (method-specializers method)))

(defmethod specializable:compute-argument-generalizing-function
    ((generic-function pattern-generic-function) arg-position)
  (declare (type fixnum arg-position))
  (let ((cell))
    (flet ((install ()
             (multiple-value-bind (maker accepts-next-method-p)
                 (make-generalizer-maker
                  (elt (generic-function-required-parameter-infos generic-function) arg-position))
              (let* ((next (when accepts-next-method-p (call-next-method))))
                (setf cell (if accepts-next-method-p
                               (lambda (object)
                                 (or (funcall (sb-ext:truly-the function maker) object next)
                                     (funcall (sb-ext:truly-the function next) object)))
                               (lambda (object)
                                 (funcall (sb-ext:truly-the function maker) object))))))))
      (setf cell (lambda (object)
                   (install)
                   (funcall cell object)))
      (lambda (object)
        (funcall (sb-ext:truly-the function cell) object)))))

(defmethod specializable:compute-effective-arguments-function
    ((generic-function pattern-generic-function) num-required)
  (declare (type fixnum num-required))
  (let ((no-bindings (load-time-value (make-array 0 :element-type t :adjustable nil) t)))
    (flet ((maybe-generalizer-bindings (generalizer) ; TODO can be done before invocation time
             (if (typep generalizer 'pattern-generalizer)
                 (pattern-generalizer-variables generalizer)
                 no-bindings)))
      (declare (inline maybe-generalizer-bindings))
      (cond
        ((= 1 num-required)
         (lambda (args generalizers)
           (declare (type list args generalizers))
           (list* (maybe-generalizer-bindings (first generalizers)) args)))
        (t
         (lambda (args generalizers)
           (declare (type list args generalizers))
           (nconc (mapcar #'maybe-generalizer-bindings generalizers) args)))))))
