;;;; pattern-generic-function.lisp --- Pattern generic function and method.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer)

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
                                     parameter-info specializer)))
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
      (let* ((specializers/parsed (let ((*parse-specializer-kind* :early))
                                    (mapcar (compose
                                             (curry #'parse-specializer-using-class
                                                    generic-function))
                                            specializers)))
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

        ;;
        (debug-method-lambda specializers new-lambda-expression)

        ;; Inject the types of the binding variable vectors in front
        ;; of SPECIALIZERS to account for the injected parameters.
        (call-next-method generic-function method qualifiers
                          (append binding-var-types specializers)
                          new-lambda-expression environment)))))

;;; pattern-generic-function

;; Associates a list of `path-info' instances with a binding slot. The
;; `path-info' instances act as reference count: if the final one is
;; removed, the binding slot can be reused for other paths.
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
    ;; Try to find a suitable component for
    ;; SPECIALIZER. `required-parameter-info-components-for' uses
    ;; :forthcoming markers to identify components created for
    ;; SPECIALIZER.
    ;;
    ;; CONTAINING can be:
    ;;
    ;; * An empty list if SPECIALIZER does not have any relation to
    ;;   the specializers in existing components. A new component
    ;;   containing only SPECIALIZER is created in this case.
    ;;
    ;; * A list containing a single component. SPECIALIZER can simply
    ;;   be added to that component in this case.
    ;;
    ;; * A list of components. This happens when the addition of
    ;;   SPECIALIZER to the graph connects existing components. The
    ;;   `specializer-component' instances are merged into one to
    ;;   which SPECIALIZER is added in this case.
    (or (when-let ((containing (required-parameter-info-components-for
                                info specializer)))
          (destructuring-bind (keep &rest removes) containing
            (when removes ; merge KEEP and REMOVES into KEEP.
              (reduce #'specializer-component-merge-into removes
                      :initial-value keep)
              (setf components (set-difference components removes)))
            keep))
        (let ((component (make-specializer-component '())))
          (push component components)
          component))))

;; TODO generic function with methods for {early,late}-pattern-specializer and everything else
;; late-pattern-specializer method will be identical to ...-upgrade-specializer
(defun required-parameter-info-add-specializer (info specializer)
  (typecase specializer
    (pattern-specializer
     (push specializer (required-parameter-info-pattern-specializers info))
     (let* ((component (required-parameter-info-ensure-component-for
                        info specializer))
            (component (specializer-component-add-specializer
                        component specializer)))
       (values info component)))
    (t
     (assert (not (find specializer (required-parameter-info-other-specializers info))))
     (push specializer
           (required-parameter-info-other-specializers info))
     info)))

;; Find `early-pattern-specializer' instances associated with
;; SPECIALIZER and replace them with SPECIALIZER. This is necessary
;; because `defmethod' expansion can only add preliminary
;; `early-pattern-specializer' instances to the
;; `required-parameter-info'. A later call to `add-method' has access
;; to the proper `late-pattern-specializer' instances and calls this
;; functions to replace the former with the latter.
(defun required-parameter-info-upgrade-specializer (info specializer)
  (unless (typep specializer 'late-pattern-specializer)
    (return-from required-parameter-info-upgrade-specializer info))

  (with-accessors ((specializers required-parameter-info-pattern-specializers)
                   (components   required-parameter-info-components))
      info
    (labels ((my-forthcoming-p (specializer*)
               (and (typep specializer* 'early-pattern-specializer)
                    (eq '= (pattern-more-specific-p
                            (specializer-parsed-pattern specializer*)
                            (specializer-parsed-pattern specializer)))))
             (replace-my-forthcoming (specializers)
               (assert (<= 0 (count-if #'my-forthcoming-p specializers) 1))
               (substitute-if specializer #'my-forthcoming-p specializers))
             (replace-in-path-info (info)
               (with-accessors ((specializers path-info-specializers)) info
                 (setf specializers (replace-my-forthcoming specializers))))
             (replace-in-component (component)
               (with-accessors ((specializers specializer-component-specializers)
                                (paths        specializer-component-%paths))
                   component
                 (setf specializers (replace-my-forthcoming specializers))
                 (mapc #'replace-in-path-info paths))))
      (setf specializers (replace-my-forthcoming specializers))
      (mapc #'replace-in-component components)))
  ;; binding-slots in INFO contains copies of the `path-info' s
  ;; already updated via components.
  info)

(defun required-parameter-info-remove-specializer (info specializer)
  (with-accessors ((specializers required-parameter-info-pattern-specializers))
      info
    (typecase specializer
      (pattern-specializer
       (removef specializers specializer)
        ;; Find the component in which SPECIALIZER is contained and
        ;; remove SPECIALIZER from it.
        ;;
        ;; This can lead to the following situations
        ;;
        ;; * The component becomes empty and has to be removed.
        ;;
        ;; * The specializers remaining in the component are no longer
        ;;   connected. The component has to be split into multiple
        ;;   components in this case.
        ;;
        ;; * A set of connected specializers remains in the component. No
        ;;   action besides removing SPECIALIZER from the component is
        ;;   necessary in this case.
        (let* ((components (required-parameter-info-components-for
                            info specializer))
               (component  (first components))) ; only one binding after assert is gone
          (assert (length= 1 components))       ; TODO remove
          (multiple-value-bind (component emptyp disconnected)
              (specializer-component-remove-specializer
               component specializer)
            (cond
              (emptyp
               (removef (required-parameter-info-components info) component))
              (disconnected
               (setf specializers (set-difference specializers disconnected))
               (mapc (lambda (specializer)
                       (mapc (curry #'required-parameter-info-ensure-binding-slot info)
                             (specializer-component-%paths
                              (nth-value 1 (required-parameter-info-add-specializer
                                            info specializer)))))
                     disconnected))))
          (values info component)))
       (t
        (removef specializers specializer :test #'eq)
        info))))

(defun required-parameter-info-ensure-binding-slot (info path-info)
  (declare (type path-info path-info))
  (let* ((slots (required-parameter-info-binding-slots info))
         (slot  (or (find-if (rcurry #'binding-slot-info-find-path path-info) slots)
                    (find-if #'binding-slot-info-unused-p slots)
                    (let ((slot (make-binding-slot-info)))
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
  (mapc (lambda (info specializer)
          (typecase specializer
            (late-pattern-specializer
             (required-parameter-info-upgrade-specializer info specializer))
            (t
             (required-parameter-info-add-specializer info specializer))))
        (generic-function-required-parameter-infos generic-function)
        (method-specializers method)))

(defmethod remove-method :after ((generic-function pattern-generic-function)
                                 (method           pattern-method))
  (mapc #'required-parameter-info-remove-specializer
        (generic-function-required-parameter-infos generic-function)
        (method-specializers method)))

;; TODO there has to be a simpler way
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
                                  (or (debug-generalizer/match
                                       (funcall (sb-ext:truly-the function maker) object next))
                                      (debug-generalizer/next
                                       (funcall (sb-ext:truly-the function next) object))))
                                (lambda (object)
                                  (debug-generalizer/match
                                   (funcall (sb-ext:truly-the function maker) object)))))))))
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
