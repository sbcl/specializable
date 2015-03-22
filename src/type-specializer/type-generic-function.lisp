;;;; type-generic-function.lisp --- Type generic function and method.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:type-specializer)

;;; type-method

(defclass type-method (standard-method)
  ())

;; TODO document
(defstruct (required-parameter-info (:copier nil))
  (type-specializers  '() :type list #|of type-specializer|#)
  (other-specializers '() :type list #|of sb-pcl:specializer|#)
  (components         '() :type list #|of specializer-component|#))

(defmethod print-object ((object required-parameter-info) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~D[~D]/~D"
            :specializers
            (length (required-parameter-info-type-specializers object))
            (length (required-parameter-info-components object))
            (length (required-parameter-info-other-specializers object)))))

(defun required-parameter-info-components-for (info specializer)
  (declare (type type-specializer specializer))
  (remove-if-not (rcurry #'specializer-component-contains-p specializer)
                 (required-parameter-info-components info)))

(defun required-parameter-info-ensure-component-for (info specializer)
  (declare (type required-parameter-info info)
           (type type-specializer specializer))
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

(defmethod required-parameter-info-add-specializer
    ((info        required-parameter-info)
     (specializer sb-pcl:specializer))
  (assert (not (find specializer (required-parameter-info-other-specializers info))))
  (push specializer (required-parameter-info-other-specializers info))
  info)

(defmethod required-parameter-info-add-specializer
    ((info        required-parameter-info)
     (specializer type-specializer))
  (assert (not (find specializer (required-parameter-info-type-specializers info))))
  (push specializer (required-parameter-info-type-specializers info))
  (let* ((component (required-parameter-info-ensure-component-for
                     info specializer))
         (component (specializer-component-add-specializer
                     component specializer)))
    (values info component)))

(defun required-parameter-info-remove-specializer (info specializer)
  (with-accessors ((specializers required-parameter-info-type-specializers))
      info
    (typecase specializer
      (type-specializer
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
               (setf specializers (set-difference specializers disconnected)))))
          (values info component)))
       (t
        (removef specializers specializer :test #'eq)
        info))))

;; TODO document
(defclass type-generic-function (specializable:specializable-generic-function)
  ((required-parameter-infos :type   list #|of required-parameter-info|#
                             :reader generic-function-required-parameter-infos))
  (:metaclass funcallable-standard-class)
  (:default-initargs
   :method-class (find-class 'type-method))) ; TODO is type-method even needed?

(defun type-generic-function-ensure-required-parameter-infos (generic-function)
  (let ((num-required (sb-pcl::arg-info-number-required
                       (sb-pcl::gf-arg-info generic-function)))) ; TODO make a function somewhere
    (unless (and (slot-boundp generic-function 'required-parameter-infos)
                 (= num-required
                    (length (slot-value generic-function 'required-parameter-infos))))
      (setf (slot-value generic-function 'required-parameter-infos)
            (map-into (make-list num-required) #'make-required-parameter-info)))))

(defmethod generic-function-required-parameter-infos :before
    ((generic-function type-generic-function))
  (type-generic-function-ensure-required-parameter-infos generic-function))

(defmethod add-method :after ((generic-function type-generic-function)
                              (method           type-method))
  (mapc #'required-parameter-info-add-specializer
        (generic-function-required-parameter-infos generic-function)
        (method-specializers method)))

(defmethod remove-method :after ((generic-function type-generic-function)
                                 (method           type-method))
  (mapc #'required-parameter-info-remove-specializer
        (generic-function-required-parameter-infos generic-function)
        (method-specializers method)))

;; TODO there has to be a simpler way
(defmethod specializable:compute-argument-generalizing-function
    ((generic-function type-generic-function) arg-position)
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
