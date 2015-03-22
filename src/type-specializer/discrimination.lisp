;;;; discrimination.lisp --- TODO.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:type-specializer)

;;; Utility functions

(declaim (ftype (function (t) (values function t &optional))
                specializer-relation-is))
(let ((predicates (make-hash-table :test #'equal)))
  (defun specializer-relation-is (relation)
    (let ((test
           (etypecase relation
             ((member = < > /= //)
              (lambda (result)
                (eq result relation)))
             (cons
              (lambda (result)
                (find result relation :test #'eq))))))
      (declare (type function test))
      (ensure-gethash
       relation predicates
       (lambda (specializer1 specializer2)
         (funcall test (compare-types
                        (specializer-type specializer1)
                        (specializer-type specializer2))))))))

(defun specializer-relation-is-p (relation specializer1 specializer2)
  (funcall (specializer-relation-is relation) specializer1 specializer2))

;;; Graph operations

(defun specializer-transitive-closure (all
                                       &key
                                       (start all)
                                       (up t) (down t))
  (let ((seen   (make-hash-table :test #'eq))
        (result ()))
    (labels
        ((directly-connected (direction node all)
           (when-let* ((test       (specializer-relation-is direction))
                       (candidates (remove-if-not (curry test node) all)))
             (remove-if (lambda (node) (some (rcurry test node) candidates))
                        candidates)))
         (add-closure (node)
           (unless (gethash node seen)
             (setf (gethash node seen) t)
             (when up
               (mapc #'add-closure (directly-connected '< node all)))
             (push node result)
             (when down
               (mapc #'add-closure (directly-connected '> node all))))))
      (mapc #'add-closure start))
    result))

(defstruct (specializer-component
             (:constructor %make-specializer-component ())
             (:copier nil))
  (specializers '() :type list #|of type-specializer|#))

(defmethod print-object ((object specializer-component) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~D"
            (length (specializer-component-specializers object)))))

(defun make-specializer-component (specializers)
  (reduce #'specializer-component-add-specializer specializers
          :initial-value (%make-specializer-component)))

(defun specializer-component-contains-p (component specializer)
  (when-let* ((specializers (specializer-component-specializers component))
              (test         (rcurry (specializer-relation-is '(< > = /=))
                                    specializer)))
    (some test specializers)))

(defun specializer-component-add-specializer (component specializer)
  (push specializer (specializer-component-specializers component))
  ;; TODO explain
  (specializer-component-update-graph component)
  component)

(defun specializer-component-remove-specializer (component specializer)
  (with-accessors ((specializers specializer-component-specializers))
      component
    (removef specializers specializer)
    ;; Topologically re-sort remaining specializers. If removing
    ;; SPECIALIZER disconnected the remaining specializers, keep one
    ;; connected component of specializers and remove the others. The
    ;; third return value is the list of disconnected
    ;; specializers. The caller should form a new
    ;; `specializer-component' instance from these.
    (if (emptyp specializers)
        (values component t nil)
        (let ((disconnected (specializer-component-update-graph component)))
          (values component nil disconnected)))))

(defun specializer-component-update-graph (component)
  (with-accessors ((specializers specializer-component-specializers))
      component
    ;; First remove all `augment-type-specializer' instances, then add
    ;; necessary `augment-type-specializer' instances.
    (setf specializers (remove-if (of-type 'augment-type-specializer)
                                  specializers))
    (tagbody
     :restart
       (loop :for (first . rest) :on specializers :do
          (loop :for second :in rest :do
             (when (specializer-relation-is-p '/= first second)
               (let* ((augment-type `(and ,(specializer-type first)
                                          ,(specializer-type second)))
                      (augment-specializer
                       (make-instance 'augment-type-specializer
                                      :type augment-type)))
                 (unless (find augment-specializer specializers
                               :test (specializer-relation-is '=))
                   (push augment-specializer specializers)
                   (go :restart)))))))

    ;; Rebuild a connected component of specializers, starting with an
    ;; arbitrary one. Two things to note:
    ;;
    ;; 1. SPECIALIZERS may not form a single connected component
    ;;    (e.g. after a specializer has been removed).
    ;;
    ;; 2. Paths consisting only of < and > relations are sufficient to
    ;;    describe components since /= relations have been dealt with
    ;;    in the above augmentation step.
    ;;
    ;; Return a list of specializers that are no longer reachable from
    ;; the computed connected component (see 1.).
    (let ((specializers* (specializer-transitive-closure
                          specializers :start (list (first specializers)))))
      (prog1
          (set-difference specializers specializers* :test #'eq)
        (setf specializers specializers*)))))

(defun specializer-component-merge-into (into-component component)
  (appendf (specializer-component-specializers into-component)
           (specializer-component-specializers component))
  into-component)

;;; Discriminating function generation

;; This generates a lambda expression of the form (a-g-f =
;; argument-generalizing-function):
;;
;;   (lambda (arg [next-a-g-f])
;;     (match arg
;;       (MOST-SPECIFIC-TYPE-IN-COMPONENT
;;         (make-type-generalizer[-with-next]
;;          ;; Specializers of applicable methods, most specific
;;          ;; first.
;;          '(SPECIALIZER-OBJECTS)
;;          'TYPES-AS-HASH-KEY
;;          [(funcall next-a-g-f arg)])))
;;       CLAUSES-FOR-LESS-SPECIFIC-TYPES-IN-CLUSTER))
;;
;; The forms next-a-g-f-related forms are only generated if
;; ACCEPT-NEXT-A-G-F-P is true.
(defun make-generalizer-maker-form (components accept-next-a-g-f-p)
  (debug-make-generalizer-maker-form
   components accept-next-a-g-f-p)

  (with-unique-names (arg next-a-g-f)
    (labels ((make-component-subset-clause (component most-specific-specializer)
               "Return an `typecase' clause using the type of
                MOST-SPECIFIC-SPECIALIZER in COMPONENT."
               (declare (type specializer-component component)
                        (type (or basic-type-specializer)
                              most-specific-specializer))
               (let* ((specializers (specializer-transitive-closure
                                     (specializer-component-specializers component)
                                     :start (list most-specific-specializer)
                                     :down nil))
                      (real-specializers (remove-if (of-type 'augment-type-specializer)
                                                    specializers))
                      (key (mapcar #'specializer-type specializers)) ; TODO better key; can't we just use gensym here?
                      (type (specializer-type most-specific-specializer)))

                 (debug-make-generalizer-maker-form/clause
                  `(,type
                    (,(if accept-next-a-g-f-p
                          'make-type-generalizer-with-next
                          'make-type-generalizer)
                      '(,@real-specializers) ',key
                      ,@(when accept-next-a-g-f-p
                              `((funcall (sb-ext:truly-the function ,next-a-g-f) ,arg)))))
                  specializers type)))

             (make-component-clauses (component)
               "Return a list of `typecase' clauses each of which
                handles one of the specializers in COMPONENT."
               (declare (type specializer-component component))
               (mapcar (lambda (specializer)
                         (make-component-subset-clause component specializer))
                       (specializer-component-specializers component))))
      `(lambda (,arg ,@(when accept-next-a-g-f-p `(,next-a-g-f)))
         ,@(when accept-next-a-g-f-p
             `((declare (type function ,next-a-g-f))))
         (debug-try-match ,arg ,(when accept-next-a-g-f-p t nil))
         (typecase ,arg
           ,@(mappend #'make-component-clauses components)
           (otherwise
            ,@(unless accept-next-a-g-f-p `((debug-no-match)))
            nil))))))

(defun make-generalizer-maker (parameter)
  (declare (type required-parameter-info parameter))
  (print parameter)
  (let* ((components (required-parameter-info-components parameter))
         ;; a-g-f = argument-generalizing-function
         (accept-next-a-g-f-p (when (required-parameter-info-other-specializers parameter) t))
         (form (make-generalizer-maker-form components accept-next-a-g-f-p)))
    (debug-generalizer-maker-form form)
    (values (compile nil form) accept-next-a-g-f-p))) ; TODO handle failed compilation?
