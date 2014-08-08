;;;; discrimination.lisp --- TODO.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer)

;;; Utility functions

(declaim (ftype (function (t) (values function t &optional nil))
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
         (funcall test (pattern-more-specific-p
                        (specializer-parsed-pattern specializer1)
                        (specializer-parsed-pattern specializer2))))))))

(defun specializer-relation-is-p (relation specializer1 specializer2)
  (funcall (specializer-relation-is relation) specializer1 specializer2))

;;; Paths

(defun path-for-bindings (path)
  (remove-if (of-type '(member and-pattern guard-pattern)) path
             :key #'car))

(defstruct (path-info (:constructor make-path-info (path))
                      (:copier nil))
  (path         (required-argument :path) :type list :read-only t)
  (specializers '()                       :type list))

(defmethod print-object ((object path-info) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~D)" (length (path-info-specializers object)))))

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
  (specializers '() :type list #|of pattern-specializer|#)
  (%paths       '() :type list #|of path-info|#))

(defmethod print-object ((object specializer-component) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~D/~D)"
            (length (specializer-component-%paths object))
            (length (specializer-component-specializers object)))))

(defun make-specializer-component (specializers)
  (reduce #'specializer-component-add-specializer specializers
          :initial-value (%make-specializer-component)))

(defun specializer-component-paths (component)
  (mapcar #'path-info-path (specializer-component-%paths component)))

(defun specializer-component-contains-p (component specializer)
  (when-let* ((specializers (specializer-component-specializers component))
              (test         (rcurry (specializer-relation-is '(< > = /=))
                                    specializer)))
    (some test specializers)))

(defun specializer-component-add-specializer (component specializer)
  (with-accessors ((specializers specializer-component-specializers)
                   (paths        specializer-component-%paths))
      component
    (push specializer specializers)
    (mapc-variables-and-paths
     (lambda (name path)
       (declare (ignore name))
       (let* ((path      (path-for-bindings path))
              (path-info (or (find path paths :key #'path-info-path :test #'equal)
                             (let ((info (make-path-info path)))
                               (assert (not (find info paths))) ; TODO remove
                               (push info paths)
                               info))))
         (push specializer (path-info-specializers path-info))))
     (specializer-parsed-pattern specializer))
    ;; TODO explain
    (specializer-component-update-graph component))
  component)

(defun specializer-component-remove-specializer (component specializer)
  (with-accessors ((specializers specializer-component-specializers)
                   (paths        specializer-component-%paths))
      component
    (flet ((remove-specializer-paths (specializer)
             (mapc-variables-and-paths
              (lambda (name path)
                (declare (ignore name))
                (let* ((path      (path-for-bindings path))
                       (path-info (find path paths
                                        :key  #'path-info-path
                                        :test #'equal)))
                  (assert path-info) ; TODO remove
                  (removef (path-info-specializers path-info) specializer)
                  (when (emptyp (path-info-specializers path-info))
                    (removef paths path-info))))
              (specializer-parsed-pattern specializer))))
      ;; Remove SPECIALIZER and unreference its paths.
      (removef specializers specializer)
      (remove-specializer-paths specializer)
      ;; Topologically re-sort remaining specializers. If removing
      ;; SPECIALIZER disconnected the remaining specializers, keep one
      ;; connected component of specializers and remove the
      ;; others. The third return value is the list of disconnected
      ;; specializers. The caller should form a new
      ;; `specializer-component' instance from these.
      (if (emptyp specializers)
          (values component t nil)
          (let ((disconnected (specializer-component-update-graph component)))
            (mapc #'remove-specializer-paths disconnected)
            (values component nil disconnected))))))

(defun specializer-component-update-graph (component)
  (with-accessors ((specializers specializer-component-specializers)
                   (paths        specializer-component-%paths))
      component
    ;; First remove all `augment-pattern-specializer' instances, then
    ;; add necessary `augment-pattern-specializer' instances.
    (setf specializers (remove-if (of-type 'augment-pattern-specializer)
                                  specializers))
    (tagbody
     :restart
       (loop :for (first . rest) :on specializers :do
          (loop :for second :in rest :do
             (when (specializer-relation-is-p '/= first second)
               (let* ((augment-pattern `(and ,(unparse-pattern ; TODO make a function
                                               (pattern-anonymize-variables
                                                (specializer-parsed-pattern first)))
                                             ,(unparse-pattern
                                               (pattern-anonymize-variables
                                                (specializer-parsed-pattern second)))))
                      (augment-specializer
                       (make-instance 'augment-pattern-specializer
                                      :pattern augment-pattern)))
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
  (appendf (specializer-component-%paths into-component)
           (specializer-component-%paths component))
  into-component)

;;; Discriminating function generation

(defun augment-pattern-for-discriminating-function (pattern paths)
  "TODO of the form

     (PATH NAME POSITION)

   where PATH is "
  (format t "~S~%~{| ~S~%~}"
          'augment-pattern-for-discriminating-function paths)
  (let ((variables '()))
    (flet ((maybe-add-entry (name position)
             #+no (if (find position variables :test #'= :key #'third)
                 (format t "  ~A => duplicate~%"
                         :path)
                 (format t "  ~A => ~A~%    ~A~%"
                         :path position
                         (list name position)))
             (unless (find position variables :test #'eq :key #'second)
               (push (list name position) variables))))
      (values
       (map-patterns-and-paths/reconstitute
        (lambda (pattern path recurse reconstitute)
          (declare (ignore recurse))
          (let ((path/binding (path-for-bindings path)))
            (when-let ((position (find path/binding paths :test #'equal)))
              (typecase pattern
                (constant-pattern
                 (let ((value-form (unparse-pattern pattern)))
                   (maybe-add-entry (cons :constant value-form) position))
                 (values))
                (variable-pattern
                 (let ((name (variable-pattern-name pattern)))
                   (maybe-add-entry name position))
                 (values)) ; copy the pattern unmodified
                ((not (or guard-pattern and-pattern))
                 (let ((name (gensym)))
                   (when (maybe-add-entry name position)
                     (make-and-pattern (make-variable-pattern name)
                                       (funcall reconstitute)))))))))
        pattern)
       variables))))

;; This generates a lambda expression of the form (a-g-f =
;; argument-generalizing-function):
;;
;;   (lambda (arg [next-a-g-f])
;;     (match arg
;;       (MOST-SPECIFIC-PATTERN-IN-COMPONENT
;;         (let ((bindings (make-array NUMBER-OF-PATTERN-VARS :initial-element nil)))
;;           (setf (aref bindings 0) PATTERN-VAR1)
;;           (setf (aref bindings 1) PATTERN-VAR2)
;;           …
;;           (make-pattern-generalizer[-with-next]
;;            ;; Specializers of applicable methods, most specific
;;            ;; first.
;;            '(SPECIALIZER-OBJECTS)
;;            'PATTERNS-AS-HASH-KEY
;;            bindings
;;            [(funcall next-a-g-f arg)]))))
;;       CLAUSES-FOR-LESS-SPECIFIC-PATTERNS-IN-CLUSTER))
;;
;; The forms next-a-g-f-related forms are only generated if
;; ACCEPT-NEXT-A-G-F-P is true.
(defun make-generalizer-maker-form (components binding-slot-infos accept-next-a-g-f-p)
  (with-unique-names (arg next-a-g-f bindings)
    (labels ((make-binding-vector (variables) ; TODO separate function?
               (when (emptyp variables)
                 (return-from make-binding-vector
                   `(load-time-value (vector) t)))

               (let ((forms (make-list (length binding-slot-infos))))
                 (loop :for (name position) :in variables :do
                    (let ((position (position position binding-slot-infos
                                              :test (lambda (x y)
                                                      (binding-slot-info-find-path y x)))))
                      (setf (elt forms position)
                            (ematch name
                              ((cons :constant value)   value)
                              ((and name (type symbol)) name)))))
                 `(vector ,@forms)))
             (make-component-subset-clause (component most-specific-specializer)
               "Return an `optima:match' clause using the pattern of
                MOST-SPECIFIC-SPECIALIZER in COMPONENT."
               (declare (type specializer-component component)
                        (type (or late-pattern-specializer
                                  augment-pattern-specializer)
                              most-specific-specializer))
               (let* ((specializers (specializer-transitive-closure
                                     (specializer-component-specializers component)
                                     :start (list most-specific-specializer)
                                     :down nil))
                      (real-specializers (remove-if (of-type 'augment-pattern-specializer)
                                                    specializers))
                      (used-paths (remove nil ; TODO ugly
                                          (map 'list (lambda (info)
                                                       (when (some (curry #'binding-slot-info-find-specializer info)
                                                                   real-specializers)
                                                         (first (binding-slot-info-paths info))))
                                               binding-slot-infos)))
                      (key (mapcar #'specializer-pattern specializers))) ; TODO better key; can't we just use gensym here?

                 (multiple-value-bind (pattern variables)
                     (augment-pattern-for-discriminating-function
                      (specializer-parsed-pattern most-specific-specializer)
                      (mapcar #'path-info-path used-paths))

                   (format t "~A:~%~
                            ~2@T~A~%~
                            ~2@T~A~%~
                            ~2@TAugmented~%~
                            ~4@T~A~%~
                            ~4@T~A~%"
                           'make-generalizer-maker-form
                           specializers used-paths
                           pattern variables)

                   `(,(unparse-pattern pattern)
                     (let ((,bindings ,(make-binding-vector variables)))
                       #+no ,@(when *debug*
                                `((debug-clause-matching
                                   ,pattern (list ,@specializers)
                                   ',binding-slot-infos ',variables ,bindings)))
                       (,(if accept-next-a-g-f-p
                             'make-pattern-generalizer-with-next
                             'make-pattern-generalizer)
                        '(,@real-specializers) ',key ,bindings
                        ,@(when accept-next-a-g-f-p
                            `((funcall (sb-ext:truly-the function ,next-a-g-f) ,arg))))))
                   #+no (debug-clause pattern specializers binding-slot-infos variables form)
                   )))
             (make-component-clauses (component)
               "Return a list of `optima:match' clauses each of which
                handles one of the specializers in COMPONENT."
               (declare (type specializer-component component))
               (mapcar (lambda (specializer)
                         (make-component-subset-clause component specializer))
                       (specializer-component-specializers component))))
      `(lambda (,arg ,@(when accept-next-a-g-f-p `(,next-a-g-f)))
         ,@(when accept-next-a-g-f-p
             `((declare (type function ,next-a-g-f))))
         (debug-try-match ,arg)
         (match ,arg
           ,@(mappend #'make-component-clauses components)
           (otherwise
            (debug-no-match)
            nil))))))

(defun make-generalizer-maker (parameter)
  (declare (type required-parameter-info parameter))
  (let* ((binding-slot-infos   (required-parameter-info-binding-slots parameter))
         (components (required-parameter-info-components parameter))
         ;; a-g-f = argument-generalizing-function
         (accept-next-a-g-f-p (when (required-parameter-info-other-specializers parameter) t))
         (form (make-generalizer-maker-form components binding-slot-infos accept-next-a-g-f-p)))
    (debug-generalizer-maker-form form)
    (values (compile nil form) accept-next-a-g-f-p))) ; TODO handle failed compilation?
