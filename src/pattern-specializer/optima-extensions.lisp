;;;; optima-extensions.lisp --- Necessary extensions of the optima library.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer)

;;; Protocol

(defgeneric pattern-more-specific-p (pattern1 pattern2)
  (:documentation
   "Return true if PATTERN1 is strictly more specific than
    PATTERN2.

    General principles:

    * Constant pattern are more specific than all other patterns

    * Variable patterns are less specific than all other patterns

    * For most complex patterns, subpatterns are compared
      lexicographically. Exceptions:

      * For `class-pattern' s, subclass relations have higher
        precedence. The above rule applies only when the classes are
        identical.

      * `and-pattern's are comparable to all patterns by checking
        whether some of their subpatterns are more specific than the
        pattern in question.

      * `or-pattern's are similar."))
;; TODO flip argument precedence order

(defun subpatterns-unrestricted-p (pattern)
  (every (of-type 'optima.core:variable-pattern)
         (optima.core:complex-pattern-subpatterns pattern)))

;; Implementation

(defmethod pattern-more-specific-p :around ((pattern1 optima::pattern)
                                            (pattern2 optima::pattern))
  (if (eq pattern1 pattern2)
      '=
      (call-next-method)))

(defmethod pattern-more-specific-p ((pattern1 optima::pattern)
                                    (pattern2 optima::pattern))
  '/=)

;; `constant-pattern'

(defmethod pattern-more-specific-p ((pattern1 optima.core:constant-pattern)
                                    (pattern2 optima::pattern))
  (if (typep pattern2 'optima.core:complex-pattern) ; TODO all complex or just and, or, not?
      (call-next-method)
      '<))

(defmethod pattern-more-specific-p ((pattern1 optima::pattern)
                                    (pattern2 optima.core:constant-pattern))
  (if (typep pattern1 'optima.core:complex-pattern) ; TODO necessary?
      (call-next-method)
      '>))

(defmethod pattern-more-specific-p ((pattern1 optima.core:constant-pattern)
                                    (pattern2 optima.core:constant-pattern))
  (if (equal (optima.core:constant-pattern-value pattern1)
             (optima.core:constant-pattern-value pattern2))
      '=
      '/=))

;; `variable-pattern'

(defmethod pattern-more-specific-p ((pattern1 optima.core:variable-pattern)
                                    (pattern2 optima::pattern))
  (if (typep pattern2 '(or optima.core:or-pattern optima.core:and-pattern))
      (call-next-method)
      '>))

(defmethod pattern-more-specific-p ((pattern1 optima::pattern)
                                    (pattern2 optima.core:variable-pattern))
  (if (typep pattern1 '(or optima.core:or-pattern optima.core:and-pattern)) ; TODO should not happen
      (call-next-method)
      '<))

(defmethod pattern-more-specific-p ((pattern1 optima.core:variable-pattern)
                                    (pattern2 optima.core:variable-pattern))
  '=)

;;; `guard-pattern'

(defmethod pattern-more-specific-p ((pattern1 optima.core:guard-pattern)
                                    (pattern2 optima::pattern))
  (cond ; TODO not-pattern
    ((typep pattern2 'optima.core:guard-pattern)
     (call-next-method))
    ((typep pattern2 '(or optima.core:or-pattern optima.core:and-pattern))
     (call-next-method))
    (t
     (specializable:invert-specializer<-relation
      (pattern-more-specific-p pattern2 pattern1)))))

(defmethod pattern-more-specific-p ((pattern1 optima::pattern)
                                    (pattern2 optima.core:guard-pattern))
  (if (typep pattern1 '(or optima.core:or-pattern optima.core:and-pattern)) ; TODO not-pattern
      (call-next-method)
      '>))

(defmethod pattern-more-specific-p ((pattern1 optima.core:guard-pattern)
                                    (pattern2 optima.core:guard-pattern))
                                        ; TODO not enough because of variable names; encode variables with TODO numbers
  (let* ((subpattern1 (optima.core:guard-pattern-subpattern pattern1))
         (variable1   (progn
                        (assert (typep subpattern1 'optima.core:variable-pattern)) ; TODO probably not true; we should collect variables instead
                        (optima.core:variable-pattern-name subpattern1)))
         (test-form1  (optima.core:guard-pattern-test-form pattern1))
         (type1       (type-of-type-guard pattern1))

         (subpattern2 (optima.core:guard-pattern-subpattern pattern2))
         (variable2   (progn
                        (assert (typep subpattern2 'optima.core:variable-pattern))
                        (optima.core:variable-pattern-name subpattern2)))
         (test-form2  (optima.core:guard-pattern-test-form pattern2))
         (type2       (type-of-type-guard pattern2))

         (sub1-p      (when (and type1 type2)
                        (subtypep type1 type2)))
         (sub2-p      (when (and type1 type2)
                        (subtypep type2 type1))))
    (cond
      ((equal (subst nil variable1 test-form1) ; TODO needs a code walker :(
              (subst nil variable2 test-form2))
       (pattern-more-specific-p subpattern1 subpattern2))
      ((and sub1-p sub2-p)
       (pattern-more-specific-p subpattern1 subpattern2))
      ((and sub1-p
            (member (pattern-more-specific-p subpattern1 subpattern2) '(= <)))
       '<)
      ((and sub2-p
            (member (pattern-more-specific-p subpattern2 subpattern1) '(= <)))
       '>)
      (t
       '/=))))

(defun type-of-type-guard (pattern)
  (optima:match (optima.core:guard-pattern-test-form pattern)
    ((list 'typep _ (list 'quote type))
     type)))

(defmethod pattern-more-specific-p ((pattern1 optima.core:constant-pattern)
                                    (pattern2 optima.core:guard-pattern))
  (if-let ((type (type-of-type-guard pattern2)))
    (let* ((value-type `(eql ,(optima.core:constant-pattern-value pattern1)))
           (sub1-p    (subtypep value-type type))
           (sub2-p    (subtypep type value-type)))
     (cond
       ((and sub1-p sub2-p)
        '=)
       ((and sub1-p (not sub2-p))
        '<)
       (t
        '/=)))
    (call-next-method)))

(defmethod pattern-more-specific-p ((pattern1 optima.core:variable-pattern)
                                    (pattern2 optima.core:guard-pattern))
  (if-let ((type (type-of-type-guard pattern2)))
    (if (type= 't type) '= '>)
    (call-next-method)))

(defmethod pattern-more-specific-p ((pattern1 optima.core:cons-pattern)
                                    (pattern2 optima.core:guard-pattern))
  (if-let ((type (type-of-type-guard pattern2)))
    (destructuring-bind (constructor &rest args) (ensure-list type)
      (if (subtypep constructor 'cons)
          (pattern-more-specific-p
           pattern1
           (optima.core:parse-pattern
            `(cons (type ,(or (first args) t))
                   (type ,(or (second args) t)))))
          '/=))
    (call-next-method)))

(defmethod pattern-more-specific-p ((pattern1 optima.core:class-pattern)
                                    (pattern2 optima.core:guard-pattern))
  (if-let ((type (type-of-type-guard pattern2)))
    (let* ((class  (optima.core:class-pattern-class-name pattern1))
           (sub1-p (subtypep class type))
           (sub2-p (subtypep type class)))
      (cond
        ((and sub1-p (not sub2-p))
         '<)
        ((and sub2-p (not sub1-p))
         '>)
        ((and (not sub1-p) (not sub2-p))
         '/=)
        ((optima.core:class-pattern-slot-names pattern1)
         '>)
        (t
         '=)))
    (call-next-method)))

;; `and-pattern'

(defmethod pattern-more-specific-p ((pattern1 optima.core:and-pattern)
                                    (pattern2 optima::pattern))
  (if (typep pattern2 'optima.core:and-pattern)
      (call-next-method)
      (specializable:invert-specializer<-relation
       (pattern-more-specific-p pattern2 pattern1))))

(defmethod pattern-more-specific-p ((pattern1 optima::pattern)
                                    (pattern2 optima.core:and-pattern))
  (reduce (lambda (result subpattern)
            (case (pattern-more-specific-p pattern1 subpattern)
              (<  (case result
                    ((nil <) '<)
                    (=       '=)
                    (t       '/=)))
              (>  (case result
                    ((nil > =) '>)
                    (t         '/=)))
              (=  (case result
                    ((nil < =) '=)
                    (>         '>)
                    (t         '/=)))
              (t '/=)))
          (optima.core:complex-pattern-subpatterns pattern2)
          :initial-value nil))

;; `or-pattern'

(defmethod pattern-more-specific-p ((pattern1 optima.core:or-pattern)
                                    (pattern2 optima::pattern))
  (if (typep pattern2 'optima.core:or-pattern)
      (call-next-method)
      (specializable:invert-specializer<-relation
       (pattern-more-specific-p pattern2 pattern1))))

(defmethod pattern-more-specific-p ((pattern1 optima::pattern)
                                    (pattern2 optima.core:or-pattern))
  (reduce (lambda (result subpattern)
            (case (pattern-more-specific-p pattern1 subpattern)
              (<  '<)
              (>  (case result
                    ((nil >) '>)
                    (t       result)))
              (=  (case result
                    ((nil = >) '=)
                    (t         result)))
              (/= (case result
                    ((nil) '/=)
                    (=     '<)
                    (t     result)))))
          (optima.core:complex-pattern-subpatterns pattern2)
          :initial-value nil))

;; `cons-pattern'

; TODO do this in a generic way via optima.core:complex-pattern-subpatterns
(defmethod pattern-more-specific-p ((pattern1 optima.core:cons-pattern)
                                    (pattern2 optima.core:cons-pattern))
  (let* ((car1 (optima.core:cons-pattern-car-pattern pattern1))
         (cdr1 (optima.core:cons-pattern-cdr-pattern pattern1))
         (car2 (optima.core:cons-pattern-car-pattern pattern2))
         (cdr2 (optima.core:cons-pattern-cdr-pattern pattern2))
         (result/car (pattern-more-specific-p car1 car2))
         (result/cdr (pattern-more-specific-p cdr1 cdr2)))
    (cond
      ((and (eq result/cdr '=) (eq result/car '=))
       '=)
      ((and (eq result/car '<) (member result/cdr '(< =)))
       '<)
      ((and (eq result/cdr '<) (member result/car '(< =)))
       '<)
      ((and (eq result/car '>) (member result/cdr '(> =)))
       '>)
      ((and (eq result/cdr '>) (member result/car '(> =)))
       '>)
      (t
       '/=))))

;; `class-pattern'
;;
;; This uses the following rules:
;;
;; 1. Compare class of both patterns
;;
;;   a) If both have the same class, compare slots and sub-patterns
;;
;;      I) If one pattern has a subset of the slots, compare patterns
;;         associated to common slots
;;        i) If some patterns are more specific => /=
;;        ii) If all patterns are less specific => <
;;
;;      II) If both patterns have identical slots, compare the
;;          associated patterns
;;
;;   b) If one class is a subclass of the other, compare slots and
;;      sub-patterns
;;
;;   c) If no class is a subclass of the other => /=

(defmethod pattern-more-specific-p ((pattern1 optima.core:class-pattern)
                                    (pattern2 optima.core:class-pattern))
  (let* ((class1         (optima.core:class-pattern-class-name pattern1))
         (slots1         (optima.core:class-pattern-slot-names pattern1))
         (subpatterns1   (optima.core:class-pattern-subpatterns pattern1))
         (class2         (optima.core:class-pattern-class-name pattern2))
         (slots2         (optima.core:class-pattern-slot-names pattern2))
         (subpatterns2   (optima.core:class-pattern-subpatterns pattern2))
         (fewer-slots1-p (set-difference slots2 slots1))
         (fewer-slots2-p (set-difference slots1 slots2)))
    (labels ((lookup (slot)
               (when-let ((position (position slot slots2)))
                 (nth position subpatterns2)))
             (compare-slots (initial)
               ;; TODO alternate idea: iterate over (union slots1 slots2); use lookup1 and lookup2 leading to :missing1 and :missing2
               (reduce (lambda (result slot1-and-subpattern1)
                         (destructuring-bind (slot1 . subpattern1) slot1-and-subpattern1
                           (case (if-let ((subpattern2 (lookup slot1)))
                                   (pattern-more-specific-p subpattern1 subpattern2)
                                   :missing)
                             ((< :missing) (case result
                                             ((nil < =) '<)
                                             (t         '/=)))
                             (>            (case result
                                             ((nil > =) '>)
                                             (t         '/=)))
                             (=            result)
                             (t            '/=))))
                       (mapcar #'cons slots1 subpatterns1)
                       :initial-value initial)))
      (multiple-value-bind (result1 certain1-p) (subtypep class1 class2)
        (multiple-value-bind (result2 certain2-p) (subtypep class2 class1)
          (assert (and certain1-p certain2-p))
          (cond
            ((and result1 result2)
             (compare-slots (if fewer-slots1-p '> '=)))
            (result1
             (cond
               (fewer-slots1-p '/=)
               (fewer-slots2-p (compare-slots '<))
               (t              (compare-slots '<))))
            (result2
             (cond
               (fewer-slots2-p '/=)
               (fewer-slots1-p (compare-slots '>))
               (t              (compare-slots '>))))
            (t
             '/=)))))))

;; `structure-pattern'

(defmethod pattern-more-specific-p ((pattern1 optima.core:structure-pattern)
                                    (pattern2 optima.core:structure-pattern))
  (error "not implemented"))

;;; Removing variables

(defgeneric remove-variables (pattern)
  (:method ((pattern t))
    pattern)
  (:method ((pattern optima.core:guard-pattern)) ; leave guard patterns alone
    pattern)
  (:method ((pattern optima.core:complex-pattern))
    (mapc #'remove-variables
          (optima.core:complex-pattern-subpatterns pattern))
    pattern)
  (:method ((pattern optima.core:variable-pattern))
    (setf (optima.core:variable-pattern-name pattern) nil)
    pattern))

;; TODO name; make-predicate-form?
(defun make-checking-form (pattern object-var)
  `(optima:match ,object-var
     (,(remove-variables pattern)
       t)))

(defun make-checking-function (pattern)
  (with-unique-names (object-var)
    (compile nil `(lambda (,object-var)
                    ,(make-checking-form pattern object-var)))))
;;; Variable aliasing protocol

(defgeneric pattern-variables-and-paths (pattern)
  (:documentation
   "Return a list of variables and their respective structural
    position in PATTERN with elements of the form

      (NAME . PATH)

    where PATH is a list of the format

      PATTERN-KIND1 PATTERN-KEY1 ...

    For example, the return value for a `cons-pattern' of the
    form (cons a b) would be

      ((A . (cons-pattern 0)) (b . (cons-pattern 1)))

    ."))

(defgeneric pattern-keys (pattern)
  (:documentation
   "Return a list of objects uniquely encoding the structural
    positions of variables in PATTERN.

    For example, the keys of a `cons-pattern' are '(0 1), the keys of
    a `class-pattern' are the (ordered) slot names."))

;; Implementation

;; TODO or-pattern can have duplicates?

(defmethod pattern-variables-and-paths ((pattern optima.core:constant-pattern))
  '())

(defmethod pattern-variables-and-paths ((pattern optima.core:variable-pattern))
  (with-accessors ((name optima.core:variable-pattern-name)) pattern
    (when (symbol-package name)
      (list (cons name '())))))

(defmethod pattern-keys ((pattern optima.core:complex-pattern))
  (iota (length (optima.core:complex-pattern-subpatterns pattern))))

(defmethod pattern-variables-and-paths ((pattern optima.core:complex-pattern))
  (let ((kind (class-name (class-of pattern))))
    (loop :for key :in (pattern-keys pattern)
       :for subpattern :in (optima.core:complex-pattern-subpatterns pattern)
       :appending (loop :for (name . path) :in (pattern-variables-and-paths
                                                subpattern)
                     :collect (cons name (list* kind key path))))))

(defmethod pattern-keys ((pattern optima.core:class-pattern))
  (optima.core:class-pattern-slot-names pattern))

;;; TODO move to different file?
;;; TODO section title

(defstruct (variable-info (:constructor make-variable-info
                                        (name &optional paths specializers)))
  (name         (required-argument :name) :type symbol :read-only t)
  (paths        '()                       :type list)
  (specializers '()                       :type list))

(defmethod print-object ((object variable-info) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-accessors ((name  variable-info-name)
                     (paths variable-info-paths)) object
      (let ((singletonp (length= 1 paths)))
       (format stream "~A ~:[(~D)~;@~A~]"
               name singletonp (if singletonp (first paths) (length paths)))))))

(defun variables-and-paths->variable-infos (variables-and-paths
                                            &optional specializer)
  (let ((table (make-hash-table :test #'eq)))
    (loop :for (name . path) :in variables-and-paths :do
       (let ((info (ensure-gethash name table (make-variable-info name))))
        (push path (variable-info-paths info))
        (push specializer (variable-info-specializers info))))
    (hash-table-values table)))

(defstruct (variable-cluster (:constructor make-variable-cluster
                                           (index &optional variables)))
  (index      (required-argument :index) :type non-negative-integer :read-only t)
  (variables '()                         :type list))

(defmethod print-object ((object variable-cluster) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-accessors ((index     variable-cluster-index)
                     (variables variable-cluster-variables)) object
      (format stream "~D: ~A"
              index (mapcar #'variable-info-name variables)))))

(defun add-variables-to-clusters (clusters new-variables
                                  &optional
                                  (start-index (length clusters)))
  (flet ((belongs-into-cluster-p (variable cluster)
           (some (lambda (element)
                   (intersection (variable-info-paths element)
                                 (variable-info-paths variable)
                                 :test #'equal))
                 (variable-cluster-variables cluster))))
    (let ((index    start-index)
          (clusters clusters))
      (dolist (variable new-variables clusters)
        (let ((cluster (or (find-if (curry #'belongs-into-cluster-p variable)
                                    clusters)
                           (let ((c (make-variable-cluster index)))
                             (incf index)
                             (push c clusters)
                             c))))
          (push variable (variable-cluster-variables cluster)))))))

(defun patterns->variable-clusters (patterns &optional specializers)
  (reduce #'add-variables-to-clusters patterns
          :key           (compose #'variables-and-paths->variable-infos
                                  #'pattern-variables-and-paths)
          :initial-value '()))

(defun specializers->variable-clusters (specializers)
  (reduce (lambda (clusters specializer)
            (let ((variables (variables-and-paths->variable-infos
                              (pattern-variables-and-paths
                               (specializer-parsed-pattern specializer))
                              specializer)))
              (add-variables-to-clusters clusters variables)))
          specializers
          :initial-value '()))

;;; Prototyping

(defun duplicate-variables (variable-lists)
  ;; VARIABLES is a list of lists of `variable-info'.
  (let ((seen (make-hash-table :test #'eq)))
    (flet ((check-list (variables)
             (dolist (variable variables)
               (with-accessors ((name variable-info-name)) variable
                 (push variable (gethash name seen '()))))))
      (mapc #'check-list variable-lists))
    (remove-if (lambda (entry) (length= 1 (cdr entry)))
               (hash-table-alist seen))))
