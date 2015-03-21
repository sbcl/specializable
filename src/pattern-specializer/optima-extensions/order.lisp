;;;; order.lisp --- Ordering of patterns w.r.t. specificity.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer.optima-extensions)

;;; TODO

(defmethod pattern-more-specific-p ((pattern1 optima::pattern)
                                    (pattern2 optima::pattern))
  (let ((=>  (implies-p pattern1 pattern2))
        (<=  (implies-p pattern2 pattern1))
        (//1 (disjoint-p pattern2 pattern1)))
    (cond
      ((and <= =>) '=)
      (=>          '<)
      (<=          '>)
      (//1         '//)
      (t           '/=))))

;;; Implementation of the pattern ordering protocol
;;;
;;; The strategy for a pattern class CLASS is as follows
;;;
;;; 1) There must be a method
;;;
;;;      (pattern-more-specific-p CLASS CLASS)
;;;
;;;    to compare instances of CLASS.
;;;
;;; 2) If CLASS is particularly specific or unspecific, there can be
;;;    methods
;;;
;;;      (pattern-more-specific-p optima::pattern CLASS)
;;;      (pattern-more-specific-p CLASS optima::pattern)
;;;
;;;    returning < and > or > and < as appropriate. The first method
;;;    should be similar to
;;;
;;;      (defmethod pattern-more-specific-p ((pattern1 optima::pattern)
;;;                                          (pattern2 CLASS))
;;;        (if (recursing-complex-pattern-p pattern1)
;;;            (call-next-method)
;;;            '[ > or < ]))
;;;
;;;    to let recursing complex patterns (such as `and', `or', `not')
;;;    take control in the next method. This is not necessary for the
;;;    second method, because the method specialized on the respective
;;;    recursing complex pattern will take precedence automatically.
;;;
;;; 3) For each comparable classes OTHER-CLASS and CLASS, there should
;;;    be a method
;;;
;;;      (pattern-more-specific-p OTHER-CLASS CLASS)
;;;
;;;    to perform the comparison and one method
;;;
;;;      (defmethod pattern-more-specific-p ((pattern1 CLASS)
;;;                                          (pattern2 optima::pattern))
;;;        (specializable:invert-specializer<-relation
;;;          (pattern-more-specific-p pattern2 pattern1)))
;;;
;;;    which will delegate to one the above methods if
;;;    * PATTERN2 is not of type CLASS (because of 1))
;;;    * PATTERN2 is not `recursing-complex-pattern-p' (because there
;;;      are more specific methods for all of those)
;;;
;;;    Note that such methods can be combined with the more general
;;;    behavior established in 2), refining the operation only for
;;;    certain OTHER-CLASS classes.

(defun subpatterns-unrestricted-p (pattern)
  (break) ; TODO is this used anywhere
  (every (of-type 'variable-pattern) (pattern-subpatterns pattern)))

(defmethod pattern-more-specific-1-p :around ((pattern1 optima::pattern)
                                              (pattern2 optima::pattern))
  (assert (not (recursing-complex-pattern-p pattern1)))
  (assert (not (recursing-complex-pattern-p pattern2)))

  ;; Bail out early in trivial cases; This is strictly an optimization
  ;; and should not affect the result.
  (if (eq pattern1 pattern2)
      '=
      (call-next-method)))

;; `constant-pattern'

(defmethod pattern-more-specific-1-p ((pattern1 constant-pattern)
                                      (pattern2 optima::pattern))
  (specializable:invert-specializer<-relation
   (pattern-more-specific-1-p pattern2 pattern1)))

(defmethod pattern-more-specific-1-p ((pattern1 constant-pattern)
                                      (pattern2 constant-pattern))
  (if (equal (constant-pattern-value pattern1)
             (constant-pattern-value pattern2))
      '=
      '//))

(defmethod pattern-more-specific-1-p ((pattern1 variable-pattern)
                                      (pattern2 constant-pattern))
  '>)

(defmethod pattern-more-specific-1-p ((pattern1 optima::pattern)
                                      (pattern2 constant-pattern))
  ;; This relies on normalization having transformed `cons'-valued
  ;; constant patterns into `cons-pattern's.
  (if (guard-pattern-p pattern1) ; TODO need to handle not-pattern here?
      (call-next-method)
      '//))

;; `variable-pattern'

(defmethod pattern-more-specific-1-p ((pattern1 variable-pattern)
                                      (pattern2 optima::pattern))
  (specializable:invert-specializer<-relation
   (pattern-more-specific-1-p pattern2 pattern1)))

(defmethod pattern-more-specific-1-p ((pattern1 optima::pattern)
                                      (pattern2 variable-pattern))
  (cond
    (#+no (bottom-pattern-p pattern1)
          (and (typep pattern1 'not-pattern)
               (typep (not-pattern-subpattern pattern1) 'variable-pattern))
     '<)
    ((recursing-complex-pattern-p pattern1) ; TODO should not be necessary
     nil #+no (call-next-method))
    (t
     '<)))

(defmethod pattern-more-specific-1-p ((pattern1 variable-pattern)
                                      (pattern2 variable-pattern))
  '=)

;; `cons-pattern'

(defmethod pattern-more-specific-1-p ((pattern1 cons-pattern)
                                      (pattern2 optima::pattern))
  (specializable:invert-specializer<-relation
   (pattern-more-specific-1-p pattern2 pattern1)))

(defmethod pattern-more-specific-1-p ((pattern1 cons-pattern)
                                      (pattern2 cons-pattern))
  (let* ((car1 (cons-pattern-car-pattern pattern1))
         (cdr1 (cons-pattern-cdr-pattern pattern1))
         (car2 (cons-pattern-car-pattern pattern2))
         (cdr2 (cons-pattern-cdr-pattern pattern2))
         (result/car (pattern-more-specific-1-p car1 car2))
         (result/cdr (pattern-more-specific-1-p cdr1 cdr2)))
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
      ((or (eq result/car '//) (eq result/cdr '//))
       '//)
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

(defmethod pattern-more-specific-1-p ((pattern1 class-pattern)
                                      (pattern2 optima::pattern))
  (specializable:invert-specializer<-relation
   (pattern-more-specific-1-p pattern2 pattern1)))

(defmethod pattern-more-specific-1-p ((pattern1 class-pattern)
                                      (pattern2 class-pattern))
  (let* ((class1         (class-pattern-class-name pattern1))
         (slots1         (class-pattern-slot-names pattern1))
         (subpatterns1   (pattern-subpatterns pattern1))
         (class2         (class-pattern-class-name pattern2))
         (slots2         (class-pattern-slot-names pattern2))
         (subpatterns2   (pattern-subpatterns pattern2))
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
                                   (pattern-more-specific-1-p subpattern1 subpattern2)
                                   :missing)
                             ((< :missing) (case result
                                             ((nil < =) '<)
                                             (t         '/=)))
                             (>            (case result
                                             ((nil > =) '>)
                                             (t         '/=)))
                             (=            result)
                             (//           '//)
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
               (fewer-slots1-p (case (compare-slots '<)
                                 (// '//)
                                 (t  '/=)))
               (fewer-slots2-p (compare-slots '<))
               (t              (compare-slots '<))))
            (result2
             (cond
               (fewer-slots2-p (case (compare-slots '>)
                                 (// '//)
                                 (t  '/=)))
               (fewer-slots1-p (compare-slots '>))
               (t              (compare-slots '>))))
            (t
             '//)))))))

;; TODO maybe need rule for structure-pattern here

(defmethod pattern-more-specific-1-p ((pattern1 optima::pattern)
                                      (pattern2 class-pattern))
  '//)

;; `structure-pattern'

(defmethod pattern-more-specific-1-p ((pattern1 structure-pattern)
                                      (pattern2 structure-pattern))
  (error "not implemented"))

;;; `guard-pattern'

(defmethod pattern-more-specific-1-p ((pattern1 guard-pattern)
                                      (pattern2 optima::pattern))
  (specializable:invert-specializer<-relation
   (pattern-more-specific-1-p pattern2 pattern1)))

(defmethod pattern-more-specific-1-p ((pattern1 guard-pattern)
                                      (pattern2 guard-pattern))
  (let* ((subpattern1 (guard-pattern-subpattern pattern1))
         (variables1  (mapcar #'car (pattern-variables-and-paths
                                     subpattern1 :include-uninterned t)))
         (test-form1  (guard-pattern-test-form pattern1))
         (type1       (guard-pattern-maybe-type-specifier pattern1))

         (subpattern2 (guard-pattern-subpattern pattern2))
         (variables2  (mapcar #'car (pattern-variables-and-paths
                                     subpattern2 :include-uninterned t)))
         (test-form2  (guard-pattern-test-form pattern2))
         (type2       (guard-pattern-maybe-type-specifier pattern2))

         (sub1-p      (when (and type1 type2)
                        (subtypep type1 type2)))
         (sub2-p      (when (and type1 type2)
                        (subtypep type2 type1))))
    (cond
      ((equal (replace-free-variables-in-form ; TODO make a function
               (mapcar (lambda (name) (cons name nil)) variables1)
               test-form1)
              (replace-free-variables-in-form
               (mapcar (lambda (name) (cons name nil)) variables2)
               test-form2))
       (pattern-more-specific-1-p subpattern1 subpattern2))
      ((and sub1-p sub2-p)
       (pattern-more-specific-1-p subpattern1 subpattern2))
      ((and sub1-p
            (member (pattern-more-specific-1-p subpattern1 subpattern2) '(= <)))
       '<)
      ((and sub2-p
            (member (pattern-more-specific-1-p subpattern2 subpattern1) '(= <)))
       '>)
      (t
       '/=))))

(defmethod pattern-more-specific-1-p ((pattern1 constant-pattern)
                                      (pattern2 guard-pattern))
  (if-let ((type (guard-pattern-maybe-type-specifier pattern2)))
    (compare-types `(eql ,(constant-pattern-value pattern1)) type)
    '/= #+no (call-next-method)))

(defmethod pattern-more-specific-1-p ((pattern1 variable-pattern)
                                      (pattern2 guard-pattern))
  (if-let ((type (guard-pattern-maybe-type pattern2)))
    (if (type= 't type) '= '>)
    (call-next-method)))

(defmethod pattern-more-specific-1-p ((pattern1 cons-pattern)
                                      (pattern2 guard-pattern))
  (if-let ((type (guard-pattern-maybe-type-specifier pattern2)))
    (compare-types (pattern-type-specifier pattern1) type)
    '/=))

(defmethod pattern-more-specific-1-p ((pattern1 class-pattern)
                                      (pattern2 guard-pattern))
  (if-let ((type (guard-pattern-maybe-type-specifier pattern2)))
    (let* ((class (class-pattern-class-name pattern1))
           (relation (compare-types class type)))
      (cond
        ((and (eq '= relation) (class-pattern-slot-names pattern1))
         '<)
        (t
         relation)))
    (call-next-method)))

;; `not-pattern'

;; TODO should these even be called?

(defmethod pattern-more-specific-1-p ((pattern1 not-pattern)
                                      (pattern2 optima::pattern))
  (specializable:invert-specializer<-relation ; TODO make new function for this
   (pattern-more-specific-1-p pattern2 pattern1)))

(defmethod pattern-more-specific-1-p ((pattern1 optima::variable-pattern)
                                      (pattern2 not-pattern)) ; TODO could be (not (type nil)) ...
  '>)

(defmethod pattern-more-specific-1-p ((pattern1 optima::pattern)
                                      (pattern2 not-pattern))
  (let ((subpattern (not-pattern-subpattern pattern2)))
    (when (typep subpattern 'variable-pattern)
      (return-from pattern-more-specific-1-p '>))
    (case (pattern-more-specific-1-p pattern1 subpattern)
      ((< =) '//)
      (t     '/=))))

(defmethod pattern-more-specific-1-p ((pattern1 not-pattern)
                                      (pattern2 not-pattern))
  (case (pattern-more-specific-1-p (not-pattern-subpattern pattern1)
                                   (not-pattern-subpattern pattern2))
    (= '=)
    (t '/=))) ; TODO probably not sufficient
