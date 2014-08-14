;;;; normalize.lisp --- Normalize pattern expressions.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;; This code uses ideas described in
;;;
;;;   Ucko, Aaron Mark (2001): Predicate Dispatching in the Common
;;;   Lisp Object System
;;;
;;; which is available at
;;; ftp://publications.ai.mit.edu/ai-publications/2001/AITR-2001-006.pdf

(cl:in-package #:pattern-specializer.optima-extensions)

(defvar *recursing-complex-pattern-types*
  '(not-pattern and-pattern or-pattern))

(defun recursing-complex-pattern-p (pattern)
  (some (lambda (complex-pattern-type)
          (typep pattern complex-pattern-type))
        *recursing-complex-pattern-types*))

;;;

(sb-ext:defglobal *top-pattern* (make-variable-pattern))
(declaim (sb-ext:always-bound *top-pattern*)
         (type variable-pattern *top-pattern*))

(defun top-pattern-p (pattern)
  (and (typep pattern 'variable-pattern)
       (not (variable-pattern-name pattern)))
  #+no (and (or (typep pattern 'not-pattern)
           (not (recursing-complex-pattern-p pattern)))
       (let ((result (pattern-more-specific-1-p pattern *top-pattern*)))
         (values (member result '(> =)) result))))

(sb-ext:defglobal *bottom-pattern* (make-not-pattern (make-variable-pattern)))
(declaim (sb-ext:always-bound *bottom-pattern*)
         (type not-pattern *bottom-pattern*))

(defun bottom-pattern-p (pattern)
  (and (typep pattern 'not-pattern)
       (and (top-pattern-p (not-pattern-subpattern pattern))))
  #+no (and (or (typep pattern 'not-pattern)
           (not (recursing-complex-pattern-p pattern)))
       (let ((result (pattern-more-specific-1-p pattern *bottom-pattern*)))
         (values (member result '(< =)) result))))

;;; Fancy normalization

(defmethod pattern-normalize ((form    (eql :fancy))
                              (pattern optima::pattern))
  (pattern-normalize '(:literal :cnf/strict #+no :simplify) pattern))

;;; Literal normalization
;;;
;;; Constant
;;;   '(X . Y)            => (cons X Y)
;;;
;;; Cons
;;;   (cons X Y)          => (and (cons X _) (cons _ Y))
;;;   (cons (or X Y) Z)   => (or (cons X Z) (cons Y Z))
;;;
;;; Class
;;;   (class NAME X Y)    => (and (class NAME X) (class NAME Y))
;;;
;;; Guard
;;;   (type (cons X Y))   => (cons (type X) (type Y))
;;;   (type (member X Y)) => (or X Y)
;;;   (guard X (consp X)) => (cons _ _)

(defmethod pattern-normalize-1 ((form    (eql :literal))
                                (pattern constant-pattern))
  (labels ((convert-1 (value)
             (match value
               ((cons car cdr)
                `(cons ,(convert-1 car) ,(convert-1 cdr)))
               ((guard x (symbolp x))
                `',x)
               (x x)))
           (convert (value)
             (when (consp value)
               (parse-pattern (convert-1 value)))))
    (convert (constant-pattern-value pattern))))

(defmethod pattern-normalize-1 ((form    (eql :literal))
                                (pattern cons-pattern))
  (let ((subpatterns (pattern-subpatterns pattern)))
    (cond
      ((call-next-method))
      ;; This following clauses can only execute when the next method
      ;; did nothing meaning that all subpatterns are normalized.

      ;; TODO (cons A B) => (and (cons A <top>) (cons <top> B))
      ((unless (find-if #'top-pattern-p subpatterns)
         (make-and-pattern (make-cons-pattern (first subpatterns) *top-pattern*)
                           (make-cons-pattern *top-pattern* (second subpatterns)))))

      ;; (cons (or A B) C) => (and (cons _ _) (or (cons A C) (cons B C)))
      ;;
      ;; TODO injecting (cons _ _) is probably only needed for (cons
      ;;      (not A) B), not in general?
      ((when-let* ((subpattern (find-if #'recursing-complex-pattern-p subpatterns)))
         (make-and-pattern
          (make-cons-pattern *top-pattern* *top-pattern*)
          (reconstitute-pattern
           subpattern
           (mapcar (lambda (subsubpattern)
                     (apply #'make-cons-pattern
                            (substitute subsubpattern subpattern subpatterns)))
                   (pattern-subpatterns subpattern)))))))))

(defmethod pattern-normalize-1 ((form    (eql :literal))
                                (pattern class-pattern))
  (cond
    ((call-next-method))
    ;; The following clauses can only execute when the next method did
    ;; nothing meaning that all subpatterns are normalized.

    ;; Transform (class NAME (A A) (B B)) => (and (class NAME (A A)) (class NAME (B B)))
    ((when-let* ((subpatterns (the list (pattern-subpatterns pattern)))
                 (slot-names  (class-pattern-slot-names pattern)))
       (unless (length= 1 subpatterns)
         (apply #'make-and-pattern
                (mapcar (lambda (slot-name subsubpattern)
                          (make-class-pattern
                           (class-pattern-class-name pattern)
                           (list slot-name subsubpattern)))
                        slot-names subpatterns)))))

    ;;
    ((when-let* ((subpattern (first (pattern-subpatterns pattern)))
                 (slot-name  (first (class-pattern-slot-names pattern))))
       (when (recursing-complex-pattern-p subpattern)
         (assert (length= 1 (the list (pattern-subpatterns pattern)))) ; TODO remove later
         (make-and-pattern
          (make-class-pattern
           (class-pattern-class-name pattern))
          (reconstitute-pattern
           subpattern (mapcar (lambda (subsubpattern)
                                (make-class-pattern
                                 (class-pattern-class-name pattern)
                                 (list slot-name subsubpattern)))
                              (pattern-subpatterns subpattern)))))))))

(defmethod pattern-normalize-1 ((form    (eql :literal))
                                (pattern guard-pattern))
  (cond

    ;; (type VAR TYPE)
    ((when-let ((type (guard-pattern-maybe-type pattern)))
       (flet ((constant (thing)
                (if (symbolp thing)
                    `',thing
                    thing))
              (maybe-type (specifier)
                (let ((specifier (or specifier t)))
                  (if (eq specifier t)
                      '_
                      `(type ,specifier)))))
         (match type
           ((list 'eql value)
            (parse-pattern (constant value)))
           ((guard type (type= type t))
            (make-variable-pattern))
           ('cons
            (parse-pattern '(cons _ _)))
           ((list 'cons car cdr)
            (parse-pattern `(cons ,(maybe-type car)
                                  ,(maybe-type cdr))))
           ((list* 'member values)
            (parse-pattern `(or ,@(mapcar #'constant values))))
           ((list 'satisfies predicate)
            (parse-pattern `(guard it (,predicate it))))
           ((list* (and operator (or 'or 'and 'not)) types)
            (parse-pattern `(,operator ,@(mapcar #'maybe-type types))))))))

    ;; (guard VAR (PREDICATE variable))
    ((when-let ((predicate (guard-pattern-maybe-predicate pattern)))
       (case predicate
         (consp (parse-pattern `(cons _ _))))))))

;;; Conjunctive Normal Form (CNF) normalization

(defmethod pattern-normalize ((form    (eql :cnf/strict))
                              (pattern optima::pattern))
  (let ((result (pattern-normalize :cnf pattern)))
    (flet ((maybe-wrap-and (pattern)
             (if (typep pattern 'and-pattern)
                 pattern
                 (make-and-pattern pattern)))
           (maybe-wrap-or (pattern)
             (if (typep pattern 'or-pattern)
                 pattern
                 (make-or-pattern pattern))))
      (apply #'make-and-pattern
             (mapcar #'maybe-wrap-or (pattern-subpatterns
                                      (maybe-wrap-and result)))))))

(defun simplify-associative-operator (pattern subpatterns
                                      type constructor empty-value)
  (let ((subpatterns1 (funcall subpatterns pattern)))
    (declare (type list subpatterns1))
    (cond
      ((emptyp subpatterns1)
       empty-value)
      ((length= 1 subpatterns1)
       (first subpatterns1))
      ((when-let ((patterns (remove-if-not (of-type type) subpatterns1)))
         (apply constructor
                (append (set-difference subpatterns1 patterns)
                        (mappend subpatterns patterns))))))))

(defun distribute-logical-operators (pattern subpatterns
                                     outer-constructor
                                     inner-type inner-constructor)
  (let ((subpatterns1 (funcall subpatterns pattern)))
    (when-let ((inner-pattern (find-if (of-type inner-type) subpatterns1)))
      (let ((other-patterns (remove inner-pattern subpatterns1)))
        (apply inner-constructor
               (mapcar (lambda (subpattern)
                         (apply outer-constructor (list* subpattern other-patterns)))
                       (funcall subpatterns inner-pattern)))))))

(defmethod pattern-normalize-1 ((form    (eql :cnf))
                                (pattern not-pattern))
  (flet ((make-not (&rest subpatterns)
           (make-not-pattern (first subpatterns))))
    (cond
      ;; Eliminate double negation.
      ((when (typep (not-pattern-subpattern pattern) 'not-pattern)
         (not-pattern-subpattern (not-pattern-subpattern pattern))))

      ((distribute-logical-operators
        pattern #'pattern-subpatterns
        #'make-not 'or-pattern #'make-and-pattern))

      ((distribute-logical-operators
        pattern #'pattern-subpatterns
        #'make-not 'and-pattern #'make-or-pattern)))))

(defmethod pattern-normalize-1 ((form    (eql :cnf))
                                (pattern and-pattern))
  (cond
    ((simplify-associative-operator
      pattern #'pattern-subpatterns
      'and-pattern #'make-and-pattern *top-pattern*))))

(defmethod pattern-normalize-1 ((form    (eql :cnf))
                                (pattern or-pattern))
  (cond
    ((simplify-associative-operator
      pattern #'pattern-subpatterns
      'or-pattern #'make-or-pattern *bottom-pattern*))

    ((distribute-logical-operators
      pattern #'pattern-subpatterns
      #'make-or-pattern 'and-pattern #'make-and-pattern))))

;;; Simplification

(defun some-opposite-p (subpatterns &key disjointp)
  (map-product
   (lambda (left right)
     (cond
       ((eq left right))
       ((and disjointp
             (or (typep left 'not-pattern)
                 (not (recursing-complex-pattern-p left)))
             (or (typep right 'not-pattern)
                 (not (recursing-complex-pattern-p right)))
             (eq (pattern-more-specific-1-p left right) '//))
        (return-from some-opposite-p t))
       ((let ((complement (pattern-normalize :fancy (make-not-pattern left))))
          (and (or (typep right 'not-pattern)
                   (not (recursing-complex-pattern-p right)))
               (or (typep complement 'not-pattern)
                   (not (recursing-complex-pattern-p complement)))
               (member (pattern-more-specific-1-p complement right)
                       (if disjointp '(> =) '(< =)))))
        (return-from some-opposite-p t))
       ((let ((complement (pattern-normalize :fancy (make-not-pattern right))))
          (and (or (typep left 'not-pattern)
                   (not (recursing-complex-pattern-p left)))
               (or (typep complement 'not-pattern)
                   (not (recursing-complex-pattern-p complement)))
               (member (pattern-more-specific-1-p left complement)
                       (if disjointp '(< =) '(> =)))))
        (return-from some-opposite-p t))))
   subpatterns subpatterns)
  nil)

(defun find-redundant (subpatterns relation)
  (map-product
   (lambda (left right)
     (cond
       ((eq left right))
       (#+no (and (or (typep left 'not-pattern) ; TODO not sufficient for e.g. (and (or 1 2) 1) => 1
                      (not (recursing-complex-pattern-p left)))
                  (or (typep right 'not-pattern)
                      (not (recursing-complex-pattern-p right)))
                  (member (pattern-more-specific-1-p left right) (list relation '=)))
             (member (pattern-more-specific-p left right) (list relation '=))
             (return-from find-redundant left))))
   subpatterns subpatterns)
  nil)

(defmethod pattern-normalize-1 ((form    (eql :simplify))
                                (pattern and-pattern))
  (let ((subpatterns (pattern-subpatterns pattern)))
    (cond
      ((find-if #'bottom-pattern-p subpatterns)
       *bottom-pattern*)

      ((when-let ((tops (remove-if-not #'top-pattern-p subpatterns)))
         (apply #'make-and-pattern (set-difference subpatterns tops))))

      ((when (some-opposite-p subpatterns :disjointp t)
         *bottom-pattern*))

      ((when-let ((redundant (find-redundant subpatterns '>)))
         (apply #'make-and-pattern (remove redundant subpatterns)))))))

(defmethod pattern-normalize-1 ((form    (eql :simplify))
                                (pattern or-pattern))
  (let ((subpatterns (pattern-subpatterns pattern)))
    (cond
      ((find-if #'top-pattern-p subpatterns)
       *top-pattern*)

      ((when-let ((bottoms (remove-if-not #'bottom-pattern-p subpatterns)))
         (apply #'make-or-pattern (set-difference subpatterns bottoms))))

      ((when (some-opposite-p subpatterns)
         *top-pattern*))

      ((when-let ((redundant (find-redundant subpatterns '<)))
         (apply #'make-or-pattern (remove redundant subpatterns)))))))
