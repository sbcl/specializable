;;;; satisfiable.lisp --- Satisfiability of pattern expressions.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; Straightforward implementation of the DPLL satisfiability
;;;; algorithm. Based on pseudocode in
;;;; http://en.wikipedia.org/wiki/DPLL_algorithm.

(cl:in-package #:pattern-specializer.optima-extensions)

(defun literal-negative-p (literal)
  (minusp literal))

(defun literal-negate (literal)
  (* -1 literal))

(defun literal-positive-in-clause-p (literal clause)
  (find literal clause :test #'eql))

(defun literal-negative-in-clause-p (literal clause)
  (find (literal-negate literal) clause :test #'eql))

(defun literal-substitute-in-clause (literal polarity clause)
  (let ((result clause))
    (when (literal-positive-in-clause-p literal result)
      (setf result
            (if polarity
                '(t)
                (remove literal result :test #'eql))))
    (when (literal-negative-in-clause-p literal result)
      (setf result
            (if polarity
                (remove (literal-negate literal) result :test #'eql)
                '(t))))
    result))

(defun literal-pure-p (literal clauses)
  (cond
    ((eq literal t)
     nil)
    ((notany (curry #'literal-positive-in-clause-p literal) clauses)
     (values t nil))
    ((notany (curry #'literal-negative-in-clause-p literal) clauses)
     (values t t))))

(defun clause-empty-p (clause)
  (emptyp clause))

(defun clause-unit-p (clause)
  (declare (type list clause))
  (and (length= 1 clause) (not (eq (first clause) t))))

(defun literals-consistent-p (clauses)
  (every (lambda (clause)
           (not (or (emptyp clause) (find t clause :test-not #'eq))))
         clauses))

(defun unit-propagate (clause clauses)
  (let ((literal (first clause)))
    (mapcar (curry #'literal-substitute-in-clause literal t)
            (remove clause clauses))))

(defun pure-literal-assignment (literal polarity clauses)
  (mapcar (curry #'literal-substitute-in-clause literal polarity)
          clauses))

(defun choose-literal (clauses)
  (dolist (clause clauses)
    (dolist (literal clause)
      (when (not (eq literal t))
        (return-from choose-literal literal)))))

(defun dpll (operator clauses)
  (let ((operator (coerce operator 'function)))
    (labels
        ((rec (clauses bindings)
           (flet ((add-binding (literal polarity)
                    (multiple-value-bind (key polarity)
                        (if (literal-negative-p literal)
                            (values (literal-negate literal) (not polarity))
                            (values literal                  polarity))
                      (let ((existing (assoc key bindings)))
                        (cond
                          ((not existing)
                           (list* (cons key polarity) bindings))
                          ((eq (cdr existing) polarity)
                           bindings)
                          (t
                           (error "~@<Incompatible bindings for literal ~S~@:>" key)))))))
             #+no (print (list :entry clauses))
             (cond
               ;; if Φ is a consistent set of literals
               ;;   then return true;
               ((literals-consistent-p clauses)
                (values t bindings))

               ;; if Φ contains an empty clause
               ;;   then return false;
               ((some #'clause-empty-p clauses)
                (values nil bindings))

               (t
                ;; for every unit clause l in Φ
                ;;   Φ ← unit-propagate(l, Φ);
                (loop :for clause := (find-if #'clause-unit-p clauses)
                   :while clause :do
                   #+no (print (list :unit clause))
                   (setf bindings (add-binding (first clause) t)
                         clauses  (unit-propagate clause clauses))
                   #+no (print (list :unit clauses)))

                ;; for every literal l that occurs pure in Φ
                ;;   Φ ← pure-literal-assign(l, Φ);
                (loop :for (literal . polarity)
                   := (block find
                        (dolist (clause clauses)
                          (dolist (literal clause)
                            (multiple-value-bind (result polarity)
                                (literal-pure-p literal clauses)
                              (when result
                                (return-from find (cons literal polarity)))))))
                   :while literal :do
                   #+no (print (list :pure literal polarity))
                   (setf bindings (add-binding literal polarity)
                         clauses  (pure-literal-assignment
                                   literal polarity clauses))
                   #+no (print (list :pure clauses)))

                ;; l ← choose-literal(Φ);
                ;;   return DPLL(Φ ∧ l) or DPLL(Φ ∧ not(l));
                (if-let ((literal (choose-literal clauses)))
                  (progn
                    #+no (print (list :literal literal))
                    (funcall operator
                             (lambda ()
                               (rec (list* (list literal) clauses)
                                    (add-binding literal t)))
                             (lambda ()
                               (rec (list* (list (literal-negate literal)) clauses)
                                    (add-binding literal nil)))))
                  (progn
                    #+no (print :no-literal)
                    (rec clauses bindings))))))))
      (rec clauses '()))))

(defun satisfiablep (clauses)
  "Test satisfiability of the expression consisting of a conjunction
   of the list CLAUSES.

   CLAUSES is a list of clauses interpreted as a conjunction

     (CLAUSE1 CLAUSE2 ...)

   where each CLAUSEN is a list of possibly negated literals

     (LITERAL1 LITERAL2 ...)

   where each LITERALN is one of

     SYMBOL
     (not SYMBOL)

   where the latter form indicates a negated literal. SYMBOLs have no
   significance besides indicating identity of literals.  "
  (dpll (lambda (lhs rhs)
          (declare (type function lhs rhs))
          (multiple-value-bind (result bindings) (funcall lhs)
            (cond
              (result (values result bindings))
              (rhs    (funcall rhs)))))
        clauses))

(defun tautology (clauses)
  (not (satisfiablep (error "cannot negate"))))

;;;

(defun make-implication-pattern (antecedent consequent)
  (make-or-pattern (make-not-pattern antecedent) consequent))

(defun pattern->expression (pattern)
  "Return a logical expression with TODO literals for PATTERN.

   Returns three values:

   1. A logical expression in CNF in which TODO literals are
      uninterned symbols.

   2. An alist with elements of the form

        (LITERAL-NAME . PATTERN-LITERAL-IN-PATTERN)

      mapping TODO literals to the pattern literals they represent.

   3. A list of logical expressions corresponding to relations derived
      between the literals in PATTERN. The recognized relations are
      * implication (both ways)
      * disjointness"
  (let ((next-id   0)
        (variables '()))
    (labels
        ((make-id ()
           (incf next-id))
         (ensure-variable (pattern)
           (let ((cell (or (find-if (lambda (other)
                                      (eq (pattern-more-specific-1-p pattern other) '=))
                                    variables :key #'cdr)
                           (let ((cell (cons (make-id) pattern)))
                             (push cell variables)
                             cell))))
             (car cell)))
         (make-relation (variable other)
           (case (pattern-more-specific-1-p (cdr variable) (cdr other))
             (=  (error "should not happen"))
             (<  `((,(literal-negate (car variable)) ,(car other))))
             (>  `((,(literal-negate (car other)) ,(car variable))))
             (// `((,(literal-negate (car other)) ,(literal-negate (car variable)))))))
         (make-relations ()
           (loop :for (variable . rest) :on variables
              :nconc (loop :for other :in rest
                        :nconc (make-relation variable other)))))
      (values (map-pattern
               'list (lambda (pattern recurse)
                       (typecase pattern
                         (not-pattern
                          (literal-negate (first (funcall recurse))))
                         ((or and-pattern or-pattern)
                          (funcall recurse))
                         (t
                          (ensure-variable pattern))))
               pattern)
              variables
              (make-relations)))))

(defun %pattern-tautology-p (pattern)
  (multiple-value-bind (expression variables relations)
      (pattern->expression (pattern-normalize :fancy pattern))
    (declare (ignore variables))
    (not (satisfiablep (append expression relations)))))

(defun implies-p (pattern1 pattern2)
  (%pattern-tautology-p
   (make-not-pattern (make-implication-pattern pattern1 pattern2))))

(defun disjoint-p (pattern1 pattern2)
  (%pattern-tautology-p (make-and-pattern pattern1 pattern2)))

(implies-p (parse-pattern '(and (cons 1 x) (cons b (type integer))))
           (parse-pattern '(and (cons 1 x) (cons b (type real)))))
