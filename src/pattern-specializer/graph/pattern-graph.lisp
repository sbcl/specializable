;;;; pattern-graph.lisp --- TODO
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer.graph)

;;; Pattern graph

(defclass pattern-graph ()
  ((patterns :initarg :patterns
             :type    list
             :reader  pattern-graph-patterns)
   (context  :initarg :context
             :reader  pattern-graph-context)))

(defmethod cl-dot:graph-object-node ((graph  pattern-graph)
                                     (object optima::pattern))
  (let* ((context (pattern-graph-context graph))
         (string  (pattern-label1 object context)))
    (make-instance 'cl-dot:node
                   :attributes (list :label `(:html () ,string)))))

(defmethod cl-dot:graph-object-points-to ((graph  pattern-graph)
                                          (object optima::pattern))
  (flet ((edgep (from to)
           (eq '< (pattern-more-specific-p to from))))
    (let ((all (remove-if-not (curry #'edgep object)
                              (pattern-graph-patterns graph))))
      (remove-if (lambda (pattern)
                   (some (rcurry #'edgep pattern) all))
                 all))))

(defun make-pattern-graph (patterns)
  (flet ((maybe-make-pattern (pattern)
           (typecase pattern
             (optima::pattern pattern)
             (t               (optima.core:parse-pattern pattern)))))
    (let ((patterns (mapcar #'maybe-make-pattern patterns)))
      (make-instance 'pattern-graph
                     :patterns patterns
                     :context  (patterns->variable-context patterns)))))

(defun pattern-graph (patterns output-file
                      &rest args &key &allow-other-keys)
  (let* ((graph     (make-pattern-graph patterns))
         (dot-graph (cl-dot:generate-graph-from-roots
                     graph (pattern-graph-patterns graph))))
    (apply #'cl-dot:dot-graph dot-graph output-file args)))

;;; Test

#+no (pattern-graph '((cons 1 nil)
                 (cons 2 nil)
                 (cons (cons e f) g)
                 (cons (cons (cons x 3) 2) h)
                 (cons (cons (and 1 z) 2) h)
                 (cons b nil)
                 (cons b (type symbol))
                 (cons b (type list))
                 (cons b c))
               "/tmp/bla.png" :format :png)

#+no (pattern-graph '((list '* 0 x)
                 (list '* 1 x)
                 (list '+ 0 x)
                 (list '/ x 1)
                 (list* (and op (type symbol)) args)
                 (list* (and op (type symbol))
                        (optima:guard args (every #'numberp args)))
                 (list* (and op (type (member + *)))
                             (optima:guard args (and (notany #'consp args)
                                              (some #'constantp args)
                                              (notevery #'constantp args)))))
               "/tmp/patterns-2.png" :format :png)
