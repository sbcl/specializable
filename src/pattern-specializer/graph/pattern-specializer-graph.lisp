;;;; pattern-specializer-graph.lisp --- TODO
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer.graph)

;;; Specializer graph integration

(defclass pattern-specializer-graph (specializable.graph::specializer-graph)
  ((context :initarg :context
            :reader  specializer-graph-context)))

(defmethod specializer-label ((graph       pattern-specializer-graph)
                              (specializer pattern-specializer))
  (let ((pattern-label (pattern-label1
                         (pattern-specializer::specializer-parsed-pattern specializer)
                         (specializer-graph-context graph))))
    `(:font ()
      (format nil "#<~A " ,(class-name (class-of specializer)))
      ,pattern-label ">")))

(defmethod specializable.graph:make-specializer-graph ((generic-function  pattern-generic-function)
                                                       (argument-position integer)
                                                       (argument          t))
  (let* ((graph                (call-next-method))
         (specializers         (specializer-graph-specializers graph))
         (pattern-specializers (remove-if-not (of-type 'pattern-specializer)
                                              specializers))
         (patterns             (mapcar #'pattern-specializer::specializer-parsed-pattern
                                       pattern-specializers)))
    (change-class graph 'pattern-specializer-graph
                  :context (patterns->variable-context patterns))))

;;; Test

#+no (cl-dot:dot-graph
 (let ((graph (make-instance 'specializer-graph
                             :generic-function
                             #+no #'pattern-specializer.test::class-pattern
                             #+no #'pattern-specializer.test::next-method.1
                             #+no #'pattern-specializer.examples.lambda-calculus::parse
                             #+no #'pattern-specializer.examples.lambda-calculus::eval1
                             #+no #'pattern-specializer.examples.simplifier::simplify
                             :generalizer (find-class 'integer))))
   (cl-dot:generate-graph-from-roots graph (generic-function-specializers
                                            (specializer-graph-generic-function graph) 0)))
 "/tmp/patterns.png" :format :png)
