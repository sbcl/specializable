;;;; graph.lisp --- Draw graphs of methods and specializers.
;;;;
;;;; Copyright (C) 2014, 2015, Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:specializable.graph)

;;; Utilities

(defun generic-function-nth-arg-specializers (generic-function arg)
  (let ((selector (curry #'nth arg)))
    (mapcar (compose selector #'sb-mop:method-specializers)
            (sb-mop:generic-function-methods generic-function))))

;;;

(defgeneric specializer-label (graph specializer))

(defmethod specializer-label ((graph t) (specializer sb-pcl:specializer))
  (princ-to-string specializer))

;;; Specializer graph

(defclass specializer-graph ()
  ((generic-function  :initarg :generic-function
                      :reader  specializer-graph-generic-function)
   (argument-position :initarg :argument-position
                      :type    non-negative-integer
                      :reader  specializer-graph-argument-position)
   (argument          :initarg :argument
                      :reader  specializer-graph-argument)
   (generalizer       :initarg :generalizer
                      :reader  specializer-graph-generalizer)))

(defmethod cl-dot:graph-object-node ((graph  specializer-graph)
                                     (object sb-pcl:specializer))
  (with-accessors ((generic-function specializer-graph-generic-function)
                   (argument         specializer-graph-argument))
      graph
    (let ((acceptsp (specializable:specializer-accepts-p
                     object argument))
          (string   (specializer-label graph object)))
      (make-instance 'cl-dot:node
                     :attributes (list :label     string
                                       :style     :filled
                                       :fillcolor (if acceptsp
                                                      "white"
                                                      "lightgrey"))))))

(defmethod cl-dot:graph-object-points-to ((graph  specializer-graph)
                                          (object sb-pcl:specializer))
  (with-accessors ((generic-function  specializer-graph-generic-function)
                   (argument-position specializer-graph-argument-position)
                   (generalizer       specializer-graph-generalizer))
      graph
    (flet ((edgep (from to)
             (eq '< (specializable:specializer<
                     generic-function to from generalizer))))
      (let* ((specializers (generic-function-nth-arg-specializers
                            generic-function argument-position))
             (all          (remove-if-not (curry #'edgep object) specializers)))
        (remove-if (lambda (specializer)
                     (some (rcurry #'edgep specializer) all))
                   all)))))

;;; Convenience interface

(defun make-specializer-graph (generic-function argument-position argument)
  (let ((generalizer (specializable:generalizer-of-using-class
                      generic-function argument argument-position)))
    (make-instance 'specializer-graph
                   :generic-function  generic-function
                   :argument-position argument-position
                   :argument          argument
                   :generalizer       generalizer)))

(defun specializer-graph (generic-function argument-position argument output-file
                          &rest args &key &allow-other-keys)
  (let* ((graph        (make-specializer-graph
                        generic-function argument-position argument))
         (specializers (generic-function-nth-arg-specializers
                        (specializer-graph-generic-function graph)
                        argument-position))
         (dot-graph    (cl-dot:generate-graph-from-roots
                        graph specializers
                        (list :label (let ((*print-right-margin* most-positive-fixnum))
                                       (format nil "~A ~:R arg = ~A"
                                               (specializer-graph-generic-function graph)
                                               (specializer-graph-argument-position graph)
                                               (specializer-graph-argument graph)))))))
    (apply #'cl-dot:dot-graph dot-graph output-file args)))
