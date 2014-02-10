(cl:in-package #:specializable.graph)

;;; Utilities

(defun generic-function-nth-arg-specializers (generic-function arg)
  (let ((selector (curry #'nth arg)))
    (mapcar (compose selector #'sb-mop:method-specializers)
            (sb-mop:generic-function-methods generic-function))))

;;; Specializer graph

(defclass specializer-graph ()
  ((generic-function :initarg :generic-function
                     :reader  specializer-graph-generic-function)
   (argument         :initarg :argument
                     :type    non-negative-integer
                     :reader  specializer-graph-argument)
   (generalizer      :initarg :generalizer
                     :reader  specializer-graph-generalizer)))

(defmethod cl-dot:graph-object-node ((graph  specializer-graph)
                                     (object sb-pcl:specializer))
  (with-accessors ((generic-function specializer-graph-generic-function)
                   (generalizer      specializer-graph-generalizer)) graph
    (let ((acceptsp (specializable:specializer-accepts-generalizer-p
                     generic-function object generalizer)))
      (make-instance 'cl-dot:node
                     :attributes (list :label     (princ-to-string object)
                                       :style     :filled
                                       :fillcolor (if acceptsp
                                                      "white"
                                                      "grey"))))))

(defmethod cl-dot:graph-object-points-to ((graph  specializer-graph)
                                          (object sb-pcl:specializer))
  (with-accessors ((generic-function specializer-graph-generic-function)
                   (argument         specializer-graph-argument)
                   (generalizer      specializer-graph-generalizer)) graph
    (flet ((edgep (from to)
             (eq '< (specializable:specializer<
                     generic-function to from generalizer))))
      (let* ((specializers (generic-function-nth-arg-specializers
                            generic-function argument))
             (all          (remove-if-not (curry #'edgep object) specializers)))
        (remove-if (lambda (specializer)
                     (some (rcurry #'edgep specializer) all))
                   all)))))

;;; Convenience interface

(defun make-specializer-graph (generic-function argument-position argument)
  (let ((generalizer (specializable:generalizer-of-using-class
                      generic-function argument argument-position)))
    (make-instance 'specializer-graph
                   :generic-function generic-function
                   :argument         argument-position
                   :generalizer      generalizer)))

(defun specializer-graph (generic-function argument-position argument output-file
                          &rest args &key &allow-other-keys)
  (let* ((graph        (make-specializer-graph
                        generic-function argument-position argument))
         (specializers (generic-function-nth-arg-specializers
                        (specializer-graph-generic-function graph)
                        argument-position))
         (dot-graph    (cl-dot:generate-graph-from-roots graph specializers)))
    (apply #'cl-dot:dot-graph dot-graph output-file args)))

;;; Test

(specializer-graph #'cons-specializer.example::keyword-args 0 1.0d0
                   "/tmp/specializer-dag-4.png" :format :png)

(specializer-graph #'accept-specializer.example::cn-test 0 "text/plain;q=0.2,text/html;q=0.1"
                   "/tmp/specializer-dag-5.png" :format :png)
