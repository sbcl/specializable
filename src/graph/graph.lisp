;;;; graph.lisp --- Draw graphs of methods and specializers.
;;;;
;;;; Copyright (C) 2014, 2015, 2016, Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:specializable.graph)

;;; Utilities

(defun generic-function-nth-arg-specializers (generic-function arg)
  (let ((selector (curry #'nth arg)))
    (mapcar (compose selector #'sb-mop:method-specializers)
            (sb-mop:generic-function-methods generic-function))))

;;; Specializer labels

(defmethod specializer-html-label ((graph t) (specializer sb-pcl:specializer))
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

(defmethod specializer-graph-specializers ((graph specializer-graph))
  (generic-function-nth-arg-specializers
   (specializer-graph-generic-function graph)
   (specializer-graph-argument-position graph)))

(defmethod make-specializer-graph ((generic-function  specializable:specializable-generic-function)
                                   (argument-position integer)
                                   (argument          t))
  (let ((generalizer (specializable:generalizer-of-using-class
                      generic-function argument argument-position)))
    (make-instance 'specializer-graph
                   :generic-function  generic-function
                   :argument-position argument-position
                   :argument          argument
                   :generalizer       generalizer)))

;;; Graph object protocol implementation

(defmethod cl-dot:graph-object-node ((graph  specializer-graph)
                                     (object sb-pcl:specializer))
  (with-accessors ((generic-function specializer-graph-generic-function)
                   (argument         specializer-graph-argument))
      graph
    (let ((acceptsp (specializable:specializer-accepts-p
                     object argument))
          (label    (specializer-html-label graph object)))
      (make-instance 'cl-dot:node
                     :attributes (list :label     `(:html () ,label)
                                       :style     :filled
                                       :fillcolor (if acceptsp
                                                      "white"
                                                      "lightgrey"))))))

(defun transitive-closure (direction start all generic-function generalizer
                           &key (min-distance 1) max-distance)
  (let ((direction (ensure-list direction)))
    (labels ((connectedp (direction left right)
               (member (specializable:specializer<
                        generic-function left right generalizer)
                       direction))
             (directly-connected-p (direction node all)
               (let ((candidates (remove-if-not
                                  (curry #'connectedp direction node) all)))
                 (remove-if (lambda (node)
                              (some (lambda (other)
                                      (and (not (eq other node))
                                           (connectedp direction other node)))
                                    candidates))
                            candidates))))
      (let ((seen   (make-hash-table :test #'eq))
            (result ()))
        (labels ((add-closure (node &optional (distance 0))
                   (unless (or (and max-distance (> distance max-distance))
                               (gethash node seen))
                     (setf (gethash node seen) t)
                     (when (member '< direction)
                       (mapc (rcurry #'add-closure (1+ distance))
                             (directly-connected-p '(<) node all)))
                     (when (>= distance (or min-distance 0))
                       (push node result))
                     (when (member '> direction)
                       (mapc (rcurry #'add-closure (1+ distance))
                             (directly-connected-p '(>) node all)))
                     (when-let ((other (set-difference direction '(< >)) ))
                       (mapc (rcurry #'add-closure (1+ distance))
                             (directly-connected-p other node all))))))
          (add-closure start))
        result))))

(defmethod cl-dot:graph-object-points-to ((graph  specializer-graph)
                                          (object sb-pcl:specializer))
  (with-accessors ((generic-function  specializer-graph-generic-function)
                   (argument-position specializer-graph-argument-position)
                   (generalizer       specializer-graph-generalizer)
                   (specializers      specializer-graph-specializers))
      graph
    (labels ((attributed (target color label)
               (make-instance 'cl-dot:attributed
                              :object     target
                              :attributes `(:label     ,label
                                            :color     ,color
                                            :weight    0
                                            :arrowhead :none)))
             (relation (relation color label)
               (let* ((cluster   (transitive-closure '(< >) object specializers generic-function generalizer
                                                   :min-distance 2))
                      (neighbors (transitive-closure relation object cluster generic-function generalizer
                                                     :max-distance 1)))
                 (mapcar (rcurry #'attributed color label) neighbors)))
             (ordered ()
               (transitive-closure '> object specializers generic-function generalizer
                                   :max-distance 1)))
      (append (ordered)
              #+no (relation '// "green")     ; disjoint
              (relation '/= "red"    "/=")    ; distinct
              (relation '=  "orange" "="))))) ; equal

;;; Convenience interface

(defun specializer-graph-title (graph)
  (let ((*print-right-margin* most-positive-fixnum))
    (format nil "~A ~:R arg = ~A"
            (specializer-graph-generic-function graph)
            (specializer-graph-argument-position graph)
            (specializer-graph-argument graph))))

(defun specializer-graph (generic-function argument-position argument output-file
                          &rest args &key title &allow-other-keys)
  (let* ((graph        (make-specializer-graph
                        generic-function argument-position argument))
         (title        (or title (specializer-graph-title graph)))
         (specializers (generic-function-nth-arg-specializers
                        (specializer-graph-generic-function graph)
                        argument-position))
         (dot-graph    (cl-dot:generate-graph-from-roots
                        graph specializers
                        (list :label title))))
    (apply #'cl-dot:dot-graph dot-graph output-file
           (remove-from-plist args :title))))
