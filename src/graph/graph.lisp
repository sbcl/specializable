(cl:in-package #:specializable.graph)

;;; Utilities

(defun generic-function-nth-arg-specializers (generic-function arg)
  (let ((selector (curry #'nth arg)))
    (mapcar (compose selector #'sb-mop:method-specializers)
            (sb-mop:generic-function-methods generic-function))))

;;;

(defgeneric specializer-html-string (graph specializer))

(defmethod specializer-html-string ((graph t) (specializer sb-pcl:specializer))
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

;;;

(defmethod cl-dot:graph-object-node ((graph  specializer-graph)
                                     (object sb-pcl:specializer))
  (with-accessors ((generic-function specializer-graph-generic-function)
                   (argument         specializer-graph-argument)) graph
    (let ((acceptsp (specializable:specializer-accepts-p
                     object argument))
          (string   (specializer-html-string graph object)))
      (make-instance 'cl-dot:node
                     :attributes (list :label     `(:html () ,string)
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
                                      (connectedp direction other node))
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
                   (specializers      specializer-graph-specializers)) graph
    (labels ((ordered ()
               (transitive-closure '> object specializers generic-function generalizer
                                   :max-distance 1))
             (disjoint ()
               (let* ((all (transitive-closure '(< >) object specializers generic-function generalizer
                                               :min-distance 2))
                      (result (transitive-closure '// object all generic-function generalizer
                                                  :max-distance 1)))
                 (mapcar (lambda (target)
                           (make-instance 'cl-dot:attributed
                                          :object target
                                          :attributes `(:color "green"
                                                        :weight    0
                                                        :arrowhead :none)))
                         result)))
             (distinct ()
               (let* ((all (transitive-closure '(< >) object specializers generic-function generalizer
                                               :min-distance 2))
                      (result (transitive-closure '/= object all generic-function generalizer
                                                  :max-distance 1)))
                 (mapcar (lambda (target)
                           (make-instance 'cl-dot:attributed
                                          :object target
                                          :attributes `(:color "red"
                                                        :weight    0
                                                        :arrowhead :none)))
                         result))))
      (append (ordered) #+no (disjoint) (distinct)))))

;;;

(defgeneric make-specializer-graph (generic-function argument-position argument))

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

;;; Convenience interface

(defun specializer-graph (generic-function argument-position argument output-file
                          &rest args &key &allow-other-keys)
  (let* ((graph        (make-specializer-graph
                        generic-function argument-position argument))
         (specializers (specializer-graph-specializers graph))
         (dot-graph    (cl-dot:generate-graph-from-roots
                        graph specializers
                        (list :label (let ((*print-right-margin* most-positive-fixnum))
                                       (format nil "~A ~:R arg = ~A"
                                               (specializer-graph-generic-function graph)
                                               (specializer-graph-argument-position graph)
                                               (specializer-graph-argument graph)))))))
    (apply #'cl-dot:dot-graph dot-graph output-file args)))

;;; Test

;; (specializer-graph #'cons-specializer.example::keyword-args 0 1.0d0
;;                    "/tmp/specializer-dag-4.png" :format :png)
;;
;; (specializer-graph #'accept-specializer.example::cn-test 0 "text/plain;q=0.2,text/html;q=0.1"
;;                    "/tmp/specializer-dag-5.png" :format :png)
;;
;; (specializer-graph #'pattern-specializer.examples.lambda-calculus::eval1 0 1
;;                    "/tmp/specializer-dag-6.png" :format :png)
;;

;; (specializer-graph #'pattern-specializer.test::class-pattern 0 1
;;                    "/tmp/specializer-dag-2-a.png" :format :png)
;;
;; (specializer-graph #'pattern-specializer.test::next-method.1 0 1
;;                    "/tmp/specializer-dag-7-a.png" :format :png)
;; (specializer-graph #'pattern-specializer.test::next-method.1 0 (cons 1 2)
;;                    "/tmp/specializer-dag-7-b.png" :format :png)
;; (specializer-graph #'pattern-specializer.test::next-method.1 0 (cons 1 nil)
;;                    "/tmp/specializer-dag-7-c.png" :format :png)


(specializer-graph #'pattern-specializer.examples.simplifier::simplify 0 '(+ 1 x)
                   "/tmp/specializer-dag-8-a.png" :format :png)

;; (pattern-specializer::generic-function-generalizer-makers #'pattern-specializer.test::christophe.1)
;; (specializer-graph #'pattern-specializer.test::christophe.1 0 (cons 0 1)
;;                    "/tmp/specializer-dag-9-a.png" :format :png)
;;
;; (pattern-specializer::generic-function-generalizer-makers #'pattern-specializer.test::christophe.2)
;; (specializer-graph #'pattern-specializer.test::christophe.2 0 (cons 0 1)
;;                    "/tmp/specializer-dag-10-a.png" :format :png)
