;;;; accept-specializer.lisp --- Specializer for dispatching on content type of requests.
;;;;
;;;; Copyright (C) 2013, 2014 Christophe Rhodes
;;;;
;;;; Author: Christophe Rhodes <csr21@cantab.net>

(cl:in-package #:accept-specializer)

(defstruct accept-node
  (name (error "missing name"))
  (children nil)
  (q nil))
(defun find-child (name node)
  (declare (type string name)
           (type accept-node node))
  (let* ((children (accept-node-children node))
         (position (position name (accept-node-children node)
                             :key #'accept-node-name :test #'string=)))
    (when position
      (values (nth position children) position (length children)))))
(defun append-child (child node)
 (setf (accept-node-children node)
       (append (accept-node-children node) (list child)))
 child)
(defun ensure-child (name node)
  (or (find-child name node)
      (append-child (make-accept-node :name name) node)))
(defun print-accept-tree (tree stream)
  (let (*stack*)
    (declare (special *stack*))
    (labels ((walk (fun node)
               (let ((*stack* (cons node *stack*)))
                 (declare (special *stack*))
                 (mapc (lambda (x) (walk fun x)) (accept-node-children node)))
               (funcall fun node))
             (stringify (node)
               (case (length *stack*)
                 (0 "*/*")
                 (1 (format nil "~A/*" (accept-node-name node)))
                 (2 (format nil "~A/~A" (accept-node-name (car *stack*)) (accept-node-name node))))))
      (let ((first t))
        (walk
         (lambda (x)
           (let ((q (accept-node-q x)))
             (when q
               (format stream "~:[, ~;~]" first)
               (format stream "~A~:[;q=~A~;~]" (stringify x) (= q 1.0) q)
               (setf first nil))))
         tree)))))
(defmethod print-object ((o accept-node) s)
  (if (accept-node-name o)
      (call-next-method)
      (pprint-logical-block (s nil)
        (print-unreadable-object (o s :type t)
          (print-accept-tree o s)))))

(defun q (media-type accept-tree)
  (let* ((pos (position #\/ media-type))
         (type (subseq media-type 0 pos))
         (subtype (subseq media-type (1+ pos))))
    (labels ((find-node (node position base types)
               (multiple-value-bind (child-node child-position child-length)
                   (when types (find-child (first types) node))
                 (if child-node
                     (find-node child-node
                                (+ position (* base (1+ child-position)))
                                (* base (1+ child-length))
                                (rest types))
                     (values node position)))))
      (multiple-value-bind (node position)
          (find-node accept-tree 0 1 (list type subtype))
        (values (accept-node-q node) position)))))

(defun q-ok (media-type accept-tree)
  (let ((q (q media-type accept-tree)))
    (and q (> q 0) q)))

(defun insert (range q tree)
  (labels ((ensure-node (range tree)
             (if (null range)
                 tree
                 (ensure-node
                  (rest range) (ensure-child (first range) tree)))))
    (let ((node (ensure-node range tree)))
      ;; we could choose different behaviour here
      (setf (accept-node-q node) q))
    tree))

(defun parse-accept-string (string)
  (flet ((whitespacep (x)
           (member x '(#\Space #\Tab))))
    (let ((string (remove-if #'whitespacep string))
          (result (make-accept-node :name nil)))
      (cl-ppcre:do-register-groups (type subtype qp q)
          ;; not desperately error-proof
          ("([a-z]*|\\*)/([a-z0-9]*|\\*)(;q=([01]\\.[0-9]*))?(,|$)" string result)
        (if qp
            (setf q (float (+ (digit-char-p (char q 0))
                              (/ (parse-integer q :start 2)
                                 (expt 10 (- (length q) 2))))))
            (setf q 1.0))
        (let ((range (and (string/= type "*")
                          (cons type (and (string/= subtype "*")
                                          (list subtype))))))
          (insert range q result))))))

(defclass accept-specializer (extended-specializer)
  ((media-type :initarg :media-type :type string :reader media-type)))
(defmethod print-object ((o accept-specializer) s)
  (print-unreadable-object (o s :type t)
    (format s "~S" (media-type o))))
;;; FIXME: would be cute to have sb-pcl:generalizer to inherit from.
;;; Or maybe specializable:extended-generalizer could handle the NEXT
;;; functionality?
(defclass accept-generalizer ()
  ((header :initarg :header :reader header)
   (tree)
   (next :initarg :next :reader next)))
(defmethod print-object ((o accept-generalizer) s)
  (print-unreadable-object (o s :type t)
    (print-accept-tree (tree o) s)))
(defmethod tree ((x accept-generalizer))
  (if (slot-boundp x 'tree)
      (slot-value x 'tree)
      (setf (slot-value x 'tree) (parse-accept-string (header x)))))
(defclass accept-generic-function (specializable-generic-function)
  ()
  (:metaclass sb-mop:funcallable-standard-class))

(define-extended-specializer accept (gf arg)
  (declare (ignore gf))
  (make-instance 'accept-specializer :media-type arg))
(defmethod sb-pcl:unparse-specializer-using-class
    ((gf accept-generic-function) (specializer accept-specializer))
  `(accept ,(media-type specializer)))
(defmethod sb-pcl::same-specializer-p
    ((s1 accept-specializer) (s2 accept-specializer))
  (string= (media-type s1) (media-type s2)))


(defmethod generalizer-equal-hash-key
    ((gf accept-generic-function) (g accept-generalizer))
  `(accept-generalizer ,(header g)))
(defmethod sb-pcl:specializer-type-specifier ((specializer accept-specializer))
  't)
(defmethod specializer-accepts-generalizer-p ((gf accept-generic-function) (s accept-specializer) (generalizer accept-generalizer))
  (values (q-ok (media-type s) (tree generalizer)) t))
(defmethod specializer-accepts-generalizer-p ((gf accept-generic-function) (s accept-specializer) generalizer)
  (values nil t))
(defmethod specializer-accepts-generalizer-p ((gf accept-generic-function) (s sb-mop:specializer) (generalizer accept-generalizer))
  (specializer-accepts-generalizer-p gf s (next generalizer)))

(defmethod specializer-accepts-p ((specializer accept-specializer) obj)
  nil)

(defmethod specializer< ((gf accept-generic-function) (s1 accept-specializer) (s2 accept-specializer) generalizer)
  (let ((media-type1 (media-type s1))
        (media-type2 (media-type s2))
        %tree %q1 p1 %q2 p2) ; poor person's lazy evaluation
    (symbol-macrolet
        ((tree (or %tree (setf %tree (tree generalizer))))
         (q1 (or %q1 (setf (values %q1 p1) (q media-type1 tree))))
         (q2 (or %q2 (setf (values %q2 p2) (q media-type2 tree)))))
      (cond
        ((string= media-type1 media-type2) '=)
        ((not (and q1 q2))                 '/=)
        ((< q1 q2)                         '>)
        ((> q1 q2)                         '<)
        ;; Implies different media types but q1 = q2. RFC 2616,
        ;; Section 14.1 (Accept) does not specify a tie breaker for
        ;; media-types with identical q value. We arbitrarily prefer
        ;; one based on the position of the corresponding node in the
        ;; accept tree.
        ((< p1 p2)                         '<)
        ((> p1 p2)                         '>)
        ;; Implies different media types but same node in accept tree
        ;; (e.g. "*/*" or "SOMETHING/*" is the best match for both
        ;; media types). Use media type strings as tiebreaker.
        ((string< media-type1 media-type2) '<)
        (t                                 '>)))))
(defmethod specializer< ((gf accept-generic-function) (s1 accept-specializer) (s2 class) generalizer)
  '<)
(defmethod specializer< ((gf accept-generic-function) (s1 accept-specializer) (s2 sb-mop:eql-specializer) generalizer)
  '>)
(defmethod specializer< ((gf accept-generic-function) (s1 sb-mop:specializer) (s2 accept-specializer) generalizer)
  (invert-specializer<-relation (specializer< gf s2 s1 generalizer)))
(defmethod specializer< ((gf accept-generic-function) (s1 sb-mop:specializer) (s2 sb-mop:specializer) (g accept-generalizer))
  (specializer< gf s1 s2 (next g)))

(defvar *actual-content-type*)
(defgeneric handle-content-type (x))
(define-method-combination content-negotiation ()
  ((around (:around))
   (before (:before))
   (primary () :required t)
   (after (:after)))
  (:arguments request)
  (labels ((call-methods (methods)
             (mapcar #'(lambda (method)
                         `(call-method ,method))
                     methods))
           (transform (primaries)
             (let ((method (car primaries))
                   (nexts (cdr primaries)))
               `(make-method
                 (progn
                   (let ((request-specializer (car (sb-mop:method-specializers ,method))))
                     (when (typep request-specializer 'accept-specializer)
                       (setf *actual-content-type* (media-type request-specializer))))
                   (throw 'content-negotiation (call-method ,method ,@(and nexts `((,(transform nexts))))))))))
           (wrap (form)
             `(let ((*actual-content-type*))
                (multiple-value-prog1
                    ,form
                  (handle-content-type ,request)))))
    (let ((form (if (or before after (rest primary))
                    `(multiple-value-prog1
                         (progn ,@(call-methods before)
                                (catch 'content-negotiation (call-method ,(transform primary))))
                       ,@(call-methods (reverse after)))
                    `(catch 'content-negotiation (call-method ,(transform primary))))))
      (if around
          (wrap `(call-method ,(first around)
                              (,@(rest around) (make-method ,form))))
          (wrap form)))))
(define-method-combination content-negotiation/or ()
  ((around (:around))
   (primary (or) :required t))
  (:arguments request)
  (labels ((transform/1 (method)
             `(make-method
               (progn
                 (let ((s (car (sb-mop:method-specializers ,method))))
                   (when (typep s 'accept-specializer)
                     (setf *actual-content-type* (media-type s))))
                 (call-method ,method))))
           (transform (primaries)
             (mapcar #'(lambda (x) `(call-method ,(transform/1 x)))
                     primaries))
           (wrap (form)
             `(let ((*actual-content-type*))
                (multiple-value-prog1
                    ,form
                  (handle-content-type ,request)))))
    (let ((form (if (rest primary)
                    `(or ,@(transform primary))
                    `(call-method ,(transform/1 (car primary))))))
      (if around
          (wrap `(call-method ,(first around)
                              (,@(rest around) (make-method ,form))))
          (wrap form)))))

(defmethod generalizer-of-using-class ((gf accept-generic-function) (s string))
  (make-instance 'accept-generalizer
                 :header s
                 :next (call-next-method)))
(defmethod specializer-accepts-p ((s accept-specializer) (string string))
  (q-ok (media-type s) (parse-accept-string string)))

(defmethod handle-content-type ((x string))
  (format t "~&Content-Type: ~A" *actual-content-type*))
