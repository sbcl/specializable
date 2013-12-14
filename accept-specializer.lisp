(in-package "SPECIALIZABLE")

(defvar *actual-content-type*)

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
                   (setf *actual-content-type* ,method)
                   (call-method ,method ,@(and nexts `((,(transform nexts)))))))))
           (wrap (form)
             `(let ((*actual-content-type*))
                (multiple-value-prog1
                    ,form
                  (print ,request)))))
    (let ((form (if (or before after (rest primary))
                    `(multiple-value-prog1
                         (progn ,@(call-methods before)
                                (call-method ,(transform primary)))
                       ,@(call-methods (reverse after)))
                    `(call-method ,(transform primary)))))
      (if around
          (wrap `(call-method ,(first around)
                              (,@(rest around) (make-method ,form))))
          (wrap form)))))

(defstruct accept-node
  (name (error "missing name"))
  (children nil)
  (q nil))
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
  (pprint-logical-block (s nil)
    (print-unreadable-object (o s :type t)
      (print-accept-tree o s))))

(defun q (media-type accept-tree)
  (let* ((pos (position #\/ media-type))
         (type (subseq media-type 0 pos))
         (subtype (subseq media-type (1+ pos)))
         (type-node (find type (accept-node-children accept-tree) :key #'accept-node-name :test #'string=))
         (subtype-node (and type-node (find subtype (accept-node-children type-node) :key #'accept-node-name :test #'string=))))
    (or (and subtype-node (accept-node-q subtype-node))
        (and type-node (accept-node-q type-node))
        (accept-node-q accept-tree))))

(defclass accept-generic-function (specializable-generic-function)
  ()
  (:metaclass sb-mop:funcallable-standard-class))




(defgeneric respond (request)
  (:method-combination content-negotiation)
  (:generic-function-class accept-generic-function))
(defmethod respond :after (request)
  (print *actual-content-type*))
(defmethod respond (request)
  t)
