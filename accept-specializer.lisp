(in-package "SPECIALIZABLE")

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
  (if (accept-node-name o)
      (call-next-method)
      (pprint-logical-block (s nil)
        (print-unreadable-object (o s :type t)
          (print-accept-tree o s)))))

(defun q (media-type accept-tree)
  (let* ((pos (position #\/ media-type))
         (type (subseq media-type 0 pos))
         (subtype (subseq media-type (1+ pos)))
         (type-node (find type (accept-node-children accept-tree) :key #'accept-node-name :test #'string=))
         (subtype-node (and type-node (find subtype (accept-node-children type-node) :key #'accept-node-name :test #'string=))))
    (or (and subtype-node (accept-node-q subtype-node))
        (and type-node (accept-node-q type-node))
        (accept-node-q accept-tree))))

(defun insert (range q tree)
  (labels ((ensure-node (range tree)
             (cond
               ((null range) tree)
               (t (ensure-node (cdr range)
                               (or (find (car range) (accept-node-children tree)
                                         :key #'accept-node-name :test #'string=)
                                   (car (push
                                         (make-accept-node :name (car range))
                                         (accept-node-children tree)))))))))
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
          ("([a-z]*|\\*)/([a-z]*|\\*)(;q=([01]\\.[0-9]*))?(,|$)" string result)
        (if qp
            (setf q (float (+ (digit-char-p (char q 0))
                              (/ (parse-integer q :start 2)
                                 (expt 10 (- (length q) 2))))))
            (setf q 1.0))
        (let ((range (and (string/= type "*")
                          (cons type (and (string/= subtype "*")
                                          (list subtype))))))
          (insert range q result))))))

;;; FIXME: tiebreaker predicate (maybe defaulting to string<)?
(defclass accept-specializer (extended-specializer)
  ((media-type :initarg :media-type :reader media-type)))
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
  (make-instance 'accept-specializer :media-type arg))
(defmethod sb-pcl:unparse-specializer-using-class
    ((gf accept-generic-function) (specializer accept-specializer))
  `(accept ,(media-type specializer)))
(defmethod sb-pcl::same-specializer-p
    ((s1 accept-specializer) (s2 accept-specializer))
  (string= (media-type s1) (media-type s2)))

(defmethod generalizer-of-using-class ((gf accept-generic-function) (arg tbnl:request))
  (make-instance 'accept-generalizer
                 :header (tbnl:header-in :accept arg)
                 :next (class-of arg)))
(defmethod generalizer-equal-hash-key
    ((gf accept-generic-function) (g accept-generalizer))
  `(accept-generalizer ,(header g)))
(defmethod specializer-accepts-generalizer-p ((gf accept-generic-function) (s accept-specializer) (generalizer accept-generalizer))
  (values (q (media-type s) (tree generalizer)) t))
(defmethod specializer-accepts-generalizer-p ((gf accept-generic-function) (s accept-specializer) generalizer)
  (values nil t))
(defmethod specializer-accepts-generalizer-p ((gf accept-generic-function) (s sb-mop:specializer) (generalizer accept-generalizer))
  (specializer-accepts-generalizer-p gf s (next generalizer)))

(defmethod specializer-accepts-p ((specializer accept-specializer) obj)
  nil)
(defmethod specializer-accepts-p ((specializer accept-specializer) (obj tbnl:request))
  (q (media-type specializer) (parse-accept-string (tbnl:header-in :accept obj))))

(defmethod specializer< ((gf accept-generic-function) (s1 accept-specializer) (s2 accept-specializer) generalizer)
  (cond
    ((string= (media-type s1) (media-type s2)) '=)
    (t (let ((q1 (q (media-type s1) (tree generalizer)))
             (q2 (q (media-type s2) (tree generalizer))))
         (cond
           ((= q1 q2) '=)
           ((< q1 q2) '>)
           (t '<))))))
(defmethod specializer< ((gf accept-generic-function) (s1 accept-specializer) (s2 class) generalizer)
  '<)
(defmethod specializer< ((gf accept-generic-function) (s1 accept-specializer) (s2 sb-mop:eql-specializer) generalizer)
  '>)
(defmethod specializer< ((gf accept-generic-function) (s1 sb-mop:specializer) (s2 accept-specializer) generalizer)
  (ecase (specializer< gf s2 s1 generalizer)
    ((>) '<)
    ((<) '>)))
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
                   (call-method ,method ,@(and nexts `((,(transform nexts)))))))))
           (wrap (form)
             `(let ((*actual-content-type*))
                (multiple-value-prog1
                    ,form
                  (handle-content-type ,request)))))
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

(defmethod generalizer-of-using-class ((gf accept-generic-function) (s string))
  (make-instance 'accept-generalizer
                 :header s
                 :next (class-of s)))
(defmethod specializer-accepts-p ((s accept-specializer) (string string))
  (q (media-type s) (parse-accept-string string)))

(defmethod handle-content-type ((x tbnl:request))
  (setf (tbnl:content-type*) *actual-content-type*))
(defmethod handle-content-type ((x string))
  (format t "~&Content-Type: ~A" *actual-content-type*))

(defgeneric respond (request)
  (:generic-function-class accept-generic-function)
  (:method-combination list))
(defmethod respond list (request)
  t)
(defmethod respond list ((s string))
  'string)
(defmethod respond list ((s (accept "text/html")))
  "text/html")
(defmethod respond list ((s (accept "audio/mp3")))
  "audio/mp3")

