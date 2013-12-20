;;; written by David Lichteblau, based on code by Christophe Rhodes,
;;; Closette, and SBCL
;;;
;;; http://www.lichteblau.com/git/?p=specializable.git;a=blob_plain;f=specializable.lisp;hb=eb30d235951c3c1d128811278760f1db36cd336c

(defpackage "SPECIALIZABLE"
  (:use "CL" "SB-EXT")
  (:export "SPECIALIZABLE-GENERIC-FUNCTION" "SPECIALIZABLE-METHOD"
           "EXTENDED-SPECIALIZER"

           "SPECIALIZER-ACCEPTS-P" "SPECIALIZER-ACCEPTS-GENERALIZER-P"
           "SPECIALIZER<"

           "GENERALIZER-OF-USING-CLASS"
           "COMPUTE-APPLICABLE-METHODS-USING-GENERALIZERS"
           "GENERALIZER-EQUAL-HASH-KEY"
           
           "DEFINE-EXTENDED-SPECIALIZER"))

(in-package "SPECIALIZABLE")

(defclass extended-specializer (sb-mop:specializer)
  ((direct-methods-table :allocation :class
                         :initform nil :accessor direct-methods-table)))

(defmethod sb-mop:add-direct-method ((specializer extended-specializer) method)
  (let* ((table (direct-methods-table specializer))
         (cell (assoc specializer table :test #'sb-pcl::same-specializer-p)))
    (unless cell
      (setf cell (cons specializer nil))
      (push cell (direct-methods-table specializer)))
    (push method (cdr cell))))

(defmethod sb-mop:remove-direct-method ((specializer extended-specializer) method)
  (let* ((table (direct-methods-table specializer))
         (cell (assoc specializer table :test #'sb-pcl::same-specializer-p)))
    (setf (cdr cell) (remove method (cdr cell)))))

(defmethod sb-mop:specializer-direct-methods ((specializer extended-specializer))
  (cdr (assoc specializer (direct-methods-table specializer)
              :test #'sb-pcl::same-specializer-p)))
(defmethod sb-mop:specializer-direct-generic-functions ((specializer extended-specializer))
  (remove-duplicates (mapcar #'sb-mop:method-generic-function (sb-mop:specializer-direct-methods specializer))))

(defclass specializable-generic-function (standard-generic-function)
  ((extended-specializers :initform (make-hash-table :test 'equal)
                          :reader generic-function-extended-specializers)
   (emf-table :initform (make-hash-table :test 'equal) :reader emf-table))
  (:metaclass sb-mop:funcallable-standard-class)
  (:default-initargs :method-class (find-class 'specializable-method)))

(defclass specializable-method (standard-method)
  ((lambda-expression :initarg :lambda-expression
		      :accessor specializable-method-lambda-expression)))

(defmacro define-extended-specializer (name (gf-var &rest args) &body body)
  ;; FIXME: unparser
  `(setf (get ',name 'extended-specializer-parser)
	 (lambda (,gf-var ,@args)
	   ,@body)))

;; doesn't work, because we'd have to dump GF into the fasl for the macro
;; expansion
;;; (defun intern-extended-specializer (gf sname)
;;;   (destructuring-bind (kind &rest args) sname
;;;     (setf (gethash sname (generic-function-extended-specializers gf))
;;; 	  (apply (or (get kind 'extended-specializer-parser)
;;; 		     (error "not declared as an extended specializer name: ~A"
;;; 			    kind))
;;; 		 gf
;;; 		 args))))

;;; FIXME: this (and add/remove-direct-method) don't actually work
;;; together, because two distinct calls to make-extended-specializer
;;; return two distinct specializer objects.  We need either to make
;;; the extended specializers be interned, or to have them be
;;; arbitrarily ephemeral but adjust specializer-direct-methods (and
;;; implement specializer-direct-generic-functions) accordingly.
(defun make-extended-specializer (sname)
  (destructuring-bind (kind &rest args) sname
    (apply (or (get kind 'extended-specializer-parser)
	       (error "not declared as an extended specializer name: ~A"
		      kind))
	   '|This is not a generic function| ;fixme, see comment above
	   args)))

;;; from SBCL:

(defmethod sb-pcl:parse-specializer-using-class
    ((gf specializable-generic-function) name)
  (cond
    ((typep name 'sb-mop:specializer) name)
    ((symbolp name) (find-class name))
    ((consp name)
     (case (car name)
       (eql (sb-mop:intern-eql-specializer (cadr name)))
       (t (make-extended-specializer name))))
    (t (error "unexpected specializer name"))))

(defmethod sb-pcl:make-method-specializers-form
    ((gf specializable-generic-function) method snames env)
  (declare (ignore method env))
  (flet ((parse (name)
           (cond
             ((typep name 'sb-mop:specializer) name)
             ((symbolp name) `(find-class ',name))
             ((consp name)
	      (case (car name)
		(eql `(sb-mop:intern-eql-specializer ,(cadr name)))
		(t `(make-extended-specializer ',name))))
             (t (error "unexpected specializer name")))))
    `(list ,@(mapcar #'parse snames))))

;;; from Closette, changed to use some SBCL functions:

;;; FIXME: this is not actually sufficient argument checking
(defun required-portion (gf args)
  (let ((number-required
	 (sb-pcl::arg-info-number-required (sb-pcl::gf-arg-info gf))))
    (when (< (length args) number-required)
      (error "Too few arguments to generic function ~S." gf))
    (subseq args 0 number-required)))

(defgeneric generalizer-equal-hash-key (generic-function generalizer))
(defmethod generalizer-equal-hash-key
    ((gf specializable-generic-function) (g class))
  (sb-pcl::class-wrapper g))

;;; FIXME: in some kind of order, the discriminating function needs to handle:
;;; - argument count checking;
;;; - keyword argument validity;
;;; - DONE flushing the emf cache on method addition/removal
;;; - DONE (sort of, using wrappers/g-e-h-k) flushing the cache on class redefinition;
;;; - cache thread-safety.
(defmethod sb-mop:compute-discriminating-function ((gf specializable-generic-function))
  (lambda (&rest args)
    (let* ((generalizers (mapcar (lambda (x) (generalizer-of-using-class gf x))
                                 (required-portion gf args)))
           (keys (mapcar (lambda (x) (generalizer-equal-hash-key gf x)) generalizers))
	   (emfun (gethash keys (emf-table gf) nil)))
      (if emfun
	  (sb-pcl::invoke-emf emfun args)
	  (slow-method-lookup gf args generalizers)))))

(defmethod reinitialize-instance :after ((gf specializable-generic-function) &key)
  (clrhash (emf-table gf)))

(defun slow-method-lookup (gf args generalizers)
  ;; differs from closette
  (multiple-value-bind (applicable-methods definitivep)
      (compute-applicable-methods-using-generalizers gf generalizers)
    (if definitivep
	(let* ((emfun
		(compute-effective-method-function gf applicable-methods))
               (keys (mapcar (lambda (x) (generalizer-equal-hash-key gf x)) generalizers)))
	  (setf (gethash keys (emf-table gf)) emfun)
	  (sb-pcl::invoke-emf emfun args))
	(sb-pcl::invoke-emf (compute-effective-method-function
                             gf (sb-mop:compute-applicable-methods gf args))
                            args))))

(defun compute-effective-method-function (gf methods)
  (let* ((mc (sb-mop:generic-function-method-combination gf))
         (em (sb-mop:compute-effective-method gf mc methods)))
    (sb-pcl::make-effective-method-function gf em)))

;; new, not in closette
(defgeneric generalizer-of-using-class (generic-function object))
(defmethod generalizer-of-using-class ((generic-function specializable-generic-function) object)
  (class-of object))

(defgeneric specializer-accepts-generalizer-p (gf specializer generalizer))
(defmethod specializer-accepts-generalizer-p
    ((gf specializable-generic-function) (specializer class) (generalizer class))
  (if (subtypep generalizer specializer)
      (values t t)
      (values nil t)))
(defmethod specializer-accepts-generalizer-p
    ((gf specializable-generic-function) (specializer sb-mop:eql-specializer) (generalizer class))
  (if (eq generalizer (class-of (sb-mop:eql-specializer-object specializer)))
      (values t nil)
      (values nil t)))

(defgeneric compute-applicable-methods-using-generalizers (gf generalizers))
(defmethod compute-applicable-methods-using-generalizers
    ((gf specializable-generic-function) generalizers)
  ;; differs from closette
  (let ((result-definitive-p t))
    (flet ((filter (method)
             (every (lambda (s g)
                      (multiple-value-bind (acceptsp definitivep)
                          (specializer-accepts-generalizer-p gf s g)
                        (unless definitivep
                          (setf result-definitive-p nil))
                        acceptsp))
                    (sb-mop:method-specializers method) generalizers))
           (sorter (m1 m2)
             (method-more-specific-p gf m1 m2 generalizers)))
      (values
       (sort
        (copy-list (remove-if-not #'filter (sb-mop:generic-function-methods gf)))
        #'sorter)
       result-definitive-p))))

;; new, not in closette
(defgeneric specializer-accepts-p (specializer object))
(defmethod specializer-accepts-p ((specializer class) object)
  (typep object specializer))
(defmethod specializer-accepts-p ((specializer sb-mop:eql-specializer) object)
  (eq object (sb-mop:eql-specializer-object specializer)))

(defmethod compute-applicable-methods
    ((gf specializable-generic-function) arguments)
  ;; new, not in closette
  (sort
   (copy-list
    (remove-if-not #'(lambda (method)
		       (every #'specializer-accepts-p
			      (sb-mop:method-specializers method)
			      arguments))
		   (sb-mop:generic-function-methods gf)))
   (let ((generalizers (mapcar (lambda (x) (generalizer-of-using-class gf x))
                               (required-portion gf arguments))))
     (lambda (m1 m2)
       (method-more-specific-p gf m1 m2 generalizers)))))

(defun method-more-specific-p (gf method1 method2 generalizers)
  ;; FIXME: argument precedence order
  (block nil
    (mapc #'(lambda (spec1 spec2 generalizer)
	      (ecase (specializer< gf spec1 spec2 generalizer)
		(< (return t))
		(=)
		((nil > /=) (return nil))))
	  (sb-mop:method-specializers method1)
	  (sb-mop:method-specializers method2)
	  generalizers)
    nil))

;; new, not in closette
(defgeneric specializer< (gf s1 s2 generalizer))
(defmethod specializer<
    ((gf specializable-generic-function) (s1 class) (s2 class) (generalizer class))
  (if (eq s1 s2)
      '=
      (let ((cpl (sb-mop:class-precedence-list generalizer)))
	(if (find s2 (cdr (member s1 cpl)))
	    '<
            '>))))
(defmethod specializer<
    ((gf specializable-generic-function) (s1 sb-mop:eql-specializer) (s2 sb-mop:eql-specializer) generalizer)
  (declare (ignore generalizer))
  (if (eq (sb-mop:eql-specializer-object s1) (sb-mop:eql-specializer-object s2))
      '=
      nil))
(defmethod specializer< ((gf specializable-generic-function) (s1 sb-mop:eql-specializer) (s2 class) generalizer)
  (declare (ignore generalizer))
  '<)
(defmethod specializer< ((gf specializable-generic-function) (c1 class) (c2 sb-mop:eql-specializer) generalizer)
  (declare (ignore generalizer))
  '>)
