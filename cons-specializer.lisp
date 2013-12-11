(in-package "SPECIALIZABLE")

;;;; CONS-SPECIALIZER example
(defclass cons-specializer (extended-specializer)
  ((car :initarg :car :reader %car)))
(defclass cons-generic-function (specializable-generic-function)
  ()
  (:metaclass sb-mop:funcallable-standard-class))

(define-extended-specializer cons (gf car)
  (make-instance 'cons-specializer :car car))

(defmethod sb-pcl:unparse-specializer-using-class
    ((gf cons-generic-function) (specializer cons-specializer))
  `(cons ,(%car specializer)))
(defmethod sb-pcl::same-specializer-p
    ((s1 cons-specializer) (s2 cons-specializer))
  (eql (%car s1) (%car s2)))

(defmethod generalizer-of-using-class ((gf cons-generic-function) arg)
  (typecase arg
    ((cons symbol) (car arg))
    (t (call-next-method))))
;;; FIXME: it looks like these protocol functions should have the GF
;;; as an argument, since generalizer-of-using-class does
(defmethod specializer-accepts-generalizer-p ((specializer cons-specializer) thing)
  (if (eql (%car specializer) thing)
      (values t t)
      (values nil t)))
;;; FIXME: yes, definitely need the gf!
(defmethod specializer-accepts-generalizer-p (specializer (thing symbol))
  (specializer-accepts-generalizer-p specializer (find-class 'cons)))

;;; this one might not need the GF
(defmethod specializer-accepts-p ((specializer cons-specializer) obj)
  (and (consp obj)
       (eql (car obj) (%car specializer))))
;;; but this one does: it doesn't look like it here, but at issue is
;;; who is responsible for the SPECIALIZER< method for two distinct
;;; user-defined specializers.  Also consider a symbol generalizer
;;; being used to compare two class specializers.
(defmethod specializer< ((s1 cons-specializer) (s2 cons-specializer) generalizer)
  (declare (ignore generalizer))
  (if (eql (%car s1) (%car s2))
      '=
      nil))
(defmethod specializer< ((s1 cons-specializer) (s2 class) generalizer)
  (declare (ignore generalizer))
  '<)
(defmethod specializer< ((s1 cons-specializer) (s2 sb-mop:eql-specializer) generalizer)
  (declare (ignore generalizer))
  '>)
(defmethod specializer< ((s1 sb-mop:specializer) (s2 cons-specializer) generalizer)
  (ecase (specializer< s2 s1 generalizer)
    ((<) '>)
    ((>) '<)))

(eval
 '(progn
   (defgeneric walk (form)
     (:generic-function-class cons-generic-function))
   (defmethod walk ((form symbol))
     `(lookup ,form))
   (defmethod walk ((form cons))
     `(call (flookup ,(car form)) (list ,@(mapcar #'walk (cdr form)))))
   (defmethod walk ((form (cons quote)))
     (cadr form))
   (defmethod walk ((form (cons let)))
     (let ((bindings (cadr form)))
       `(with-bindings ,bindings ,@(cddr form))))))
