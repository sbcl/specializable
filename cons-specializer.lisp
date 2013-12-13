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
(defmethod specializer-accepts-generalizer-p ((gf cons-generic-function) (specializer cons-specializer) thing)
  (if (eql (%car specializer) thing)
      (values t t)
      (values nil t)))
(defmethod specializer-accepts-generalizer-p ((gf cons-generic-function) (specializer sb-mop:specializer) (thing symbol))
  (specializer-accepts-generalizer-p gf specializer (find-class 'cons)))

;;; note: this method operates in full knowledge of the object, and so
;;; does not require the generic function as an argument.
(defmethod specializer-accepts-p ((specializer cons-specializer) obj)
  (and (consp obj)
       (eql (car obj) (%car specializer))))

(defmethod specializer< ((gf cons-generic-function) (s1 cons-specializer) (s2 cons-specializer) generalizer)
  (declare (ignore generalizer))
  (if (eql (%car s1) (%car s2))
      '=
      nil))
(defmethod specializer< ((gf cons-generic-function) (s1 cons-specializer) (s2 class) generalizer)
  (declare (ignore generalizer))
  '<)
(defmethod specializer< ((gf cons-generic-function) (s1 cons-specializer) (s2 sb-mop:eql-specializer) generalizer)
  (declare (ignore generalizer))
  '>)
(defmethod specializer< ((gf cons-generic-function) (s1 sb-mop:specializer) (s2 cons-specializer) generalizer)
  (ecase (specializer< gf s2 s1 generalizer)
    ((<) '>)
    ((>) '<)))
;;; note: the need for this method is tricky: we need to translate
;;; from generalizers that our specializers "know" about to those that
;;; ordinary generic functions and specializers might know about.
(defmethod specializer< ((gf cons-generic-function) (s1 sb-mop:specializer) (s2 sb-mop:specializer) (generalizer symbol))
  (specializer< gf s1 s2 (find-class 'cons)))

;;; tests / examples
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
       `(with-bindings ,bindings ,@(mapcar #'walk (cddr form)))))

   (assert (equal (walk t) '(lookup t)))
   (assert (equal (walk nil) '(lookup nil)))
   (assert (equal (walk '(foo bar)) '(call (flookup foo) (list (lookup bar)))))
   (assert (equal (walk '(quote bar)) 'bar))
   (assert (equal (walk '(let foo bar)) '(with-bindings foo (lookup bar))))))

(eval
 '(progn
   (defgeneric multiple-class-specializers (x)
     (:generic-function-class cons-generic-function)
     (:method-combination list))
   (defmethod multiple-class-specializers list ((x t)) 't)
   (defmethod multiple-class-specializers list ((x cons)) 'cons)
   (defmethod multiple-class-specializers list ((x (cons foo))) '(cons foo))
   (defmethod multiple-class-specializers list ((x (cons bar))) '(cons bar))
   (defmethod multiple-class-specializers list ((x list)) 'list)
   (defmethod multiple-class-specializers list ((x null)) 'null)
   (defmethod multiple-class-specializers list ((x (eql nil))) '(eql nil))

   (assert (equal (multiple-class-specializers nil) '((eql nil) null list t)))
   (assert (equal (multiple-class-specializers t) '(t)))
   (assert (equal (multiple-class-specializers (cons nil nil)) '(cons list t)))
   (assert (equal (multiple-class-specializers (cons 'foo nil)) '((cons foo) cons list t)))
   (assert (equal (multiple-class-specializers (list 'bar nil t 3)) '((cons bar) cons list t)))))
