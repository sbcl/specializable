;;;; code-walker.lisp --- TODO.
;;;;
;;;; Copyright (C) 2013, 2014 Christophe Rhodes
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Christophe Rhodes

;;;; Partially based on TODO

(cl:defpackage #:pattern-specializer.examples.code-walker
  (:use
   #:cl
   #:pattern-specializer)

  (:import-from #:specializable
   #:cons-generic-function)

  (:import-from #:optima
   #:guard))

(cl:in-package #:pattern-specializer.examples.code-walker)

(defclass binding ()
  ((used :initform nil :accessor used)))

(defun make-env (bindings env)
  (append bindings env))
(defun find-binding (env var)
  (cdr (assoc var env)))

(defun bindings-from-ll (ll)
  (mapcar (lambda (n) (cons n (make-instance 'binding))) ll))

(define-condition walker-warning (warning)
  ((env :initarg :env :reader env)
   (call-stack :initarg :call-stack :reader call-stack)))
(define-condition unused-variable (walker-warning)
  ((name :initarg :name :reader name)))
(define-condition unbound-variable-referenced (walker-warning)
  ((name :initarg :name :reader name)))

(defmacro with-checked-bindings ((bindings env call-stack) &body body)
  `(let* ((bindings ,bindings)
          (,env (make-env bindings ,env)))
     ,@body
     (dolist (binding bindings)
       (unless (used (cdr binding))
         (warn 'unused-variable :name (car binding)
               :env ,env :call-stack ,call-stack)))))

;;; walk/cons

(defgeneric walk/cons (form env vars)
  (:generic-function-class cons-generic-function))

(defmethod walk/cons ((expr t) env call-stack)
  nil)

(defmethod walk/cons ((expr cons) env call-stack)
  (let ((cs (cons expr call-stack)))
    (when (consp (car expr))
      (walk/cons (car expr) env cs))
    (dolist (e (cdr expr))
      (walk/cons e env (cons e cs)))))

(defmethod walk/cons ((expr symbol) env call-stack)
  (if (constantp expr)
      nil
      (let ((binding (find-binding env expr)))
        (if binding
            (setf (used binding) t)
            (warn 'unbound-variable-referenced :name expr
                  :env env :call-stack call-stack)))))

(defmethod walk/cons ((expr (cons lambda)) env call-stack)
  (let ((lambda-list (cadr expr))
        (body (cddr expr)))
    (with-checked-bindings ((bindings-from-ll lambda-list) env call-stack)
      (dolist (form body)
        (walk/cons form env (cons form call-stack))))))

(defmethod walk/cons ((expr (cons multiple-value-bind)) env call-stack)
  (let ((lambda-list (cadr expr))
        (value-form (caddr expr))
        (body (cdddr expr)))
    (walk/cons value-form env (cons value-form call-stack))
    (with-checked-bindings ((bindings-from-ll lambda-list) env call-stack)
      (dolist (form body)
        (walk/cons form env (cons form call-stack))))))

(defmethod walk/cons ((expr (cons let)) env call-stack)
  (flet ((let-binding (x)
           (walk/cons (cadr x) env (cons (cadr x) call-stack))
           (cons (car x) (make-instance 'binding))))
    (with-checked-bindings ((mapcar #'let-binding (cadr expr)) env call-stack)
      (dolist (form (cddr expr))
        (walk/cons form env (cons form call-stack))))))

;;; walk/pattern

(defun walk-binding-form-body (bindings body env call-stack)
  (with-checked-bindings (bindings env call-stack)
    (dolist (form body)
      (walk/pattern form env (cons form call-stack)))))

(defgeneric walk/pattern (form env vars)
  (:generic-function-class pattern-generic-function))

(defmethod walk/pattern ((expr t) env call-stack)
  nil)

(defmethod walk/pattern ((expr cons) env call-stack)
  (let ((cs (cons expr call-stack)))
    (when (consp (car expr))
      (walk/pattern (car expr) env cs))
    (dolist (e (cdr expr))
      (walk/pattern e env (cons e cs)))))

(defmethod walk/pattern ((expr (pattern (type (and symbol (not (satisfies constantp)))))) env call-stack)
  (let ((binding (find-binding env expr)))
    (if binding
        (setf (used binding) t)
        (warn 'unbound-variable-referenced :name expr
              :env env :call-stack call-stack))))

(defmethod walk/pattern ((expr (pattern (list* 'lambda lambda-list body))) env call-stack)
  (walk-binding-form-body
   (bindings-from-ll lambda-list) body env call-stack))

(defmethod walk/pattern ((expr (pattern (list* 'multiple-value-bind lambda-list value-form body))) env call-stack)
  (walk/pattern value-form env (cons value-form call-stack))
  (walk-binding-form-body
   (bindings-from-ll lambda-list) body env call-stack))

(defmethod walk/pattern ((expr (pattern (list* 'let bindings body))) env call-stack)
  (flet ((let-binding (binding)
           (destructuring-bind (name value) binding
             (walk/pattern value env (cons value call-stack))
             (cons name (make-instance 'binding)))))
    (walk-binding-form-body
     (mapcar #'let-binding bindings) body env call-stack)))

;;; walk/case

(defun walk/case (expr env call-stack)
  (typecase expr
    (symbol
     (if (constantp expr)
         nil
         (let ((binding (find-binding env expr)))
           (if binding
               (setf (used binding) t)
               (warn 'unbound-variable-referenced :name expr
                     :env env :call-stack call-stack)))))
    ((cons (eql lambda))
     (let ((lambda-list (cadr expr))
           (body (cddr expr)))
       (with-checked-bindings ((bindings-from-ll lambda-list) env call-stack)
         (dolist (form body)
           (walk/case form env (cons form call-stack))))))
    ((cons (eql multiple-value-bind))
     (let ((lambda-list (cadr expr))
           (value-form (caddr expr))
           (body (cdddr expr)))
       (walk/case value-form env (cons value-form call-stack))
       (with-checked-bindings ((bindings-from-ll lambda-list) env call-stack)
         (dolist (form body)
           (walk/case form env (cons form call-stack))))))
    ((cons (eql macrolet)))
    ((cons (eql flet)))
    ((cons (eql labels)))
    ((cons (eql symbol-macrolet)))
    ((cons (eql if)))
    ((cons (eql progn)))
    ((cons (eql tagbody)))
    ((cons (eql return-from)))
    ((cons (eql multiple-value-call)))
    ((cons (eql block)))
    ((cons (eql catch)))
    ((cons (eql throw)))
    ((cons (eql let))
     (with-checked-bindings ((mapcar (lambda (x) (walk/case (cadr x) env (cons (cadr x) call-stack)) (cons (car x) (make-instance 'binding))) (cadr expr)) env call-stack)
       (dolist (form (cddr expr))
         (walk/case form env (cons form call-stack)))))
    (cons
     (let ((cs (cons expr call-stack)))
       (when (consp (car expr))
         (walk/case (car expr) env cs))
       (dolist (e (cdr expr))
         (walk/case e env (cons e cs)))))
    (t)))

;;; walk/meth

(defgeneric walk/meth (expr env call-stack))

(defmethod walk/meth ((expr symbol) env call-stack)
  (if (constantp expr)
      nil
      (let ((binding (find-binding env expr)))
        (if binding
            (setf (used binding) t)
            (warn 'unbound-variable-referenced :name expr
                  :env env :call-stack call-stack)))))
(defmethod walk/meth ((expr t) env call-stack)
  nil)

(defmethod walk/meth ((expr cons) env call-stack)
  (typecase expr
    ((cons (eql lambda))
     (let ((lambda-list (cadr expr))
           (body (cddr expr)))
       (with-checked-bindings ((bindings-from-ll lambda-list) env call-stack)
         (dolist (form body)
           (walk/meth form env (cons form call-stack))))))
    ((cons (eql multiple-value-bind))
     (let ((lambda-list (cadr expr))
           (value-form (caddr expr))
           (body (cdddr expr)))
       (walk/meth value-form env (cons value-form call-stack))
       (with-checked-bindings ((bindings-from-ll lambda-list) env call-stack)
         (dolist (form body)
           (walk/meth form env (cons form call-stack))))))
    ((cons (eql macrolet)))
    ((cons (eql flet)))
    ((cons (eql labels)))
    ((cons (eql symbol-macrolet)))
    ((cons (eql if)))
    ((cons (eql progn)))
    ((cons (eql tagbody)))
    ((cons (eql return-from)))
    ((cons (eql multiple-value-call)))
    ((cons (eql block)))
    ((cons (eql catch)))
    ((cons (eql throw)))
    ((cons (eql let))
     (with-checked-bindings ((mapcar (lambda (x) (walk/meth (cadr x) env (cons (cadr x) call-stack)) (cons (car x) (make-instance 'binding))) (cadr expr)) env call-stack)
       (dolist (form (cddr expr))
         (walk/meth form env (cons form call-stack)))))
    (t
     (let ((cs (cons expr call-stack)))
       (when (consp (car expr))
         (walk/meth (car expr) env cs))
       (dolist (e (cdr expr))
         (walk/meth e env (cons e cs)))))))
