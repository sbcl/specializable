;;;; prototype-specializer.lisp --- Specializer for dispatching on prototype objects.
;;;;
;;;; Copyright (C) 2013, 2014 Christophe Rhodes,
;;;;
;;;; Author: Christophe Rhodes <csr21@cantab.net>

(cl:in-package #:prototype-specializer)

;;; prototype-object and role

(defclass prototype-object () ()) ; forward declaration

(defstruct (role (:type list) (:constructor make-role (method argpos)))
  method argpos)
(defun add-role (obj role)
  (let ((pos (role-argpos role))
        (roles (roles obj)))
    (unless (< pos (length roles))
      (dotimes (i (- (1+ pos) (length roles)))
        (vector-push-extend nil roles)))
    (pushnew (role-method role) (aref roles pos))))
(defun remove-role (obj role)
  (let ((pos (role-argpos role)))
    (setf (aref (roles obj) pos)
          (remove (role-method role) (aref (roles obj) pos)))
    (tagbody
     start
       (when (or (= (length (roles obj)) 0)
                 (aref (roles obj) (1- (length (roles obj)))))
         (go done))
       (vector-pop (roles obj))
       (go start)
     done)))
(defun find-role (role obj)
  (when (< (role-argpos role) (length (roles obj)))
    (find (role-method role) (aref (roles obj) (role-argpos role)))))
(defun map-roles (fun obj)
  (dotimes (i (length (roles obj)))
    (dolist (m (aref (roles obj) i))
      (funcall fun (make-role m i)))))
(defmacro do-roles ((rvar form &optional result) &body body)
  `(progn (map-roles (lambda (,rvar) ,@body) ,form) ,result))

(eval-when (:compile-toplevel :load-toplevel :execute) ; for /root/ below
  (defclass prototype-object ()
    (;; FIXME: we should add slots at some point
     (delegations :initarg :delegations :accessor delegations)
     (roles :initform (make-array 0 :adjustable t :fill-pointer t)
            :accessor roles)
     ;; debugging aid
     (name :initarg :name))))
(defmethod print-object ((o prototype-object) s)
  (if (slot-boundp o 'name)
      (format s "~S" (slot-value o 'name))
      (print-unreadable-object (o s :type t :identity t)
        (format s "[~{~S~^, ~}]" (delegations o)))))

(defun add-delegation (obj del)
  (push del (delegations obj)))
(defun remove-delegation (obj)
  (pop (delegations obj)))
(defun map-delegations (fun obj)
  (funcall fun obj)
  ;; FIXME: should we maintain a table of visited nodes?  Should it be
  ;; topologically sorted?  Section 5.3 in PwMD [Salzman & Aldrich]
  ;; suggests not, at least for now
  (mapc (lambda (o) (map-delegations fun o)) (delegations obj))
  nil)

(defun clone (p)
  (let ((result (make-instance 'prototype-object
                               :delegations (copy-list (delegations p)))))
    (do-roles (r p result)
      (add-role result r))))

(sb-ext:defglobal /root/
    (make-instance 'prototype-object :name '/root/ :delegations nil))
(declaim (type prototype-object /root/)
         (sb-ext:always-bound /root/))

;;; prototype-specializer
;;;
;;; redefinition semantics are interesting.  We need the INFO here so
;;; that we can implement specializer-accepts-p, which must be able to
;;; lookup the particular method/argpos that the specializer
;;; represents.  But we must also be able to redefine methods in a way
;;; that isn't insane, which I think implies that same-specializer-p
;;; should ignore the INFO and just use the OBJECT.
(defclass prototype-specializer (extended-specializer)
  ((role :accessor prototype-specializer-role)
   (object :initarg :object :accessor prototype-specializer-object)))
(defmethod print-object ((o prototype-specializer) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "~S" (prototype-specializer-object o))))
(defmethod sb-pcl::same-specializer-p
    ((s1 prototype-specializer) (s2 prototype-specializer))
  (eql (prototype-specializer-object s1)
       (prototype-specializer-object s2)))

;;; prototype-generic-function

(defclass prototype-generic-function (specializable-generic-function)
  ()
  (:metaclass sb-mop:funcallable-standard-class))
(defmethod sb-pcl:make-method-specializers-form
    ((gf prototype-generic-function) method snames env)
  (flet ((frob (x)
           (typecase x
             (sb-mop:specializer x)
             (symbol `(make-instance 'prototype-specializer :object ,x))
             ((cons (eql 'class)) `(find-class ',(cadr x)))
             ((cons (eql 'eql)) `(sb-mop:intern-eql-specializer ,(cadr x)))
             (t (error "unexpected specializer name: ~S" x)))))
    `(list ,@(mapcar #'frob snames))))
(defmethod sb-pcl:parse-specializer-using-class
    ((gf prototype-generic-function) name)
  (make-instance 'prototype-specializer :object name))
(defmethod sb-pcl:unparse-specializer-using-class
    ((gf prototype-generic-function) (s prototype-specializer))
  (let ((object (prototype-specializer-object s)))
    (if (slot-boundp object 'name)
        (slot-value object 'name)
        s)))

(defmethod add-method :after ((gf prototype-generic-function) m)
  (let ((ss (sb-mop:method-specializers m)))
    (do* ((i 0 (1+ i))
          (ss ss (cdr ss))
          (s (car ss) (car ss)))
         ((null ss))
      (when (typep s 'prototype-specializer)
        (let ((object (prototype-specializer-object s))
              (role (make-role m i)))
          (setf (prototype-specializer-role s) role)
          (add-role object role))))))
(defmethod remove-method :after ((gf prototype-generic-function) m)
  (let ((ss (sb-mop:method-specializers m)))
    (do* ((i 0 (1+ i))
          (ss ss (cdr ss))
          (s (car ss) (car ss)))
         ((null ss))
      (when (typep s 'prototype-specializer)
        (let ((object (prototype-specializer-object s))
              (role (make-role m i)))
          (setf (prototype-specializer-role s) nil)
          ;; this is one of the places where the semantics
          ;; are... dodgy.  Removing the method from the generic
          ;; function, and the role from the object, doesn't affect
          ;; the roles in any clones.  We could potentially use the
          ;; fact that once removed the method is no longer associated
          ;; with a generic function?  Hm, C-A-M will not consider the
          ;; removed method for applicability...
          (remove-role object role))))))

(defmethod generalizer-of-using-class
    ((gf prototype-generic-function) (object prototype-object))
  object)

(defmethod specializer-accepts-generalizer-p
    ((gf prototype-generic-function) (s prototype-specializer) object)
  (values (specializer-accepts-p s object) t))

(defmethod specializer-accepts-p ((specializer prototype-specializer) object)
  (cond
    ((not (typep object 'prototype-object)) nil)
    ((eql (prototype-specializer-object specializer) /root/) t)
    (t
     (let ((role (prototype-specializer-role specializer)))
       (map-delegations
        (lambda (o)
          (when (find-role role o)
            (return-from specializer-accepts-p t)))
        object)))))

(defmethod specializer< ((gf prototype-generic-function) (s1 prototype-specializer) (s2 prototype-specializer) g)
  (let ((o1 (prototype-specializer-object s1))
        (o2 (prototype-specializer-object s2)))
    (map-delegations
     (lambda (o)
       (cond
         ((eql o o1) (return-from specializer< '<))
         ((eql o o2) (return-from specializer< '>))))
     g)
    '=))

(defmethod compute-applicable-methods-using-generalizers ((gf prototype-generic-function) generalizers)
  (values nil nil))
(defmethod generalizer-equal-hash-key ((gf prototype-generic-function) (g prototype-object))
  g)
