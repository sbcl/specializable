;;;; prototype-specializer.lisp --- Specializer for dispatching on prototype objects.
;;;;
;;;; Copyright (C) 2013, 2014 Christophe Rhodes
;;;;
;;;; Author: Christophe Rhodes <csr21@cantab.net>
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

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

(declaim (ftype (function (prototype-object prototype-object))
                add-delegation))
(defun add-delegation (object delegation)
  ;; Check for cyclic delegations DELEGATION may introduce.
  ;;
  ;; MAP-DELEGATIONS-AND-PATHS conses and is generally slower. So
  ;; perform a quick check via MAP-DELEGATIONS and only call
  ;; MAP-DELEGATIONS-AND-PATHS if an error report has to be generated.
  (flet ((loose ()
           (map-delegations-and-paths
            (lambda (other-delegation path)
              (when (eq object other-delegation)
                (error 'delegation-cycle-error
                       :object     object
                       :delegation delegation
                       :path       (list* object (reverse (list* object path))))))
            delegation)))
    (map-delegations (lambda (other-delegation)
                       (when (eq object other-delegation)
                         (loose)))
                     delegation))
  (pushnew delegation (delegations object)))
(declaim (ftype (function (prototype-object))
                remove-delegation))
(defun remove-delegation (obj)
  (pop (delegations obj)))
(defun map-delegations (fun obj)
  (funcall fun obj)
  ;; FIXME: should we maintain a table of visited nodes?  Should it be
  ;; topologically sorted?  Section 5.3 in PwMD [Salzman & Aldrich]
  ;; suggests not, at least for now
  (mapc (lambda (o) (map-delegations fun o)) (delegations obj))
  nil)
(declaim (ftype (function ((or function symbol cons) prototype-object))
                map-delegations-and-paths))
(defun map-delegations-and-paths (function object)
  (let ((function (coerce function 'function)))
    (labels ((recur (object path)
               (funcall function object path)
               (when-let ((delegations (delegations object)))
                 (mapc (rcurry #'recur (list* object path))
                       delegations))))
      (declare (dynamic-extent #'recur))
      (recur object '())))
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
(defmethod sb-pcl::same-specializer-p
    ((s1 prototype-specializer) (s2 prototype-specializer))
  (eql (prototype-specializer-object s1)
       (prototype-specializer-object s2)))

;; Instances of this class are used when a specializer object has to
;; be created for a prototype object that is only known by name
;; (e.g. at DEFMETHDO macroexpansion time).
(defclass early-prototype-specializer (prototype-specializer)
  ((object :type symbol)))
(defmethod print-object ((object early-prototype-specializer) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "object named ~S" (prototype-specializer-object object))))

(defclass late-prototype-specializer (prototype-specializer)
  (;; FIXME This slot type may be too restrictive later but is useful
   ;; now to catch things like constructing a specializer with a
   ;; symbol instead of the object named by it (at high debug and/or
   ;; safety optimization qualities). That particular problem could
   ;; happen for example if something went wrong in the
   ;; SPECIALIZER-TYPE-SPECIFIER protocol.
   (object :type prototype-object)))
(defmethod print-object ((object late-prototype-specializer) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (prototype-specializer-object object))))

;;; prototype-generic-function

(defclass prototype-generic-function (specializable-generic-function)
  ()
  (:metaclass sb-mop:funcallable-standard-class))
(defmethod sb-pcl:make-method-specializers-form
    ((gf prototype-generic-function) method snames env)
  (flet ((frob (x)
           (typecase x
             (sb-mop:specializer x)
             ((or symbol prototype-object)
              `(make-instance 'late-prototype-specializer :object ,x))
             ((cons (eql 'class)) `(find-class ',(cadr x)))
             ((cons (eql 'eql)) `(sb-mop:intern-eql-specializer ,(cadr x)))
             (t (error "unexpected specializer name: ~S" x)))))
    `(list ,@(mapcar #'frob snames))))

(defmethod sb-pcl:parse-specializer-using-class
    ((gf prototype-generic-function) (name symbol))
  (make-instance 'early-prototype-specializer :object name))
(defmethod sb-pcl:parse-specializer-using-class
    ((gf prototype-generic-function) (name prototype-object))
  (make-instance 'late-prototype-specializer :object name))

(defmethod sb-pcl:unparse-specializer-using-class
    ((gf prototype-generic-function) (s early-prototype-specializer))
  (prototype-specializer-object s))
(defmethod sb-pcl:unparse-specializer-using-class
    ((gf prototype-generic-function) (s late-prototype-specializer))
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
      (when (typep s 'late-prototype-specializer)
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
      (when (typep s 'late-prototype-specializer)
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
    ((gf prototype-generic-function) (s late-prototype-specializer) object)
  (values (specializer-accepts-p s object) t))

(defmethod specializer-accepts-p ((specializer late-prototype-specializer) object)
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

(defmethod specializer< ((gf prototype-generic-function)
                         (s1 late-prototype-specializer)
                         (s2 late-prototype-specializer)
                         g)
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
