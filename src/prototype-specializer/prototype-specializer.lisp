;;;; prototype-specializer.lisp --- Specializer for dispatching on prototype objects.
;;;;
;;;; Copyright (C) 2013, 2014 Christophe Rhodes,
;;;;
;;;; Author: Christophe Rhodes <csr21@cantab.net>
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:prototype-specializer)

;;; prototype-object and role

(defclass prototype-object () ()) ; forward declaration

(defstruct (role (:type list)
                 (:constructor make-role (method argpos))
                 (:copier nil))
  method argpos)
(defun add-role (obj role)
  (let ((position (role-argpos role))
        (roles (roles obj)))
    (unless (< position (length roles))
      (adjust-array roles (1+ position)
                    :initial-element '() :fill-pointer (1+ position)))
    (pushnew (role-method role) (aref roles position))))
(defun remove-role (obj role)
  (let ((position (role-argpos role))
        (roles (roles obj)))
    (setf (aref roles position)
          (remove (role-method role) (aref roles position)))
    ;; Remove trailing NILs in ROLES.
    (loop :until (or (zerop (length roles))
                     (aref roles (1- (length roles))))
       :do (vector-pop roles))))
(defun find-role (role obj)
  (let ((position (role-argpos role))
        (roles (roles obj)))
    (when (< position (length roles))
      (find (role-method role) (aref roles position)))))
(defun map-roles (function object)
  (let ((function (coerce function 'function))
        (roles (roles object)))
    (dotimes (i (length roles))
      (dolist (m (aref roles i))
        (funcall function (make-role m i))))))
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
(defmethod print-object ((o prototype-object) stream)
  (if (slot-boundp o 'name)
      (format stream ">~S" (slot-value o 'name))
      (print-unreadable-object (o stream :type t :identity t)
        (format stream "[~{~S~^, ~}]" (delegations o)))))

(declaim (ftype (function (prototype-object prototype-object))
                add-delegation))
(defun add-delegation (object delegation)
  (map-delegations
   (lambda (old-delegation)
     (when (eq delegation old-delegation)
       (error "~@<Adding delegation ~A to ~A would create a delegation
               cycle.~@:>"
              delegation object)))
   object)
  (push delegation (delegations object)))
(declaim (ftype (function (prototype-object))
                remove-delegation))
(defun remove-delegation (obj)
  (pop (delegations obj)))

(declaim (ftype (function ((or function symbol cons) prototype-object))
                map-delegations))
(defun map-delegations (function object)
  (let ((function (coerce function 'function)))
    (labels ((recur (object)
               (funcall function object)
               ;; FIXME: should we maintain a table of visited nodes?
               ;; Should it be topologically sorted?  Section 5.3 in
               ;; PwMD [Salzman & Aldrich] suggests not, at least for
               ;; now
               (mapc #'recur (delegations object))))
      (declare (dynamic-extent #'recur))
      (recur object)))
  nil)

(defun clone (object)
  (let ((result (make-instance 'prototype-object
                               :delegations (copy-list (delegations object)))))
    (do-roles (role object result)
      (add-role result role))))

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

(defmethod sb-pcl:make-specializer-form-using-class or
    ((proto-generic-function prototype-generic-function)
     (proto-method standard-method)
     (specializer-name prototype-object)
     environment)
  `(make-instance 'late-prototype-specializer :object ,specializer-name))
(defmethod sb-pcl:make-specializer-form-using-class or
    ((proto-generic-function prototype-generic-function)
     (proto-method standard-method)
     (specializer-name symbol)
     environment)
  `(make-instance 'late-prototype-specializer :object ,specializer-name))

(defmethod sb-pcl:parse-specializer-using-class
    ((gf prototype-generic-function) (name symbol))
  (make-instance 'early-prototype-specializer :object name))
(defmethod sb-pcl:parse-specializer-using-class
    ((gf prototype-generic-function) (name prototype-object))
  (make-instance 'late-prototype-specializer :object name))

(defmethod sb-pcl:unparse-specializer-using-class
    ((gf prototype-generic-function) (specializer early-prototype-specializer))
  (prototype-specializer-object specializer))
(defmethod sb-pcl:unparse-specializer-using-class
    ((gf prototype-generic-function) (specializer late-prototype-specializer))
  (let ((object (prototype-specializer-object specializer)))
    (if (slot-boundp object 'name)
        (slot-value object 'name)
        specializer)))

(defmethod add-method :after ((gf prototype-generic-function) method)
  (loop :for specializer :in (sb-mop:method-specializers method)
     :for i :from 0
     :do (when (typep specializer 'late-prototype-specializer)
           (let ((object (prototype-specializer-object specializer))
                 (role (make-role method i)))
             (setf (prototype-specializer-role specializer) role)
             (add-role object role)))))
(defmethod remove-method :after ((gf prototype-generic-function) method)
  (loop :for specializer :in (sb-mop:method-specializers method)
     :for i :from 0
     :do (when (typep specializer 'late-prototype-specializer)
           (let ((object (prototype-specializer-object specializer))
                 (role (make-role method i)))
             (setf (prototype-specializer-role specializer) nil)
             ;; this is one of the places where the semantics
             ;; are... dodgy.  Removing the method from the generic
             ;; function, and the role from the object, doesn't affect
             ;; the roles in any clones.  We could potentially use the
             ;; fact that once removed the method is no longer
             ;; associated with a generic function?  Hm, C-A-M will
             ;; not consider the removed method for applicability...
             (remove-role object role)))))

(defmethod generalizer-of-using-class
    ((gf prototype-generic-function) (object prototype-object) arg-position)
  object)

(defmethod sb-pcl:specializer-type-specifier ((proto-generic-function prototype-generic-function)
                                              (proto-method standard-method)
                                              (specializer symbol))
  'prototype-object)
(defmethod sb-pcl:specializer-type-specifier ((proto-generic-function prototype-generic-function)
                                              (proto-method standard-method)
                                              (specializer prototype-specializer))
  'prototype-object)

(defmethod specializer-accepts-generalizer-p
    ((gf prototype-generic-function) (specializer late-prototype-specializer) object)
  (values (specializer-accepts-p specializer object) t))

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
                         generalizer)
  (let ((o1 (prototype-specializer-object s1))
        (o2 (prototype-specializer-object s2)))
    (map-delegations
     (lambda (object)
       (cond
         ((eql object o1) (return-from specializer< '<))
         ((eql object o2) (return-from specializer< '>))))
     generalizer)
    '=))

(defmethod compute-applicable-methods-using-generalizers
    ((gf prototype-generic-function) generalizers)
  (values nil nil))
(defmethod generalizer-equal-hash-key ((gf prototype-generic-function)
                                       (generalizer prototype-object))
  generalizer)
