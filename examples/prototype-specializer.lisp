;;;; prototype-specializer.lisp --- prototype specializer examples.
;;;;
;;;; Copyright (C) 2013, 2014 Christophe Rhodes
;;;;
;;;; Author: Christophe Rhodes <csr21@cantab.net>

(cl:defpackage #:prototype-specializer.example
  (:use
   #:cl
   #:prototype-specializer))

(cl:in-package #:prototype-specializer.example)

(defmacro defpvar (name value)
  `(let ((val ,value))
     (setf (slot-value val 'prototype-specializer::name) ',name)
     (defparameter ,name val)))

(defpvar /animal/ (clone /root/))
(defpvar /fish/ (clone /root/))
(defpvar /shark/ (clone /root/))
(defpvar /healthy-shark/ (clone /root/))
(defpvar /dying-shark/ (clone /root/))
(add-delegation /fish/ /animal/)
(add-delegation /shark/ /animal/)
(add-delegation /shark/ /healthy-shark/)

(defgeneric fight (x y)
  (:generic-function-class prototype-generic-function))
(defmethod fight ((x /healthy-shark/) (y /shark/))
  (remove-delegation x)
  (add-delegation x /dying-shark/)
  x)

(defgeneric encounter (x y)
  (:generic-function-class prototype-generic-function))
(defmethod encounter ((x /fish/) (y /healthy-shark/))
  (format t "~&~A swims away~%" x))
(defmethod encounter ((x /fish/) (y /animal/))
  x)
(defmethod encounter ((x /healthy-shark/) (y /fish/))
  (format t "~&~A swallows ~A~%" x y))
(defmethod encounter ((x /healthy-shark/) (y /shark/))
  (format t "~&~A fights ~A~%" x y)
  (fight x y))
