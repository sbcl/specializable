;;;; accept-specializer.lisp --- accept specializer examples.
;;;;
;;;; Copyright (C) 2013, 2014 Christophe Rhodes,
;;;;
;;;; Author: Christophe Rhodes <csr21@cantab.net>

(cl:defpackage #:accept-specializer.example
  (:use
   #:cl
   #:accept-specializer))

(cl:in-package #:accept-specializer.example)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric respond (request)
    (:generic-function-class accept-generic-function)
    (:method-combination list)))
(defmethod respond list (request)
  t)
(defmethod respond list ((s string))
  'string)
(defmethod respond list ((s (accept "text/html")))
  "text/html")
(defmethod respond list ((s (accept "audio/mp3")))
  "audio/mp3")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric cn-test (request)
    (:generic-function-class accept-generic-function)
    (:method-combination content-negotiation)))
(defmethod cn-test ((request (accept "text/html")))
  'html)
(defmethod cn-test ((request (accept "text/plain")))
  'plain)
(defmethod cn-test ((request (accept "image/webp")))
  'webp)
(defmethod cn-test ((request (accept "audio/mp3")))
  (call-next-method)
  'mp3)
(defmethod cn-test :after (request)
  (print 'after))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric cn/or-test (request)
    (:generic-function-class accept-generic-function)
    (:method-combination content-negotiation/or)))
(defmethod cn/or-test or ((request (accept "audio/mp3")))
  'mp3)
(defmethod cn/or-test or ((request (accept "image/webp")))
  'webp)
(defmethod cn/or-test :around ((request t))
  (print :around)
  (call-next-method))
