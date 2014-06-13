;;;; hunchentoot.lisp --- Protocol implementation for Hunchentoot requests.
;;;;
;;;; Copyright (C) 2013, 2014 Christophe Rhodes
;;;;
;;;; Author: Christophe Rhodes <csr21@cantab.net>

(cl:in-package #:accept-specializer)

(defmethod generalizer-of-using-class ((gf accept-generic-function) (arg tbnl:request))
  (make-instance 'accept-generalizer
                 :header (tbnl:header-in :accept arg)
                 :next (call-next-method)))

(defmethod specializer-accepts-p ((specializer accept-specializer) (obj tbnl:request))
  (q-ok (media-type specializer) (parse-accept-string (tbnl:header-in :accept obj))))

(defmethod handle-content-type ((x tbnl:request))
  (setf (tbnl:content-type*) *actual-content-type*))
