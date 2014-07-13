;;;; conditions.lisp --- conditions used in the prototype-specializer system.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:prototype-specializer)

(define-condition delegation-cycle-error (error)
  ((object     :initarg :object
               :reader  delegation-cycle-error-object)
   (delegation :initarg :delegation
               :reader  delegation-cycle-error-delegation)
   (path       :initarg :path
               :type    list
               :reader  delegation-cycle-error-path))
  (:default-initargs
   :object     (required-argument :object)
   :delegation (required-argument :delegation)
   :path       (required-argument :path))
  (:report (lambda (condition stream)
             (format stream "~@<Adding delegation ~A to ~A would ~
                             create a delegation cycle:~@:_~@:_~
                             ~2@T~@<    ~{~A~^~@:_ -> ~}~:>~@:>"
                     (delegation-cycle-error-delegation condition)
                     (delegation-cycle-error-object condition)
                     (delegation-cycle-error-path condition)))))
