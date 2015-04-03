;;;; specializable-graph.asd --- System definition for the specializable-graph system.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:asdf-user)

(defsystem :specializable-graph
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :license     "TODO"
  :description "Draw graphs of specializer relations"
  :depends-on  (:alexandria
                :specializable
                :cl-dot)
  :components  ((:module "src/graph"
                 :serial t
                 :components ((:file       "package")
                              (:file       "protocol")
                              (:file       "graph")))))
