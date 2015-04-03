;;;; language-extension.pattern-specializer-graph.asd --- System definition for the language-extension.pattern-specializer-graph system.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:asdf-user)

(defsystem :language-extension.pattern-specializer-graph
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :license     "TODO"
  :description "Draw graphs of pattern-specializer relations"
  :depends-on  (:specializable-graph
                :language-extension.pattern-specializer)
  :components  ((:module     "graph"
                 :pathname   "src/pattern-specializer/graph"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")
                              (:file       "label")
                              (:file       "pattern-graph")
                              (:file       "pattern-specializer-graph")))))
