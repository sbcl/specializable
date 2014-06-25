;;;; language-extension.pattern-specializer.asd --- System definition for the language-extension.pattern-specializer system.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:asdf-user)

(defsystem :language-extension.pattern-specializer
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :license     "TODO"
  :description "Use optima patterns as specializers in CLOS methods - SBCL ONLY"
  :depends-on  (:specializable

                :alexandria
                :optima)
  :components  ((:module     "optima-extensions"
                 :pathname   "src/pattern-specializer/optima-extensions"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")
                              (:file       "type-interoperation")
                              (:file       "transform")
                              (:file       "normalize")))

                (:module     "pattern-specializer"
                 :pathname   "src/pattern-specializer"
                 :depends-on ("optima-extensions")
                 :serial     t
                 :components ((:file       "package"))))

  :in-order-to ((test-op (test-op :language-extension.pattern-specializer-test))))

(defsystem :language-extension.pattern-specializer-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :license     "TODO"
  :description "Tests for the language-extension.pattern-specializer system."
  :depends-on  (:fiveam

                :language-extension.pattern-specializer)
  :components  ((:module     "pattern-specializer"
                 :pathname   "test/pattern-specializer"
                 :serial     t
                 :components ((:file       "package")))

                (:module     "optima-extensions"
                 :pathname   "test/pattern-specializer/optima-extensions"
                 :depends-on ("pattern-specializer")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "util")
                              (:file       "type-interoperation")
                              (:file       "transform")
                              (:file       "normalize")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :language-extension.pattern-specializer-test))))
  (funcall (read-from-string "pattern-specializer.test:run-tests")))
