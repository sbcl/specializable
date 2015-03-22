;;;; language-extension.type-specializer.asd --- System definition for the language-extension.type-specializer system.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:asdf-user)

(defsystem :language-extension.type-specializer
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :license     "TODO"
  :description "Use types as specializers in CLOS methods - SBCL ONLY"
  :depends-on  (:specializable

                :alexandria)
  :components  ((:module     "type-specializer"
                 :pathname   "src/type-specializer"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")
                              (:file       "discrimination")
                              (:file       "type-specializer")
                              (:file       "type-generic-function")

                              (:file       "debug"))))

  :in-order-to ((test-op (test-op :language-extension.type-specializer-test))))

(defsystem :language-extension.type-specializer-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :license     "TODO"
  :description "Tests for the language-extension.type-specializer system."
  :depends-on  (:fiveam

                :language-extension.type-specializer

                :specializable-test)
  :components  ((:module     "type-specializer"
                 :pathname   "test/type-specializer"
                 :serial     t
                 :components ((:file       "package")
                              #+later (:file       "discrimination")
                              (:file       "type-specializer")
                              #+later (:file       "type-generic-function")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :language-extension.type-specializer-test))))
  (funcall (read-from-string "type-specializer.test:run-tests")))
