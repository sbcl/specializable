;;;; language-extension.prototype-specializer.asd --- System definition for the language-extension.prototype-specializer system.
;;;;
;;;; Copyright (C) 2014 Christophe Rhodes
;;;;
;;;; Author: Christophe Rhodes <csr21@cantab.net>

(cl:in-package #:asdf-user)

(defsystem :language-extension.prototype-specializer
  :author      "Christophe Rhodes <csr21@cantab.net>"
  :license     "TODO"
  :description "Dispatch on prototype objects in CLOS methods - SBCL ONLY"
  :depends-on  (:specializable)
  :components  ((:module     "src"
                 :pathname   "src/prototype-specializer"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "conditions")
                              (:file       "prototype-specializer"))))

  :in-order-to ((test-op (test-op :language-extension.prototype-specializer-test))))

(defsystem :language-extension.prototype-specializer-test
  :author      "Christophe Rhodes <csr21@cantab.net>"
  :license     "TODO"
  :description "Tests for the language-extension.prototype-specializer system."
  :depends-on  (:fiveam

                :language-extension.prototype-specializer

                :specializable-test)
  :components  ((:module     "test"
                 :pathname   "test/prototype-specializer"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "prototype-specializer")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :language-extension.prototype-specializer-test))))
  (funcall (read-from-string "prototype-specializer.test:run-tests")))
