;;;; language-extension.accept-specializer.asd --- System definition for the language-extension.accept-specializer system.
;;;;
;;;; Copyright (C) 2014 Christophe Rhodes
;;;;
;;;; Author: Christophe Rhodes <csr21@cantab.net>

(cl:in-package #:asdf-user)

(defsystem :language-extension.accept-specializer
  :author      "Christophe Rhodes <csr21@cantab.net>"
  :license     "TODO"
  :description "Dispatch on accepted content types of requests in CLOS methods - SBCL ONLY"
  :depends-on  (:specializable
                :cl-ppcre
                :hunchentoot)
  :components  ((:module     "src"
                 :pathname   "src/accept-specializer"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "accept-specializer"))))

  :in-order-to ((test-op (test-op :language-extension.accept-specializer-test))))

(defsystem :language-extension.accept-specializer-test
  :author      "Christophe Rhodes <csr21@cantab.net>"
  :license     "TODO"
  :description "Tests for the language-extension.accept-specializer system."
  :depends-on  (:fiveam

                :language-extension.accept-specializer

                :specializable-test)
  :components  ((:module     "test"
                 :pathname   "test/accept-specializer"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "accept-specializer")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :language-extension.accept-specializer-test))))
  (funcall (read-from-string "accept-specializer.test:run-tests")))
