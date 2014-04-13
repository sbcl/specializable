;;;; language-extension.pattern-specializer.asd --- System definition for the language-extension.pattern-specializer system.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language-extension.pattern-specializer-sytem
  (:use
   #:cl
   #:asdf))

(cl:in-package #:language-extension.pattern-specializer-sytem)

(defsystem :language-extension.pattern-specializer
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :license     "LLGPLv3; see COPYING file for details."
  :description "Use optima patterns as specializers in CLOS methods - SBCL ONLY"
  :depends-on  (;; (:feature :sbcl) this works differently than one might think; it's more like (:if-features :sbcl :foo :bar)

                :specializable

                :alexandria
                :optima)
  :components  ((:module     "src"
                 :pathname   "src/pattern-specializer"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")
                              (:file       "optima-extensions")
                              (:file       "pattern-specializer"))))

  :in-order-to ((test-op (test-op :language-extension.pattern-specializer-test))))

(defsystem :language-extension.pattern-specializer-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :license     "LLGPLv3; see COPYING file for details."
  :description "Tests for the language-extension.pattern-specializer system."
  :depends-on  (:fiveam

                :language-extension.pattern-specializer)
  :components  ((:module     "test"
                 :pathname   "test/pattern-specializer"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "optima-extensions")
                              (:file       "pattern-specializer")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :language-extension.pattern-specializer-test))))
  (funcall (read-from-string "pattern-specializer.test:run-tests")))
