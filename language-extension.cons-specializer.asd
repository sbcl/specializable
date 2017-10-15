;;;; language-extension.cons-specializer.asd --- System definition for the language-extension.cons-specializer system.
;;;;
;;;; Copyright (C) 2014 Christophe Rhodes
;;;;
;;;; Author: Christophe Rhodes <csr21@cantab.net>

(defsystem "language-extension.cons-specializer"
  :author      "Christophe Rhodes <csr21@cantab.net>"
  :license     "TODO"
  :description "Dispatch and car or conses in CLOS methods - SBCL ONLY"
  :depends-on  ("specializable")
  :components  ((:module     "src"
                 :pathname   "src/cons-specializer"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "cons-specializer"))))

  :in-order-to ((test-op (test-op "language-extension.cons-specializer/test"))))

(defsystem "language-extension.cons-specializer/test"
  :author      "Christophe Rhodes <csr21@cantab.net>"
  :license     "TODO"
  :description "Tests for the language-extension.cons-specializer system."
  :depends-on  ("fiveam"

                "language-extension.cons-specializer"

                "specializable/test")
  :components  ((:module     "test"
                 :pathname   "test/cons-specializer"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "cons-specializer")

                              (:file       "examples"))))
  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:cons-specializer.test '#:run-tests)))
