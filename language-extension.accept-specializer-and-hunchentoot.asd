;;;; language-extension.accept-specializer-and-hunchentoot.asd --- System definition for the language-extension.accept-specializer system.
;;;;
;;;; Copyright (C) 2014, 2017 Christophe Rhodes, Jan Moringen
;;;;
;;;; Author: Christophe Rhodes <csr21@cantab.net>
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "language-extension.accept-specializer-and-hunchentoot"
  :author      "Christophe Rhodes <csr21@cantab.net>"
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :license     "TODO"
  :description "Support for hunchentoot requests in accept-generic-function - SBCL ONLY"
  :depends-on  ("language-extension.accept-specializer"
                "hunchentoot")
  :components  ((:module     "src"
                 :pathname   "src/accept-specializer"
                 :components ((:file       "hunchentoot")))))
