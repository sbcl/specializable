;;;; specializable.asd --- System definition for the specializable system.
;;;;
;;;; Copyright (C) 2014 Christophe Rhodes, Jan Moringen
;;;;
;;;; Author: Christophe Rhodes <csr21@cantab.net>
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(asdf:defsystem :specializable
  :author      "Christophe Rhodes <csr21@cantab.net>"
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :license     "TODO"
  :description "Generalized specializers - SBCL only"
  :components  ((:file       "specializable")))
