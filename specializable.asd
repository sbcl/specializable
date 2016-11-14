;;;; specializable.asd --- System definition for the specializable system.
;;;;
;;;; Copyright (C) 2014, 2015, 2016 Christophe Rhodes, Jan Moringen
;;;;
;;;; Author: Christophe Rhodes <csr21@cantab.net>
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:asdf-user)

(defsystem :specializable
  :author      "Christophe Rhodes <csr21@cantab.net>"
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :license     "TODO"
  :description "Generalized specializers - SBCL only"
  :components  ((:module "src"
                 :serial t
                 :components ((:module     "pcl-patch"
                               :components ((:file       "specializer-type-specifier"))
                               :if-feature :sbcl)

                              (:file       "package")
                              (:file       "protocol")
                              (:file       "syntax")
                              (:file       "specializable"))))
  :in-order-to ((test-op (test-op :specializable-test))))

(defmethod perform :before ((operation load-op) (component (eql (find-system :specializable))))
  (let ((required-version '(1 3 12)))
    (flet ((lose ()
             (error "This system only work on SBCL, version ~{~D~^.~} or newer"
                    required-version)))
      #-sbcl (lose)
      #+sbcl
      (let ((version-assert (find-symbol "ASSERT-VERSION->=" :sb-ext)))
        (if version-assert
            (apply version-assert required-version)
            (lose))))))

(defsystem :specializable-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :author      "Christophe Rhodes <csr21@cantab.net>"
  :license     "TODO"
  :description "Unit tests of the specializable system"
  :depends-on  (:fiveam)
  :components  ((:module "test"
                 :serial t
                 :components ((:file       "package")
                              (:file       "util")
                              (:file       "syntax")
                              (:file       "specializable")

                              (:file       "examples")))) )

(defmethod perform ((operation test-op) (component (eql (find-system :specializable-test))))
  (uiop:symbol-call "SPECIALIZABLE-TEST" "RUN-TESTS"))
