;;;; package.lisp --- Package definition for the specializable system.
;;;;
;;;; Copyright (C) 2014 Christophe Rhodes, Jan Moringen
;;;;
;;;; Author: Christophe Rhodes <csr21@cantab.net>
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage "SPECIALIZABLE"
  (:use "CL" "SB-EXT")
  (:export "SPECIALIZABLE-GENERIC-FUNCTION" "SPECIALIZABLE-METHOD"
           "EXTENDED-SPECIALIZER"

           "SPECIALIZER-ACCEPTS-P" "SPECIALIZER-ACCEPTS-GENERALIZER-P"
           "SPECIALIZER<"

           "GENERALIZER-OF-USING-CLASS"
           "COMPUTE-APPLICABLE-METHODS-USING-GENERALIZERS"
           "GENERALIZER-EQUAL-HASH-KEY"

           "DEFINE-EXTENDED-SPECIALIZER"))
