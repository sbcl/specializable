;;;; package.lisp --- Package for unit tests of the specializable system.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage "SPECIALIZABLE-TEST"
  (:use "CL" "SPECIALIZABLE" "FIVEAM")
  (:export "RUN-TESTS"))

(cl:in-package "SPECIALIZABLE-TEST")

(def-suite :specializable
  :description
  "Test suite of the specializable system.")

(defun run-tests ()
  (run! :specializable))
