;;;; util.lisp --- Utilities for tests of optima extensions.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer.optima-extensions.test)

(defun gen-leaf (&key
                 (value (gen-integer :min -10 :max 10)))
  (lambda ()
    (when (zerop (random 2))
      (funcall value))))

;; TODO propose for inclusion in fiveam
(defun gen-tree1 (&key
                  (node-constructor #'cons)
                  (degree           2)
                  (size             20)
                  (elements         (gen-leaf)))
  (labels ((rec (&optional (current-depth 0))
             (let ((key (random (+ 3 (- size current-depth)))))
               (if (> key 2)
                   (apply node-constructor
                          (map-into (make-list degree)
                                    (lambda () (rec (+ current-depth 1)))))
                   (funcall elements)))))
    (lambda ()
      (rec))))
