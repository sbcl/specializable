;;;; label.lisp --- Labels in pattern-specializer graphs.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer.graph)

;;; Utilities

(defun pattern-constructor-name (pattern)
  (let* ((class-name  (class-name (class-of pattern)))
         (name-string (string class-name))
         (suffix      (string '#:-pattern)))
    (if (ends-with-subseq suffix name-string)
        (subseq name-string 0 (- (length name-string) (length suffix)))
        name-string)))

;;; Variable labels

(defstruct (variable-context (:constructor make-variable-context (clusters)))
  (clusters)
  (colors (make-hash-table :test #'eq)))

(defun patterns->variable-context (patterns) ; TODO name
  (let ((clusters '() #+no (patterns->variable-clusters patterns)))
    (make-variable-context clusters)))

(defun variable-context-cluster-color (cluster context)
  (let ((colors1 '("red" "blue" "green" "purple" "yellow" "pink"))
        (colors (variable-context-colors context)))
    (flet ((new-color ()
             (nth (hash-table-count colors) colors1)))
      (ensure-gethash cluster colors (new-color)))))

(defun maybe-color (path context)
  (let ((cluster (find (remove 'and-pattern path :test #'eq :key #'car)
                       (variable-context-clusters context)
                       :test #'equal
                       :key  #'variable-info-path
                       #+no (lambda (path cluster)
                         (some
                          (lambda (variable)
                            (find path (variable-info-paths variable) :test #'equal))
                          (variable-cluster-variables cluster))))))
    (when cluster
      (variable-context-cluster-color cluster context))))

(defun pattern-label1 (pattern context)
  (map-patterns-and-paths
   nil (lambda (pattern path recurse)
         (pattern-label pattern path context recurse))
   pattern))

(defmethod pattern-label :around ((pattern optima::pattern)
                                        (path    t)
                                        (context t)
                                        (recurse function))
  (let* ((color  (maybe-color path context))
         (string (call-next-method)))
    (if color
        `(:font ((:color ,color)) ,string)
        string)))

(defmethod pattern-label ((pattern optima::pattern)
                                (path    t)
                                (context t)
                                (recurse function))
  (princ-to-string pattern))

(defmethod pattern-label ((pattern optima.core:variable-pattern)
                                (path    t)
                                (context t)
                                (recurse function))
  (let* ((name        (optima.core:variable-pattern-name pattern))
         (name-string (princ-to-string name))
         (color       (unless (symbol-package name)
                        "#606060")))
    `(:font (,@(when color `((:color ,color))))
            (:i () ,name-string))))

(defmethod pattern-label ((pattern optima.core:complex-pattern)
                                (path    t)
                                (context t)
                                (recurse function))
  (let* ((subresults (funcall recurse))
         (substrings (loop :for result :in subresults
                        :for i :from 1
                        :collect result
                        :collect (if (< i (length subresults)) " " ""))))
    `(:font ()
            ,(format nil "(~A " (pattern-constructor-name pattern))
            ,@substrings
            ")")))

(defmethod pattern-label ((pattern optima.core:class-pattern)
                                (path    t)
                                (context t)
                                (recurse function))
  (let* ((class      (string (optima.core:class-pattern-class-name pattern)))
         (slots      (optima.core:class-pattern-slot-names pattern))
         (subresults (funcall recurse))
         (substrings (loop :for slot :in slots
                        :for result :in subresults
                        :for i :from 1
                        :collect `(:font () "(" ,(string slot) " ",result ")")
                        :collect (if (< i (length subresults)) " " ""))))
    `(:font ()
            ,(format nil "(~A ~A " (pattern-constructor-name pattern) class)
            ,@substrings
            ")")))

(defmethod pattern-label ((pattern optima.core:guard-pattern)
                                path
                                (context t)
                                recurse)
  (let* ((subresult    (first (funcall recurse)))
         (guard-string (princ-to-string
                        (optima.core:guard-pattern-test-form pattern))))
    `(:font ()
            ,(format nil "(~A " (pattern-constructor-name pattern))
            ,subresult
            " "
            (:font ((:color "#606060")) ,guard-string)
            ")")))
