;;;; transform.lisp --- Tests for transform operations.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:pattern-specializer.optima-extensions.test)

;;; Basic pattern transformation framework

(def-suite :pattern-specializer.optima-extensions.basic-transformations
    :in :pattern-specializer.optima-extensions)
(in-suite :pattern-specializer.optima-extensions.basic-transformations)

;;; map-pattern family of functions

(test map-pattern.smoke
  "Smoke test for the `map-pattern' function."

  (labels ((mock-transform (pattern recurse)
             (typecase pattern
               (variable-pattern
                (variable-pattern-name pattern))
               (t
                (list* (type-of pattern) (funcall recurse)))))
           (apply-map-pattern (pattern)
              (map-pattern 'list #'mock-transform pattern)))
    (mapc
     (make-pattern-predicate-test-case-thunk #'apply-map-pattern 1)

     '(((cons (cons x y) 1) (cons-pattern (cons-pattern x y) (constant-pattern)))))))

(test mapc-pattern.smoke
  "Smoke test for the `mapc-pattern' function."

  (flet ((apply-mapc-pattern (pattern)
           (let ((result '()))
             (flet ((mock-transform (pattern)
                      (appendf result
                               (list (typecase pattern
                                       (variable-pattern
                                        (variable-pattern-name pattern))
                                       (t
                                        (type-of pattern)))))))
               (mapc-pattern #'mock-transform pattern))
             result)))
    (mapc
     (make-pattern-predicate-test-case-thunk #'apply-mapc-pattern 1)

     '(((cons (cons x y) 1) (cons-pattern cons-pattern x y constant-pattern))))))

(test map-pattern/reconstitute
  "Smoke test for the `map-pattern/reconstitute' function."

  (flet ((complicated-transform (pattern recurse reconstitute)
           (typecase pattern
             (variable-pattern
              (make-variable-pattern))
             (and-pattern
              (apply #'make-and-pattern (reverse (funcall recurse))))
             (cons-pattern
              (make-not-pattern (funcall reconstitute))))))

    (mapc
     (make-pattern-transform-test-case-thunk
      'map-pattern/reconstitute #'complicated-transform)

     '((1                   1)

       (x                   _)

       ((cons (cons x y) z) (not (cons (not (cons _ _)) _)))

       ((not x)             (not _))
       ((not (cons 1 2))    (not (not (cons 1 2))))
       ((not (and x 2))     (not (and 2 _)))

       ((and 1 2)           (and 2 1))))))

;; map-patterns-and-paths family of functions

(test map-patterns-and-paths.smoke
  "Smoke test for the `map-patterns-and-paths' function."

  (labels ((mock-transform (pattern path recurse)
             (list* (type-of pattern) path (funcall recurse)))
           (apply-map-patterns-and-paths (pattern)
             (map-patterns-and-paths 'list #'mock-transform pattern)))
    (mapc
     (make-pattern-predicate-test-case-thunk
      #'apply-map-patterns-and-paths 1)

     '((1                   (constant-pattern ()))

       (x                   (variable-pattern ()))

       ((cons (cons x y) 1) (cons-pattern ()
                             (cons-pattern ((cons-pattern . 0))
                               (variable-pattern ((cons-pattern . 0)
                                                  (cons-pattern . 0)))
                               (variable-pattern ((cons-pattern . 0)
                                                  (cons-pattern . 1))))
                             (constant-pattern ((cons-pattern . 1)))))))))

(test mapc-patterns-and-paths.smoke
  "Smoke test for the `mapc-patterns-and-paths' function."

  (flet ((apply-mapc-patterns-and-paths (pattern)
           (let ((result '()))
             (flet ((mock-transform (pattern path)
                      (appendf result (list (list (type-of pattern) path)))))
               (mapc-patterns-and-paths #'mock-transform pattern))
             result)))
    (mapc
     (make-pattern-predicate-test-case-thunk
      #'apply-mapc-patterns-and-paths 1)

     '((1                   ((constant-pattern ())))

       (x                   ((variable-pattern ())))

       ((cons (cons x y) 1) ((cons-pattern ())
                             (cons-pattern ((cons-pattern . 0)))
                             (variable-pattern ((cons-pattern . 0)
                                                (cons-pattern . 0)))
                             (variable-pattern ((cons-pattern . 0)
                                                (cons-pattern . 1)))
                             (constant-pattern ((cons-pattern . 1)))))))))

(test map-patterns-and-paths/reconstitute.smoke
  "Smoke test for the `map-patterns-and-paths/reconstitute' function.

   Test using the pattern, the path, the recurse function and the
   reconstitute function in a complicated mock transformation."

  (flet ((complicated-transform (pattern path recurse reconstitute)
           (declare (type list path))
           (cond
             ((and (typep pattern 'variable-pattern) (evenp (length path)))
              (make-variable-pattern))
             ((and (typep pattern 'cons-pattern) (length= 1 path))
              (apply #'make-cons-pattern (reverse (funcall recurse))))
             ((and (typep pattern 'cons-pattern) (length= 2 path))
              (make-not-pattern (funcall reconstitute))))))

    (mapc
     (make-pattern-transform-test-case-thunk
      'map-patterns-and-paths/reconstitute
      #'complicated-transform)

     '((1                            1)

       (x                            _)

       ((cons (cons v (cons w x)) y) (cons (cons (not (cons w x)) _) y))

       ((not x)                      (not x))
       ((not (cons 1 2))             (not (cons 2 1)))
       ((not (cons x y))             (not (cons _ _)))))))

;; Specialized pattern transformations

(def-suite :pattern-specializer.optima-extensions.specialized-transformations
    :in :pattern-specializer.optima-extensions)
(in-suite :pattern-specializer.optima-extensions.specialized-transformations)

(test pattern-variables-and-paths.smoke
  "Smoke test for the `pattern-variables-and-paths' function."

  (mapc
   (make-pattern-predicate-test-case-thunk 'pattern-variables-and-paths 1)

   ;; Input pattern                   Expected variables and paths
   '((1                               ())

     (_                               ())
     (_ :include-unnamed t            ((nil . ())))
     (x                               ((x . ())))

     ((cons x y)                      ((x . ((cons-pattern . 0)))
                                       (y . ((cons-pattern . 1)))))

     ((class clazz (name x))          ((x . (((class-pattern . clazz) . name)))))

     ((type integer)                  ())
     ((guard x (consp x))             ((x . ((guard-pattern . 0)))))

     ((not 1)                         ())
     ((not _)                         ())
     ((not x)                         ((x . ((not-pattern . 0)))))

     ((and _ 1)                       ())
     ((and x 1)                       ((x . ((and-pattern . 0)))))
     ((and 1 x)                       ((x . ((and-pattern . 1)))))

     ((or _ 1)                        ())
     ((or x 1)                        ((x . ((or-pattern . 0)))))
     ((or 1 x)                        ((x . ((or-pattern . 1)))))

     ((and (not x) (or (cons 1 y) 2)) ((x . ((and-pattern . 0)
                                             (not-pattern . 0)))
                                       (y . ((and-pattern . 1)
                                             (or-pattern . 0 )
                                             (cons-pattern . 1))))))))

(test pattern-anonymize-variables.smoke
  "Smoke test for the `pattern-anonymize-variables' function."

  (mapc
   (make-pattern-transform-test-case-thunk 'pattern-anonymize-variables)

   ;; Input pattern          Expected transformed pattern
   '((1                      1)
     ('x                     'x)
     (:x                     :x)
     ('('x . y)              '('x . y))
     ('(:x . y)              '(:x . y))
     ('(1 . y)               '(1 . y))
     ('(x . y)               '(x . y))

     (x                      _)

     ((cons 'x y)            (cons 'x _))
     ((cons :x y)            (cons :x _))
     ((cons 1 y)             (cons 1 _))
     ((cons x y)             (cons _ _))
     ((cons '('x . y) z)     (cons '('x . y) _))
     ((cons '(:x . y) z)     (cons '(:x . y) _))
     ((cons '(1 . y) z)      (cons '(1 . y) _))
     ((cons '(x . y) z)      (cons '(x . y) _))

     ((class class (name x)) (class class (name _)))

     ((type integer)         :unchanged)
     ((guard x (consp x))    :unchanged)

     ((not x)                (not _))
     ((not 1)                (not 1))

     ((and x 1)              (and _ 1))

     ((or x 1)               (or _ 1)))))

(test make-predicate.smoke
  "Smoke test for the `make-predicate' function.

   Make sure compilation of the generated predicate form does not
   signal warnings about unused variables."

  (flet ((make-predicate-and-call (pattern object)
           (handler-case
               (funcall (make-predicate pattern) object)
             (warning (condition)
               (format nil "~@<~S signaled ~S: ~A~@:>"
                       'make-predicate (type-of condition) condition)))))
    (mapc
     (make-pattern-predicate-test-case-thunk #'make-predicate-and-call 1)

     ;; Pattern    Argument   Expected
     '((1          1          t)
       (1          2          nil)

       (x          1          t)

       ((cons x y) (cons 1 2) t)
       ((cons x y) 1          nil)

       ((not 1)    2          t)
       ((not 1)    1          nil)
       ((not x)    1          nil)

       ((and 1 2)  2          nil)
       ((and 1 2)  1          nil)
       ((and x 1)  1          t)
       ((and x 1)  2          nil)

       ((or 1 2)   2          t)
       ((or 1 2)   1          t)
       ((or x 1)   1          t)
       ((or x 1)   2          t)))))
