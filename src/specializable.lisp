;;; written by David Lichteblau, based on code by Christophe Rhodes,
;;; Closette, and SBCL
;;;
;;; http://www.lichteblau.com/git/?p=specializable.git;a=blob_plain;f=specializable.lisp;hb=eb30d235951c3c1d128811278760f1db36cd336c

(cl:in-package "SPECIALIZABLE")

;;; EXTENDED-SPECIALIZER protocol class

(defclass extended-specializer (sb-mop:specializer)
  ;; FIXME: this doesn't actually do quite what I wanted.
  ((direct-methods-table :allocation :class
                         :initform nil :accessor direct-methods-table)))

(defmethod print-object ((object extended-specializer) stream)
  (let ((syntax-name (specializer-syntax-name object)))
    (if (extended-specializer-name-p syntax-name)
        (let ((printer (extended-specializer-syntax-printer
                        (find-extended-specializer-syntax syntax-name))))
          (print-unreadable-object (object stream :type t :identity t)
            (funcall printer stream object)))
        (call-next-method))))

(defmethod sb-mop:add-direct-method ((specializer extended-specializer) method)
  (let* ((table (direct-methods-table specializer))
         (cell (assoc specializer table :test #'sb-pcl::same-specializer-p)))
    (unless cell
      (setf cell (cons specializer nil))
      (push cell (direct-methods-table specializer)))
    (push method (cdr cell))))

(defmethod sb-mop:remove-direct-method ((specializer extended-specializer) method)
  (let* ((table (direct-methods-table specializer))
         (cell (assoc specializer table :test #'sb-pcl::same-specializer-p)))
    (setf (cdr cell) (remove method (cdr cell)))))

(defmethod sb-mop:specializer-direct-methods ((specializer extended-specializer))
  (cdr (assoc specializer (direct-methods-table specializer)
              :test #'sb-pcl::same-specializer-p)))
(defmethod sb-mop:specializer-direct-generic-functions ((specializer extended-specializer))
  (remove-duplicates (mapcar #'sb-mop:method-generic-function (sb-mop:specializer-direct-methods specializer))))

(defclass specializable-generic-function (standard-generic-function)
  ((emf-table              :initform (make-hash-table :test 'equal)
                           :reader   emf-table)
   (disabled-optimizations :initarg  :disabled-optimizations
                           :initform ()
                           :reader   disabled-optimizations
                           :documentation
                           "Stores a list of keywords designating
                            optimizations that should be disabled (for
                            example for unit tests or performance
                            measurements).

                            Currently, the following designators are
                            recognized: :cacheing, :single-arg-cacheing,
                            :standard-discrimination."))
  (:metaclass sb-mop:funcallable-standard-class)
  (:default-initargs :method-class (find-class 'specializable-method)))

;;; TODO: we don't use this class yet, but we might do later
(defclass specializable-method (standard-method) ())

;;; from SBCL:

(defmethod sb-pcl:parse-specializer-using-class
    ((gf specializable-generic-function) (specializer-name sb-pcl:specializer))
  specializer-name)
(defmethod sb-pcl:parse-specializer-using-class
    ((gf specializable-generic-function) (specializer-name cons))
  (if (extended-specializer-name-p (car specializer-name))
      (make-extended-specializer gf specializer-name)
      (call-next-method)))

(defmethod sb-pcl:unparse-specializer-using-class
    ((gf specializable-generic-function) (specializer extended-specializer))
  (or (let ((syntax-name (specializer-syntax-name specializer)))
        (when (extended-specializer-name-p syntax-name)
          (let ((unparser (extended-specializer-syntax-unparser
                           (find-extended-specializer-syntax syntax-name))))
            `(,syntax-name ,@(funcall unparser gf specializer)))))
      (call-next-method)))

(defmethod sb-pcl:make-specializer-form-using-class or
    ((proto-generic-function specializable-generic-function)
     (proto-method standard-method)
     (specializer-name cons)
     environment)
  (when (extended-specializer-name-p (car specializer-name))
    `(make-extended-specializer
      (sb-pcl:class-prototype (find-class ',(type-of proto-generic-function)))
      ',specializer-name)))

;;; from Closette, changed to use some SBCL functions:

(defmethod generalizer-equal-hash-key
    ((gf specializable-generic-function) (g class))
  (sb-pcl::class-wrapper g))

(defmethod compute-argument-generalizing-function
    ((generic-function specializable-generic-function)
     arg-position)
  (declare (type fixnum arg-position))
  (lambda (object)
    (generalizer-of-using-class generic-function object arg-position)))

(defmethod compute-effective-arguments-function
    ((generic-function specializable-generic-function)
     num-required)
  nil)

(defun first-arg-only-special-case-p (gf)
  (let ((arg-info (sb-pcl::gf-arg-info gf)))
    (and (not (member :single-arg-cacheing (disabled-optimizations gf)
                      :test #'eq))
         (>= (sb-pcl::arg-info-number-required arg-info) 1)
         (every (lambda (x) (eql x t))
                (cdr (sb-pcl::arg-info-metatypes arg-info))))))

(defun method-specializers-standard-p (method)
  (flet ((standard-specializer-p (s)
           (typep s '(or class sb-mop:eql-specializer
                         sb-pcl::class-eq-specializer))))
    (every #'standard-specializer-p (sb-mop:method-specializers method))))

(defun only-has-standard-specializers-p (gf)
  (every #'method-specializers-standard-p
         (sb-mop:generic-function-methods gf)))

;;; FIXME: in some kind of order, the discriminating function needs to handle:
;;; - argument count checking;
;;; - DONE (in effective method) keyword argument validity;
;;; - DONE flushing the emf cache on method addition/removal
;;; - DONE (sort of, using wrappers/g-e-h-k) flushing the cache on class redefinition;
;;; - cache thread-safety.
;;; - speed
;;; - DONE (in SBCL itself) interaction with TRACE et al.
(defmethod sb-mop:compute-discriminating-function ((gf specializable-generic-function))
  ;; This method employs a number of optimizations by default that can
  ;; selectively be disabled by adding elements to
  ;; DISABLED-OPTIMIZATIONS in GF:
  ;;
  ;; :STANDARD-DISCRIMINATION
  ;;   Do not fall back to standard CLOS discrimination behavior if GF only
  ;;   has standard specializers.
  ;;
  ;; :SINGLE-ARG-CACHEING
  ;;   Do not employ optimized cacheing strategy if GF has one required
  ;;   parameter.
  ;;
  ;; :CACHEING
  ;;   Do not employ cacheing strategy based on generalizer hash keys.
  (when (and (not (member :standard-discrimination (disabled-optimizations gf)
                          :test #'eq))
             (only-has-standard-specializers-p gf))
    (return-from sb-mop:compute-discriminating-function (call-next-method)))

  ;; FIXME: this is not actually sufficient argument checking
  (let* ((num-required
          (sb-pcl::arg-info-number-required (sb-pcl::gf-arg-info gf)))
         (generalizer-makers
          (coerce (loop :for i :below num-required
                     :collect (compute-argument-generalizing-function gf i))
                  '(simple-array function (*))))
         (arguments-function
          (compute-effective-arguments-function gf num-required))
         (emf-table (emf-table gf)))
    (declare (type (integer 0 (#.lambda-parameters-limit)) num-required)
             (type (or null function) arguments-function)
             (type hash-table emf-table))
    (flet ((check-arguments (args)
             (declare (type list args))
             (when (< (length args) num-required)
               (error "Too few arguments to generic function ~S." gf)))
           (compute-generalizers (into args)
             (declare (type list into args))
             (loop
                :for i :of-type fixnum :from 0 :below num-required
                :for arg :in args
                :for cell :of-type cons :on into
                :do (setf (car cell)
                          (funcall (sb-ext:truly-the
                                    function
                                    (locally (declare (optimize (sb-c::insert-array-bounds-checks 0)))
                                      (svref (sb-ext:truly-the simple-array generalizer-makers) i)))
                                   arg))))
           (compute-hash-key (generalizer)
             (generalizer-equal-hash-key gf generalizer))
           (effective-arguments (generalizers args)
             (declare (type list generalizers args))
             (locally (declare (function arguments-function))
               (funcall arguments-function args generalizers))))
      (cond
        ;; Disable all cacheing: always use
        ;; SLOW-METHOD-LOOKUP-AND-CALL.
        ((member :cacheing (disabled-optimizations gf)
                 :test #'eq)
         (lambda (&rest args)
           (check-arguments args)
           (let ((generalizers (make-list num-required)))
             (compute-generalizers generalizers args)
             (slow-method-lookup-and-call
              gf (if arguments-function
                     (effective-arguments generalizers args)
                     args)
              generalizers))))
        ;; One argument special case: avoid consing up lists of
        ;; generalizers and generalizers hash keys.
        ((first-arg-only-special-case-p gf)
         (let ((generalizer-maker (aref generalizer-makers 0)))
           (declare (type function generalizer-maker))
           (lambda (&rest args)
             (check-arguments args)
             (let* ((generalizer (funcall generalizer-maker (first args)))
                    (all-generalizers)
                    (key (generalizer-equal-hash-key gf generalizer))
                    (emfun (gethash key emf-table nil)))
               (flet ((all-generalizers ()
                        (cond
                          (all-generalizers)
                          ((= 1 num-required)
                           (setf all-generalizers (list generalizer)))
                          (t
                           (setf all-generalizers (make-list num-required))
                           (setf (car all-generalizers) generalizer)
                           (compute-generalizers (rest all-generalizers) (rest args))
                           all-generalizers))))
                 (let ((effective-args (if arguments-function
                                           (effective-arguments (all-generalizers) args)
                                           args)))
                   (if emfun
                       (sb-pcl::invoke-emf emfun effective-args)
                       (slow-method-lookup-and-call
                        gf effective-args (all-generalizers)))))))))
        ;; General case with cacheing: compute a list of generalizers
        ;; and generalizer hash keys.
        (t
         (lambda (&rest args)
           (check-arguments args)
           (let ((generalizers (make-list num-required))
                 (keys (make-list num-required))) ; TODO would a vector be faster?
             (declare (sb-int:truly-dynamic-extent generalizers keys))
             (compute-generalizers generalizers args)
             (map-into keys #'compute-hash-key generalizers)
             (let ((emfun (gethash keys emf-table nil))
                   (effective-args (if arguments-function
                                       (effective-arguments generalizers args)
                                       args)))
               (if emfun
                   (sb-pcl::invoke-emf emfun effective-args)
                   (slow-method-lookup-and-call
                    gf effective-args generalizers))))))))))

(defmethod reinitialize-instance :after ((gf specializable-generic-function) &key)
  (clrhash (emf-table gf)))

(defun slow-method-lookup (gf args generalizers)
  (multiple-value-bind (applicable-methods definitivep)
      (compute-applicable-methods-using-generalizers gf generalizers)
    (unless definitivep
      (setf applicable-methods (compute-applicable-methods gf args)))
    (values (compute-effective-method-function gf applicable-methods)
            definitivep)))

(defun slow-method-lookup-and-call (gf args generalizers)
  (multiple-value-bind (emf cacheablep)
      (slow-method-lookup gf args generalizers)
    (when cacheablep
      (let* ((keys (mapcar (lambda (x) (generalizer-equal-hash-key gf x)) generalizers))
             (key (if (first-arg-only-special-case-p gf) (car keys) keys)))
        (setf (gethash key (emf-table gf)) emf)))
    (sb-pcl::invoke-emf emf args)))

(defun compute-effective-method-function (gf methods)
  (if (null methods)
      (lambda (&rest args) (apply #'no-applicable-method gf args))
      (let* ((mc (sb-mop:generic-function-method-combination gf))
             (sb-pcl::*applicable-methods* methods)
             (em (sb-mop:compute-effective-method gf mc methods)))
        (sb-pcl::make-effective-method-function gf em))))

(defmethod generalizer-of-using-class ((generic-function specializable-generic-function)
                                       object arg-position)
  (class-of object))

(defmethod specializer-accepts-generalizer-p
    ((gf specializable-generic-function) (specializer class) (generalizer class))
  (if (subtypep generalizer specializer)
      (values t t)
      (values nil t)))
(defmethod specializer-accepts-generalizer-p
    ((gf specializable-generic-function) (specializer sb-mop:eql-specializer) (generalizer class))
  (if (eq generalizer (class-of (sb-mop:eql-specializer-object specializer)))
      (values t nil)
      (values nil t)))

(defmethod specializer-accepts-p ((specializer class) object)
  (typep object specializer))
(defmethod specializer-accepts-p ((specializer sb-mop:eql-specializer) object)
  (eq object (sb-mop:eql-specializer-object specializer)))

(defmethod compute-applicable-methods-using-generalizers
    ((gf specializable-generic-function) generalizers)
  (let ((methods (sb-mop:generic-function-methods gf))
        (result-definitive-p t))
    (labels ((accepts-p (specializer generalizer)
               (multiple-value-bind (acceptsp definitivep)
                   (specializer-accepts-generalizer-p
                    gf specializer generalizer)
                 (unless definitivep
                   (setf result-definitive-p nil))
                 acceptsp))
             (applicable-p (method)
               (every #'accepts-p
                      (sb-mop:method-specializers method) generalizers))
             (method< (method1 method2)
               (method-more-specific-p gf method1 method2 generalizers)))
      (declare (dynamic-extent #'accepts-p #'applicable-p #'method<))
      (values (sort (copy-list (remove-if-not #'applicable-p methods)) #'method<)
              result-definitive-p))))

(defmethod compute-applicable-methods
    ((gf specializable-generic-function) arguments)
  (let* ((num-required (sb-pcl::arg-info-number-required (sb-pcl::gf-arg-info gf))) ; TODO duplicated above
         (methods      (sb-mop:generic-function-methods gf))
         (generalizers (loop
                          :for i :of-type fixnum :from 0 :below num-required
                          :for argument :in arguments
                          :collect (generalizer-of-using-class gf argument i))))
    (flet ((applicable-p (method)
             (every #'specializer-accepts-p
                    (sb-mop:method-specializers method) arguments))
           (method< (method1 method2)
             (method-more-specific-p gf method1 method2 generalizers)))
      (declare (dynamic-extent #'applicable-p #'method<))
      (sort (copy-list (remove-if-not #'applicable-p methods))
            #'method<))))

(defun method-more-specific-p (gf method1 method2 generalizers)
  (let ((lambda-list   (sb-mop:generic-function-lambda-list gf))
        (specializers1 (sb-mop:method-specializers method1))
        (specializers2 (sb-mop:method-specializers method2)))
    (loop
       :for arg :in (sb-mop:generic-function-argument-precedence-order gf)
       :for index = (position arg lambda-list)
       :for generalizer :in generalizers
       :do (let ((specializer1 (nth index specializers1))
                 (specializer2 (nth index specializers2)))
             (ecase (specializer< gf specializer1 specializer2 generalizer)
               (<         (return t))
               (=)
               ((> /= //) (return nil)))))))

(defmethod specializer< ((gf specializable-generic-function)
                         (s1 class)
                         (s2 class)
                         (generalizer class))
  (let ((cpl))
    (flet ((cpl ()
             (or cpl
                 (setf cpl (sb-mop:class-precedence-list
                            (sb-pcl:ensure-class-finalized generalizer))))))
      (cond
        ((eq s1 s2)
         '=)
        ((find s2 (cdr (member s1 (cpl))))
         '<)
        ((find s1 (cdr (member s2 (cpl))))
         '>)
        (t
         '/=)))))

(defmethod specializer< ((gf specializable-generic-function)
                         (s1 sb-mop:eql-specializer)
                         (s2 sb-mop:eql-specializer)
                         generalizer)
  (declare (ignore generalizer))
  (if (eq (sb-mop:eql-specializer-object s1) (sb-mop:eql-specializer-object s2))
      '=
      '//))

(defmethod specializer< ((gf specializable-generic-function)
                         (s1 sb-mop:eql-specializer)
                         (s2 class)
                         generalizer)
  (let* ((class (class-of (sb-mop:eql-specializer-object s1)))
         (relation (specializer< gf class s2 generalizer)))
    (case relation
      ((= <) '<)
      (t     '//))))

(defmethod specializer< ((gf specializable-generic-function)
                         (s1 class)
                         (s2 sb-mop:eql-specializer)
                         generalizer)
  (invert-specializer<-relation (specializer< gf s2 s1 generalizer)))
