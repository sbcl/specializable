;;;; specializer-type-specifier.lisp --- Hot-patch for SBCL's PCL variant.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:sb-pcl)

;;; Specializer type specifier protocol
;;;
;;; Produces type declarations for method parameters based on
;;; non-parsed or parsed specializers.

(export 'specializer-type-specifier)

;; TODO maybe should not depend on method
(defgeneric specializer-type-specifier (proto-generic-function proto-method specializer)
  #+sb-doc
  (:documentation
   "Return a type specifier for SPECIALIZER, a non-parsed specializer
    form or a SPECIALIZER instance.

    SPECIALIZER can be
    * a non-parsed specializer form such as
      * a symbol naming a class
      * a list of the form (eql OBJECT)
      * a list of the form (SPECIALIZER-KIND &rest SPECIFIC-SYNTAX)
    * an instance of a subclass of SPECIALIZER

    When SPECIALIZER cannot be parsed/used as a specializer for
    PROTO-GENERIC-FUNCTION and PROTO-METHOD, a STYLE-WARNING is
    signaled and nil returned. No type declaration will be generated
    in this case.

    nil can also be returned if SPECIALIZER is valid but its type
    should not be declared, for example for efficiency reasons."))

(defmethod specializer-type-specifier ((proto-generic-function standard-generic-function)
                                       (proto-method standard-method)
                                       (specializer specializer))
  ;; TODO protocol-not-implemented-error
  (error "~@<No method on ~S for specializer ~S~@:>"
         'specializer-type-specifier specializer))

(labels ((warn-parse (specializer &optional condition)
           (style-warn
            "~@<Cannot parse specializer ~S in ~S~@[: ~A~].~@:>"
            specializer 'specializer-type-specifier condition))
         (warn-find (specializer)
           (style-warn
            "~@<Cannot find type for specializer ~S in ~S.~@:>" ; TODO reference function and method? proper warning condition
            specializer 'specializer-type-specifier))
         (class-name-type-specifier (name)
           (case (info :type :kind name)
             (:primitive
              name)
             (:defined
              (assert "should not happen"))
             ((:instance :forthcoming-defclass-type)
              ;; CLOS classes are here but are too expensive to check (as
              ;; opposed to STRUCTURE-CLASS and SYSTEM-CLASS) which have
              ;; their own methods and thus are not handled here.
              nil)
             (t
              (warn-find name)))))

  ;; Non-parsed class specializers, i.e. class names
  ;;
  ;; Extended generic function classes with specializers which are
  ;; designated by symbols have to install their own methods
  ;; specialized on symbol to replace this logic.

  (defmethod specializer-type-specifier ((proto-generic-function standard-generic-function)
                                         (proto-method standard-method)
                                         (specializer symbol))
    (handler-case
        ;; Usually tries to find the class named SPECIALIZER. Can do
        ;; something different when there is a non-default method on
        ;; PARSE-SPECIALIZER-USING-CLASS.
        (let ((specializer (parse-specializer-using-class
                            proto-generic-function specializer)))
          (specializer-type-specifier
           proto-generic-function proto-method (specializer-class specializer)))
      (class-not-found-error ()
        ;; SPECIALIZER does not name a class, but maybe it is known to
        ;; name a :forthcoming-defclass-type.
        ;; CLASS-NAME-TYPE-SPECIFIER will emit the warning and return
        ;; nil if not.
        (class-name-type-specifier specializer))
      (error (condition) ; TODO should not happen
        (warn-parse specializer condition)
        nil)))

  ;; Non-parsed extended specializer with default syntax
  ;; i.e. (SPECIALIZER-KIND &rest SPECIFIC-SYNTAX)

  (defmethod specializer-type-specifier ((proto-generic-function standard-generic-function)
                                         (proto-method standard-method)
                                         (specializer t))
    (handler-case
        (let ((specializer (parse-specializer-using-class
                            proto-generic-function specializer)))
          (specializer-type-specifier
           proto-generic-function proto-method specializer))
      (error (condition)
        ;; This can happen, for example, if SPECIALIZER does not
        ;; designate any extended specializer or if it does but then
        ;; does not conform to respective extended specializer syntax.
        (warn-parse specializer condition)
        nil)))

  ;; Parsed eql and class-eq specializers

  (defmethod specializer-type-specifier ((generic-function standard-generic-function)
                                         (proto-method standard-method)
                                         (specializer class-eq-specializer))
    (specializer-type-specifier
     generic-function proto-method (specializer-class specializer)))

  (defmethod specializer-type-specifier ((generic-function standard-generic-function)
                                         (proto-method standard-method)
                                         (specializer eql-specializer))
    `(eql ,(eql-specializer-object specializer)))

  ;; Parsed class specializers

  (defmethod specializer-type-specifier ((generic-function standard-generic-function)
                                         (proto-method standard-method)
                                         (class structure-class))
    (class-name class))

  (defmethod specializer-type-specifier ((generic-function standard-generic-function)
                                         (proto-method standard-method)
                                         (class system-class))
    (class-name class))

  (defmethod specializer-type-specifier ((generic-function standard-generic-function)
                                         (proto-method standard-method)
                                         (specializer class))
    (let ((name (class-name specializer)))
      (when (and (typep name '(and symbol (not null)))
                 (eq specializer (find-class name nil)))
        (class-name-type-specifier name)))))

;; Hook into PCL (comments have been removed)

(define-condition class-name-condition (condition)
  ((name :initarg :name
         :reader  class-name-condition-name))
  (:default-initargs
   :name (missing-arg)))

(define-condition class-not-found-error (error
                                         class-name-condition)
  ((name :type (satisfies legal-class-name-p)))
  (:report (lambda (condition stream)
             (format stream "There is no class named ~
                             ~/sb-impl::print-symbol-with-prefix/."
                     (class-name-condition-name condition)))))

(define-condition illegal-class-name-error (error
                                            class-name-condition)
  ()
  (:report (lambda (condition stream)
             (format stream "~S is not a legal class name."
                     (class-name-condition-name condition)))))

(defun find-class-from-cell (name cell &optional (errorp t))
  (or (when cell
        (or (classoid-cell-pcl-class cell)
            (when *create-classes-from-internal-structure-definitions-p*
              (let ((classoid (classoid-cell-classoid cell)))
                (when (and classoid
                           (or (condition-classoid-p classoid)
                               (defstruct-classoid-p classoid)))
                  (ensure-non-standard-class name classoid))))))
      (cond ((null errorp) nil)
            ((legal-class-name-p name)
             (error 'class-not-found-error :name name))
            (t
             (error 'illegal-class-name-error :name name)))))

;; This function operates on
;; * non-parsed specializers, i.e. class names and extended
;;   specializer syntaxes
;; * parsed specializers, i.e. CLASSes, EQL-SPECIALIZERs,
;;   CLASS-EQ-SPECIALIZERs and generic SPECIALIZERs
(defun parameter-specializer-declaration-in-defmethod
    (proto-generic-function proto-method parameter specializer specials env)
  (flet ((declare-type (type)
           (return-from parameter-specializer-declaration-in-defmethod
             (case type
               ((nil) '(ignorable))
               (t     `(type ,type ,parameter))))))
    (cond
      ((not (eq **boot-state** 'complete))
       (declare-type nil))

      ;; Independent of SPECIALIZER, bail out if the PARAMETER is
      ;; known to be a special variable.
      ((or (var-special-p parameter env) (member parameter specials))
       (declare-type nil))

      ;; Bail out on SLOT-OBJECT special case and unparsed
      ;; EQL-specializers. TODO why the latter?
      ((eq specializer 'slot-object)
       (declare-type nil))
      ((typep specializer '(cons (eql eql)))
       (declare-type nil))

      ;; Parsed specializer objects, i.e. CLASS, EQL-SPECIALIZER,
      ;; CLASS-EQ-SPECIALIZER and generic SPECIALIZER.
      ;;
      ;; Also unparsed specializers other than EQL: these have to be
      ;; either class names or extended specializers.
      (t
       (declare-type (specializer-type-specifier
                      proto-generic-function proto-method specializer)))))) ; TODO can signal an error. is that ok?

(defun make-method-lambda-internal (proto-gf proto-method method-lambda env)
  (unless (typep method-lambda '(cons (eql lambda)))
    (error "The METHOD-LAMBDA argument to MAKE-METHOD-LAMBDA, ~S, ~
            is not a lambda form."
           method-lambda))

  (binding* (((real-body declarations documentation)
              (parse-body (cddr method-lambda)))
             (method-name *method-name*)
             (method-lambda-list *method-lambda-list*)
             ;; Macroexpansion caused by code-walking may call make-method-lambda and
             ;; end up with wrong values
             (*method-name* nil)
             (*method-lambda-list* nil)
             (generic-function-name (when method-name (car method-name)))
             (specialized-lambda-list (or method-lambda-list
                                          (ecase (car method-lambda)
                                            (lambda (second method-lambda))
                                            (named-lambda (third method-lambda)))))
             ;; the method-cell is a way of communicating what method a
             ;; method-function implements, for the purpose of
             ;; NO-NEXT-METHOD.  We need something that can be shared
             ;; between function and initargs, but not something that
             ;; will be coalesced as a constant (because we are naughty,
             ;; oh yes) with the expansion of any other methods in the
             ;; same file.  -- CSR, 2007-05-30
             (method-cell (list (make-symbol "METHOD-CELL")))
             ((parameters lambda-list specializers)
              (parse-specialized-lambda-list specialized-lambda-list))
             (required-parameters (subseq parameters 0 (length specializers)))
             (slots (mapcar #'list required-parameters))
             (class-declarations
              `(declare
                ;; These declarations seem to be used by PCL to pass
                ;; information to itself; when I tried to delete 'em
                ;; ca. 0.6.10 it didn't work. I'm not sure how
                ;; they work, but note the (VAR-DECLARATION '%CLASS ..)
                ;; expression in CAN-OPTIMIZE-ACCESS1. -- WHN 2000-12-30
                ,@(remove nil (mapcar (lambda (a s)
                                        (and (symbolp s) (neq s t)
                                             `(%class ,a ,s)))
                                      parameters specializers))
                ,@(let ((specials (declared-specials declarations)))
                    (mapcar (lambda (par spec)
                              (parameter-specializer-declaration-in-defmethod
                               proto-gf proto-method par spec specials env))
                            parameters specializers))))
             (method-lambda
              `(lambda ,lambda-list
                 (declare (ignorable ,@required-parameters))
                 ,class-declarations
                 ,@declarations
                 (block ,(fun-name-block-name generic-function-name)
                   ,@real-body)))
             (constant-value-p (and (null (cdr real-body))
                                    (constantp (car real-body))))
             (constant-value (and constant-value-p
                                  (constant-form-value (car real-body))))
             (plist (and constant-value-p
                         (or (typep constant-value '(or number character))
                             (and (symbolp constant-value)
                                  (symbol-package constant-value)))
                         (list :constant-value constant-value)))
             (applyp (dolist (p lambda-list nil)
                       (cond ((memq p '(&optional &rest &key))
                              (return t))
                             ((eq p '&aux)
                              (return nil)))))
             ((walked-lambda call-next-method-p setq-p parameters-setqd)
              (walk-method-lambda
               method-lambda required-parameters env slots))
             ((walked-lambda-body walked-declarations)
              (parse-body (cddr walked-lambda))))
    (when (some #'cdr slots)
      (let ((slot-name-lists (slot-name-lists-from-slots slots)))
        (setq plist
              `(,@(when slot-name-lists
                        `(:slot-name-lists ,slot-name-lists))
                  ,@plist))
        (setq walked-lambda-body
              `((pv-binding (,required-parameters
                             ,slot-name-lists
                             (load-time-value
                              (intern-pv-table
                               :slot-name-lists ',slot-name-lists)))
                  ,@walked-lambda-body)))))
    (when (and (memq '&key lambda-list)
               (not (memq '&allow-other-keys lambda-list)))
      (let ((aux (memq '&aux lambda-list)))
        (setq lambda-list (nconc (ldiff lambda-list aux)
                                 (list '&allow-other-keys)
                                 aux))))
    (values `(lambda (.method-args. .next-methods.)
               (simple-lexical-method-functions
                   (,lambda-list .method-args. .next-methods.
                                 :call-next-method-p
                                 ,(when call-next-method-p t)
                                 :setq-p ,setq-p
                                 :parameters-setqd ,parameters-setqd
                                 :method-cell ,method-cell
                                 :applyp ,applyp)
                 ,@walked-declarations
                 (locally (declare (disable-package-locks
                                    %parameter-binding-modified))
                   (symbol-macrolet ((%parameter-binding-modified
                                      ',@parameters-setqd))
                     (declare (enable-package-locks
                               %parameter-binding-modified))
                     ,@walked-lambda-body))))
            `(,@(when call-next-method-p `(method-cell ,method-cell))
              ,@(when (member call-next-method-p '(:simple nil))
                  '(simple-next-method-call t))
              ,@(when plist `(plist ,plist))
              ,@(when documentation `(:documentation ,documentation))))))
