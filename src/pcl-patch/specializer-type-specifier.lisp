;;;; specializer-type-specifier.lisp --- Hot-patch for SBCL's PCL variant.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:sb-pcl)

;;; Specializer type specifier protocol

(export 'specializer-type-specifier)

(defgeneric specializer-type-specifier (specializer)
  (:documentation
   ""))

(defmethod specializer-type-specifier ((specializer specializer))
  ;; TODO protocol-not-implemented-error
  (error "~@<No method on ~S for specializer ~S~@:>"
         'specializer-type-specifier specializer))

(defmethod specializer-type-specifier ((class structure-class))
  (class-name class))

(defmethod specializer-type-specifier ((class system-class))
  (class-name class))

(defmethod specializer-type-specifier ((specializer class))
  (let ((name (class-name specializer)))
    (when (and (typep name '(and symbol (not null)))
               (eq specializer (find-class name nil)))
      (ecase (info :type :kind name)
        (:primitive name)
        (:defined
         (assert "should not happen"))
        ((:instance :forthcoming-defclass-type)
         ;; CLOS classes are here but are too expensive to check (as
         ;; opposed to STRUCTURE-CLASS and SYSTEM-CLASS) which have
         ;; their own methods and thus are not handled here.
         nil)))))

(defmethod specializer-type-specifier ((specializer class-eq-specializer))
  (specializer-type-specifier (specializer-class specializer)))

(defmethod specializer-type-specifier ((specializer eql-specializer))
  `(eql ,(eql-specializer-object specializer)))

;; Hook into PCL (comments have been removed)

(defun parameter-specializer-declaration-in-defmethod
    (generic-function parameter specializer specials env)
  (flet ((declare-type (type)
           (return-from parameter-specializer-declaration-in-defmethod
             (case type
               ((nil) '(ignorable))
               (t     `(type ,type ,parameter)))))
         (warn-parse (&optional condition)
           (style-warn
            "~@<cannot parse specializer ~S in TODO~@[: ~A~].~@:>"
            specializer condition))
         (warn-find ()
           (style-warn
            "~@<can't find type for specializer ~S in ~S.~@:>" ; TODO reference function and method? proper warning condition
            specializer 'parameter-specializer-declaration-in-defmethod)))
   (cond
     ((not (eq **boot-state** 'complete))
      (declare-type nil))

     ((or (var-special-p parameter env) (member parameter specials))
      (declare-type nil))

     ((eq specializer 'slot-object)
      (declare-type nil))
     ((typep specializer 'symbol)
      (let ((class (find-class specializer nil)))
        (if class
            (declare-type (specializer-type-specifier class))
            (warn-find))))
     ((typep specializer '(cons (eql eql)))
      (declare-type nil))
     ((typep specializer '(or symbol cons))
      (let ((specializer (handler-case
                             (parse-specializer-using-class
                              generic-function specializer)
                           (error (condition)
                             (warn-parse condition)
                             (declare-type nil)))))
        (declare-type (specializer-type-specifier specializer)))) ; TODO can signal an error. is that ok?

     ((typep specializer '(or class eql-specializer class-eq-specializer specializer)) ; TODO simplify?
      (declare-type (specializer-type-specifier specializer)))

     (t
      (warn-find)
      (declare-type nil)))))

;; only change here is passing the generic function to
;; PARAMETER-SPECIALIZER-DECLARATION-IN-DEFMETHOD.
(defun make-method-lambda-internal (proto-gf proto-method method-lambda env)
  (declare (ignore proto-method))
  (unless (and (consp method-lambda) (eq (car method-lambda) 'lambda))
    (error "The METHOD-LAMBDA argument to MAKE-METHOD-LAMBDA, ~S, ~
            is not a lambda form."
           method-lambda))
  (multiple-value-bind (real-body declarations documentation)
      (parse-body (cddr method-lambda))
    (let* ((method-name *method-name*)
           (method-lambda-list *method-lambda-list*)
           (*method-name* nil)
           (*method-lambda-list* nil)
           (generic-function-name (when method-name (car method-name)))
           (specialized-lambda-list (or method-lambda-list
                                        (ecase (car method-lambda)
                                          (lambda (second method-lambda))
                                          (named-lambda (third method-lambda)))))
           (method-cell (list (make-symbol "METHOD-CELL"))))
      (multiple-value-bind (parameters lambda-list specializers)
          (parse-specialized-lambda-list specialized-lambda-list)
        (let* ((required-parameters
                (mapcar (lambda (r s) (declare (ignore s)) r)
                        parameters
                        specializers))
               (slots (mapcar #'list required-parameters))
               (class-declarations
                `(declare
                  ,@(remove nil
                            (mapcar (lambda (a s) (and (symbolp s)
                                                       (neq s t)
                                                       `(%class ,a ,s)))
                                    parameters
                                    specializers))
                  ,@(let ((specials (declared-specials declarations)))
                         (mapcar (lambda (par spec)
                                   (parameter-specializer-declaration-in-defmethod
                                    proto-gf par spec specials env))
                                 parameters
                                 specializers))))
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
                           (or (typep constant-value
                                      '(or number character))
                               (and (symbolp constant-value)
                                    (symbol-package constant-value)))
                           (list :constant-value constant-value)))
               (applyp (dolist (p lambda-list nil)
                         (cond ((memq p '(&optional &rest &key))
                                (return t))
                               ((eq p '&aux)
                                (return nil))))))
          (multiple-value-bind
                (walked-lambda call-next-method-p closurep
                               next-method-p-p setq-p
                               parameters-setqd)
              (walk-method-lambda method-lambda
                                  required-parameters
                                  env
                                  slots)
            (multiple-value-bind (walked-lambda-body
                                  walked-declarations
                                  walked-documentation)
                (parse-body (cddr walked-lambda))
              (declare (ignore walked-documentation))
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
                                           :next-method-p-p ,next-method-p-p
                                           :setq-p ,setq-p
                                           :parameters-setqd ,parameters-setqd
                                           :method-cell ,method-cell
                                           :closurep ,closurep
                                           :applyp ,applyp)
                           ,@walked-declarations
                           (locally
                               (declare (disable-package-locks
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
                          ,@(when documentation `(:documentation ,documentation)))))))))))
