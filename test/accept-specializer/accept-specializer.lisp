;;;; accept-specializer.lisp --- Unit tests for the accept specializer.
;;;;
;;;; Copyright (C) 2013, 2014 Christophe Rhodes, Jan Moringen
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Christophe Rhodes <csr21@cantab.net>
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:accept-specializer.test)

(in-suite :specializable.accept-specializer)

;;; Respond test

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric respond (request)
    (:generic-function-class accept-generic-function)
    (:method-combination list)))

(defmethod respond list (request)
  t)
(defmethod respond list ((s string))
  'string)
(defmethod respond list ((s (accept "text/html")))
  "text/html")
(defmethod respond list ((s (accept "audio/mp3")))
  "audio/mp3")

(test respond

  (mapc
   (lambda (spec)
     (destructuring-bind (input expected) spec
       (is (equal expected (respond input)))))

   ;; TODO test syntax errors in accept-spec parsing
   '(;; Not known content types
     (:foo                              (t))
     (1                                 (t))
     ("bar"                             (string t))

     ;; One content type, with and without q
     ("*/*"                             ("audio/mp3" "text/html" string t)) ; since (string< "audio/mp3" "text/html")
     ("*/*;q=1.0"                       ("audio/mp3" "text/html" string t)) ; likewise
     ("text/*"                          ("text/html" string t))
     ("text/*;q=0.15"                   ("text/html" string t))
     ("text/html"                       ("text/html" string t))
     ("text/html;q=0.1"                 ("text/html" string t))
     ("audio/*"                         ("audio/mp3" string t))
     ("audio/*;q=0.01"                  ("audio/mp3" string t))
     ("audio/mp3"                       ("audio/mp3" string t))
     ("audio/mp3;q=0.1"                 ("audio/mp3" string t))

     ;; Multiple content types, with and without q
     ("text/html,audio/mp3"             ("text/html" "audio/mp3" string t)) ; since text/html earlier in accept string
     ("audio/mp3,text/html"             ("audio/mp3" "text/html" string t)) ; opposite
     ("text/html;q=0.1,audio/mp3"       ("audio/mp3" "text/html" string t))
     ("text/html,audio/mp3;q=0.2"       ("text/html" "audio/mp3" string t))
     ("text/html;q=0.1,audio/mp3;q=0.2" ("audio/mp3" "text/html" string t))
     ("audio/mp3;q=0.2,text/html;q=0.1" ("audio/mp3" "text/html" string t))
     ("text/html;q=0.2,audio/mp3;q=0.1" ("text/html" "audio/mp3" string t))
     ("audio/mp3;q=0.1,text/html;q=0.2" ("text/html" "audio/mp3" string t))
     ("audio/*;q=0.1,text/html;q=0.2"   ("text/html" "audio/mp3" string t))
     ("audio/*;q=0.2,text/html;q=0.1"   ("audio/mp3" "text/html" string t)))))

;;; Content negotiation test

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric cn-test (request)
    (:generic-function-class accept-generic-function)
    (:method-combination content-negotiation)))

(defmethod cn-test ((request string))
  (when (string= request "image/webp")
    :string))
(defmethod cn-test ((request (accept "text/html")))
  :html)
(defmethod cn-test ((request (accept "text/plain")))
  :plain)
(defmethod cn-test :around ((request (accept "text/plain")))
  (list :around-plain (call-next-method)))
(defmethod cn-test ((request (accept "image/webp")))
  (call-next-method))
(defmethod cn-test ((request (accept "audio/mp3")))
  :mp3)
(defvar *cn-test-after*)
(defmethod cn-test :after ((request float))
  (push :after-float *cn-test-after*))
(defmethod cn-test :after ((request (accept "text/html")))
  (push :after-html *cn-test-after*))

(test content-negotiation

  (mapc
   (lambda (spec)
     (destructuring-bind (input expected) spec
       (flet ((call ()
                (let ((*cn-test-after* '()))
                  (list (funcall #'cn-test input) *cn-test-after*))))
         (case expected
           (sb-pcl::no-applicable-method-error
            (signals sb-pcl::no-applicable-method-error (call)))
           (sb-pcl::long-method-combination-error
            (signals sb-pcl::long-method-combination-error (call)))
           (t
            (let ((result (call)))
              (is (equal expected result)
                  "~@<Calling ~S with argument ~S returned ~S and ~
                   pushed onto ~S the values ~:S (expected return ~
                   value: ~S; expected value of ~S: ~:S)~@:>"
                  'cn-text input
                  (first result) '*cn-test-after* (second result)
                  (first expected) '*cn-test-after* (second expected))))))))

   '(;; Not known content types
     (:foo                              sb-pcl::no-applicable-method-error)
     (1                                 sb-pcl::no-applicable-method-error)
     (1.0f0                             sb-pcl::long-method-combination-error) ; no primary method

     ;; STRING specializer
     ("bar"                             (nil                    ()))
     ("image/webp"                      (:string                ())) ; via CALL-NEXT-METHOD

     ;; One content type, with and without q
     ("*/*"                             ((:around-plain :mp3)   (:after-html))) ; because "audio/mp3" STRING< everything else
     ("*/*;q=1.0"                       ((:around-plain :mp3)   (:after-html))) ; likewise
     ("text/*"                          ((:around-plain :html)  (:after-html))) ; because (string< "text/html" "text/plain")
     ("text/*;q=0.15"                   ((:around-plain :html)  (:after-html))) ; likewise
     ("text/plain"                      ((:around-plain :plain) ()))
     ("text/plain;q=0.1"                ((:around-plain :plain) ()))
     ("audio/*"                         (:mp3                   ()))
     ("audio/*;q=0.01"                  (:mp3                   ()))
     ("audio/mp3"                       (:mp3                   ()))
     ("audio/mp3;q=0.1"                 (:mp3                   ()))

     ;; Multiple content types, with and without q
     ("text/html,audio/mp3"             (:html                  (:after-html))) ; because "text/html" earlier in accept string
     ("text/html;q=0.1,audio/mp3"       (:mp3                   (:after-html)))
     ("text/html,audio/mp3;q=0.2"       (:html                  (:after-html)))
     ("text/html;q=0.1,audio/mp3;q=0.2" (:mp3                   (:after-html)))
     ("audio/mp3;q=0.2,text/html;q=0.1" (:mp3                   (:after-html)))
     ("text/html;q=0.2,audio/mp3;q=0.1" (:html                  (:after-html)))
     ("audio/mp3;q=0.1,text/html;q=0.2" (:html                  (:after-html)))
     ("audio/*;q=0.1,text/html;q=0.2"   (:html                  (:after-html)))
     ("audio/*;q=0.2,text/html;q=0.1"   (:mp3                   (:after-html))))))

;;; Content negotiation with or combination test

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric cn/or-test (request)
    (:generic-function-class accept-generic-function)
    (:method-combination content-negotiation/or)))

(defvar *mp3-emittable-p* nil)

(defmethod cn/or-test or ((request (accept "audio/mp3")))
  (when *mp3-emittable-p*
    'mp3))
(defmethod cn/or-test or ((request (accept "image/webp")))
  'webp)
(defmethod cn/or-test :around ((request t))
  (list :around (call-next-method)))

(test content-negotiation/or

  (mapc
   (lambda (spec)
     (destructuring-bind (input mp3 expected) spec
       (let ((*mp3-emittable-p* mp3))
         (is (equal expected (cn/or-test input))))))

   '(("audio/mp3"            t   (:around mp3))
     ("audio/mp3;q=1.0"      nil (:around nil))
     ("image/webp"           t   (:around webp))
     ("audio/mp3,image/webp" nil (:around webp))
     ("audio/mp3,image/webp" t   (:around mp3)))))
