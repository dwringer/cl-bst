;;;; DEFTEST.LISP - Lisp macro-based testing framework (optimized for SBCL)
;;;; Copyright 2017 Darren W. Ringer <dwringer@gmail.com>

;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation
;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use,
;;;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following
;;;; conditions:

;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.

;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.
(defpackage :deftest
  (:use :common-lisp)
  (:export :deftests
	   :test-inst
	   :test-post-method
	   :test-pre-method
	   :run-tests))
(in-package :deftest)

(defmacro without-style-warnings (&rest body)
  `(locally
       (declare #+sbcl(sb-ext:muffle-conditions style-warning))
     (handler-bind
	 (#+sbcl(style-warning #'muffle-warning))
       ,@body)))

(defmacro test-inst (name args return-values-as &rest body)
  `(multiple-value-bind ,return-values-as (eval (cons ',name ',args))
     ,@body))

(defmacro test-post-method (name inst args inst-as return-values-as &rest body)
  `(without-style-warnings
       (let ((,inst-as ,inst))
	 (multiple-value-bind ,return-values-as
	     (eval (cons ',name (cons ',inst ',args)))
	   ,@body))))
     
(defmacro test-pre-method (name args inst inst-as return-values-as &rest body)
  `(without-style-warnings
       (let ((,inst-as ,inst))
	 (multiple-value-bind ,return-values-as
	     (eval (concatenate 'list
				(list ',name)
				',args
				(list ,inst-as)))
	   ,@body))))

(defparameter *tests* nil)

(defmacro deftests (&rest tests)
  (map nil #'(lambda (tst)
	       (push tst *tests*))
       tests))

(defun run-tests ()
  (map nil #'(lambda (tst i)
	       (format t "Running test ~A..." i)
	       (when (null (eval tst))
		 (format t "OK~&")))
       (reverse *tests*)
       (do* ((i 1 (+ i 1))
	     (acc (list i) (cons i acc)))
	    ((> i (length *tests*)) (reverse acc))))
  (setf *tests* nil))
