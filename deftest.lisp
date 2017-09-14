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



;; DESIRED SYNTAX:
;; 
;;  (deftest function-or-macro-symbol (bind values to)
;;    ... do stuff ...
;;
;;
;;
;;
;;
    
