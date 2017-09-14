;;;; FINITE-MAP.LISP - Finite Map implemented with BST template macro
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
(load "bst.lisp")

(defpackage :finite-map
  (:nicknames :fm)
  (:use :common-lisp
	:bst)
  (:export :make-finite-map
	   :bst
	   :finite-map-bind
	   :finite-map-lookup
	   :finite-map-key-not-found))
(in-package :finite-map)

(defgeneric finite-map-bind (k v fm))
(defgeneric finite-map-lookup (k fm))

(define-condition finite-map-key-not-found (error) (e))

(defmacro make-finite-map (&key
 			     (key-element-type 'string)
 			     (value-element-type 't)
 			     (test #'string<)
			     (overwrites t))
  (let* ((id (string (gensym "")))
	 (struct
	  (intern (concatenate 'string "FINITE-MAP-" id)))
 	 (key-type
	  (intern (concatenate 'string "FINITE-MAP-KEY-" id)))
 	 (value-type
	  (intern (concatenate 'string "FINITE-MAP-VALUE-" id)))
 	 (record-struct
	  (intern (concatenate 'string "FINITE-MAP-RECORD-" id)))
	 (constructor
	  (intern (concatenate 'string "MAKE-FINITE-MAP-" id)))
	 (record-constructor
	  (intern (concatenate 'string "MAKE-FINITE-MAP-RECORD-" id))))
    `(progn
       (defstruct ,struct bst)
       (deftype ,key-type () '(or null ,key-element-type))
       (deftype ,value-type () '(or null ,value-element-type))
       (defstruct ,record-struct
 	 (key nil :type ,key-type)
 	 (value nil :type ,value-element-type))

       (defmethod finite-map-bind ((k ,key-element-type)
				   (v ,value-element-type)
				   (fm ,struct))
	 (,constructor
	  :bst (bst-insert (,record-constructor :key k :value v)
			   (slot-value fm 'bst))))

       (defmethod finite-map-lookup ((k ,key-element-type)
				     (fm ,struct))
	 (do* ((branch nil)
	       (next-rec nil (slot-value branch 'value))
	       (next-key nil (when (not (null next-rec))
			       (slot-value next-rec 'key)))
	       (next-key-not-null nil (not (null next-key)))
	       (cmp-lt nil (when next-key-not-null
			     (funcall ,test k next-key)))
	       (cmp-gt nil (when next-key-not-null
			     (funcall ,test next-key k))))
	      ((not (or (null next-key) cmp-lt cmp-gt))
	       (slot-value next-rec 'value))
	   (if (null branch)
	       (setf branch (slot-value fm 'bst))
	       (progn
		 (when (null next-key)
		   (error 'finite-map-key-not-found k))
		 (cond (cmp-lt
			(let ((next-left (slot-value branch 'left)))
			  (if (null next-left)
			      (error 'finite-map-key-not-found)
			      (setf branch next-left))))
		       (cmp-gt
			(let ((next-right (slot-value branch 'right)))
			  (if (null next-right)
			      (error 'finite-map-key-not-found)
			      (setf branch next-right)))))))))

       (,constructor
	:bst (make-bst :element-type ,record-struct
		       :test #'(lambda (a b) (funcall ,test
						 (slot-value a 'key)
						 (slot-value b 'key)))
		       :unique-only t
		       :overwrites ,overwrites)))))
