;;;; BST.LISP - Binary Search Tree template macro
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
(defpackage :bst
  (:use :common-lisp)
  (:export :make-bst
	   :value
	   :left
	   :right
	   :bst-insert
	   :bst-member
	   :bst-empty
	   :bst-to-list))
(in-package :bst)

(defgeneric bst-insert (x tr))
(defgeneric bst-member (x tr))
(defgeneric bst-empty (tr))
(defgeneric bst-to-list (tr))

(defmacro make-bst (&key (element-type 'real) (test #'<) unique-only overwrites)
  (let* ((id (string (gensym "")))
	 (type (intern (concatenate 'string "BST-NODE-" id)))
	 (elem-type (intern (concatenate 'string "BST-ELEM-" id)))
	 (struct (intern (concatenate 'string "BST-" id)))
	 (constructor (intern (concatenate 'string "MAKE-BST-" id))))
    `(progn
       (deftype ,type () '(or null ,struct))
       (deftype ,elem-type () '(or null ,element-type))
       (defstruct ,struct
	 (left nil :type ,type)
	 (value nil :type ,elem-type)
	 (right nil :type ,type))
       
       (defmethod bst-insert ((x ,element-type) (tr ,struct))
	 (with-slots ((l left) (v value) (r right)) tr
	   (cond ((null v) (,constructor :value x))
		 ((funcall ,test v x)
		  (,constructor :left l
				:value v
				:right (if (null r)
					   (,constructor :value x)
					   (bst-insert x r))))
		 ((or (not ,unique-only)
		      (funcall ,test x v))
		  (,constructor :left (if (null l)
					  (,constructor
					   :value x)
					  (bst-insert x l))
				:value v
				:right r))
		 (t (if ,overwrites
			(,constructor :left l :value x :right r)
			tr)))))
       
       (defmethod bst-member ((x ,element-type) (tr ,struct))
	 (with-slots ((l left) (v value) (r right)) tr
	   (cond ((null v) nil)
		 ((funcall ,test x v) (when (not (null l)) (bst-member x l)))
		 ((funcall ,test v x) (when (not (null r)) (bst-member x r)))
		 (t tr))))
       
       (defmethod bst-empty ((tr ,struct))
	 (with-slots ((l left) (v value) (r right)) tr
	   (and (null v)
		(or (null l) (bst-empty l))
		(or (null r) (bst-empty r)))))
       
       (defmethod bst-to-list ((tr ,struct))
	 (with-slots ((l left) (v value) (r right)) tr
	   (concatenate 'list
			(when (not (null l)) (bst-to-list l))
			(list v)
			(when (not (null r)) (bst-to-list r)))))
       
       (,constructor))))
