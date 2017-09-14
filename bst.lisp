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

;;; MAKE-BST template macro:
;; Create a typed binary search tree using a custom template tree type.
;;
;;  Keyword parameters:
;;    ELEMENT-TYPE: Unquoted type specifier (as used in method parameter types)
;;    TEST: Function by which to compare values left-to-right when ordering
;;    UNIQUE-ONLY: If true, disallow multiple nodes with the same value
;;    OVERWRITES: If true and UNIQUE-ONLY, new insertions overwrite matches
;;
;;  Returns:
;;    New instance of a custom BST type created from the template.
;;
;;  Example:
;;   > (progn 
;;       (defparameter *t* (make-bst :element-type string :test #'string<)) 
;;       *t*)
;;   #S(BST-1173 :LEFT NIL :VALUE NIL :RIGHT NIL)

(defgeneric bst-insert (x tr))
;; Nondestructive insert of value X into binary search tree TR.
;;
;;  Parameters:
;;    X: The element (of type ELEMENT-TYPE) to be inserted to the BST
;;    TR: The binary search tree into which X will be inserted
;;
;;  Returns:
;;    A BST created from TR with a [possibly additional] node representing X.
;;
;;  Example:
;;   > (setf *t* (bst-insert "world" *t*))
;;   #S(BST-1173 :LEFT NIL :VALUE "world" :RIGHT NIL)
;;   > (setf *t* (bst-insert "hello" *t*))
;;   #S(BST-1173
;;      :LEFT #S(BST-1173 :LEFT NIL :VALUE "hello" :RIGHT NIL)
;;      :VALUE "world"
;;      :RIGHT NIL)

(defgeneric bst-member (x tr))
;; If found, retrieve the subtree of binary search tree TR containing element X.
;;
;;  Parameters:
;;    X: The element (of type ELEMENT-TYPE) for which to search
;;    TR: The binary search tree in which X is to be sought
;;
;;  Returns:
;;    BST with the root node containing a match of X, or NIL if not found.
;;
;;  Example:
;;   > (bst-member "foo" *t*)
;;   NIL
;;   > (bst-member "hello" *t*)
;;   #S(BST-1173 :LEFT NIL :VALUE "hello" :RIGHT NIL)
;;   > (bst-member "world" *t*)
;;   #S(BST-1173
;;      :LEFT #S(BST-1173 :LEFT NIL :VALUE "hello" :RIGHT NIL)
;;      :VALUE "world"
;;      :RIGHT NIL)

(defgeneric bst-empty (tr))
;; Return whether the given binary search tree TR is empty.
;;
;;  Parameters:
;;    TR: The binary search tree to be checked for contents.
;;
;;  Returns:
;;    T if TR is empty [any/all nodes contain NIL], otherwise NIL.
;;
;;  Example:
;;   > (bst-empty *t*)
;;   NIL
;;   > (bst-empty (make-bst))
;;   T

(defgeneric bst-to-list (tr))
;; Convert the binary search tree TR into an ordered list.
;;
;;  Parameters:
;;    TR: Binary search tree to be converted into a list
;;
;;  Returns:
;;    A list composed of the elements of TR ordered from left-to-right.
;;
;;  Example:
;;   > (bst-to-list *t*)
;;   ("hello" "world")

(defmacro make-bst (&key (element-type 'real) (test #'<) unique-only overwrites)
  "Create a typed binary search tree using a custom template tree type."
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
	 "Nondestructive insert of value X into binary search tree TR."
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
	 "If found, retrieve the subtree of TR containing element X."
	 (with-slots ((l left) (v value) (r right)) tr
	   (cond ((null v) nil)
		 ((funcall ,test x v) (when (not (null l)) (bst-member x l)))
		 ((funcall ,test v x) (when (not (null r)) (bst-member x r)))
		 (t tr))))
       
       (defmethod bst-empty ((tr ,struct))
	 "Return whether the given binary search tree TR is empty."
	 (with-slots ((l left) (v value) (r right)) tr
	   (and (null v)
		(or (null l) (bst-empty l))
		(or (null r) (bst-empty r)))))
       
       (defmethod bst-to-list ((tr ,struct))
	 "Convert the binary search tree TR into an ordered list."
	 (with-slots ((l left) (v value) (r right)) tr
	   (concatenate 'list
			(when (not (null l)) (bst-to-list l))
			(list v)
			(when (not (null r)) (bst-to-list r)))))
       
       (,constructor))))
